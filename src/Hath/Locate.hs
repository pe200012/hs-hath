{-# LANGUAGE DataKinds #-}

-- | Finding a requested file: in the cache, or else upstream.
module Hath.Locate ( Located(..), locateResource, usableReply ) where

import           Colog                    ( Message )
import           Colog.Polysemy           ( Log )

import           Control.Concurrent       ( forkIO )
import           Control.Exception        ( finally )

import qualified Data.ByteString.Char8    as BS
import           Data.ByteString.Internal ( fromForeignPtr, fromForeignPtr0, mallocByteString )
import           Data.ByteString.Unsafe   ( unsafeUseAsCStringLen )
import qualified Data.List                as List
import           Data.String.Interpolate  ( i )

import           Foreign.ForeignPtr       ( ForeignPtr, withForeignPtr )
import           Foreign.Marshal.Utils    ( copyBytes )
import           Foreign.Ptr              ( castPtr, plusPtr )

import           Hath.Cache               ( Cache, CacheHit, lookupFile, storeFile )
import           Hath.Config              ( ClientConfig )
import           Hath.Download            ( receive )
import           Hath.Error               ( HathError )
import           Hath.FileURI             ( FileURI(..) )
import           Hath.Log                 ( logInfo, logWarning )
import           Hath.Protocol            ( hathRequestToken, outgoingHeaders, sha1Hex )
import           Hath.RPC                 ( RPC, connectMicros, fileSources, httpManager )
import           Hath.Stats               ( StatsEnv, addDownload, incFetched )

import qualified Network.HTTP.Client      as HC
import           Network.HTTP.Types       ( Header
                                          , Status
                                          , hContentLength
                                          , status404
                                          , status500
                                          , status502
                                          , statusCode
                                          )

import           Polysemy
import           Polysemy.Error           ( Error, catch )
import           Polysemy.Final           ( withWeavingToFinal )
import           Polysemy.Reader          ( Reader, ask )

import           Relude                   hiding ( Reader, ask )

import           System.IO.Error          ( ioError, mkIOError, resourceVanishedErrorType )

import           UnliftIO                 ( retrySTM, timeout, try )

data Located
  = Stored !CacheHit
    -- | A cache miss, coming in from upstream on a thread of its own, which
    -- caches the file once it is complete. Call it with a chunk writer to
    -- relay the file as it arrives; it throws if the download breaks off.
  | Streamed !((ByteString -> IO ()) -> IO ())

-- | Look in the cache; on a miss, ask the RPC server where the file is, by the
-- @fileindex@ and @xres@ of the request and the file id as the reader asked for
-- it, and start fetching it. Left is the status the reader gets when that
-- fails (see 'openUpstream').
--
-- Like the official client, we serve any file the server sends readers here
-- for; static ranges only matter for pruning the cache.
locateResource
  :: Members
    '[ Cache
     , RPC
     , Reader StatsEnv
     , Log Message
     , Reader ClientConfig
     , Error HathError
     , Embed IO
     , Final IO
     ]
    r
  => Int
  -> ByteString
  -> FileURI
  -> ( ByteString, ByteString )
  -> Sem r (Either Status Located)
locateResource maxAllowedFileSize fileId uri ( fileIndex, xres ) = lookupFile uri >>= \case
  Just hit -> pure $ Right $ Stored hit
  Nothing  -> do
    logInfo [i|Fetching resource: #{uri}|]
    openUpstream maxAllowedFileSize fileId uri fileIndex xres >>= \case
      Left status    -> do
        logWarning [i|Could not fetch #{uri}: #{statusCode status}|]
        pure (Left status)
      Right response -> do
        incFetched
        -- runs on the download's thread, after the handler has returned
        finish <- deferToIO $ \case
          Left why      -> logWarning [i|Fetching #{uri} failed: #{why}|]
          Right content -> do
            addDownload (BS.length content)
            -- a failed cache write only costs us the next hit
            catch @HathError (storeFile uri content)
              $ \e -> logWarning [i|Cache write failed for #{uri}: #{e}|]
        Right . Streamed <$> embed (download uri response finish)

-- | Open the first source that answers with the file, as the official
-- ProxyFileDownloader does; the caller reads the body and must close the
-- response. Without sources the reader gets a 404. If no source will do, a 502
-- once one of them has answered, or a 500 if none could be reached: the
-- official client changes its status only for a source it connected to
-- (ProxyFileDownloader.java:71-111).
openUpstream :: Members '[ RPC, Reader ClientConfig, Log Message, Error HathError, Embed IO ] r
             => Int
             -> ByteString
             -> FileURI
             -> ByteString
             -> ByteString
             -> Sem r (Either Status (HC.Response HC.BodyReader))
openUpstream maxAllowedFileSize fileId uri fileIndex xres = do
  urls <- filter (not . BS.null) <$> fileSources fileIndex xres fileId `catch` \(e :: HathError)
    -> [] <$ logWarning [i|srfetch failed for #{uri}: #{e}|]
  cfg <- ask @ClientConfig
  let request url = do
        req <- HC.parseRequest (BS.unpack url)
        pure
          req { HC.requestHeaders
                  = outgoingHeaders [ ( "Hath-Request", hathRequestToken cfg fileId ) ]
                -- 5 s to connect, then 30 s for the headers (ProxyFileDownloader.java:86-87)
              , HC.responseTimeout = HC.responseTimeoutMicro (connectMicros + seconds 30)
              }
      open url = case request url of
        Nothing  -> pure Unreachable
        Just req -> try @IO @HC.HttpException (HC.responseOpen req httpManager) >>= \case
          Right response
            | usableReply
                maxAllowedFileSize
                uri
                (statusCode (HC.responseStatus response))
                (HC.responseHeaders response) -> pure (Opened response)
            | otherwise -> HC.responseClose response $> Answered
          Left (HC.HttpExceptionRequest _ HC.ConnectionTimeout) -> pure Unreachable
          Left (HC.HttpExceptionRequest _ (HC.ConnectionFailure _)) -> pure Unreachable
          Left _ -> pure Answered
      firstOpened failure = \case
        []         -> pure (Left failure)
        url : rest -> open url >>= \case
          Opened response -> pure (Right response)
          Answered        -> firstOpened status502 rest
          Unreachable     -> firstOpened failure rest
  if null urls
    then pure (Left status404)
    else embed (firstOpened status500 urls)

data Attempt
  = Opened !(HC.Response HC.BodyReader)
  | Answered
    -- ^ it answered, but not with the file
  | Unreachable

-- | Whether an upstream reply, given its status and headers, is the file
-- (ProxyFileDownloader.java:92-107): it must announce the file's length, and a
-- file larger than @max_allowed_filesize@ is not taken.
usableReply :: Int -> FileURI -> Int -> [ Header ] -> Bool
usableReply maxAllowedFileSize uri status headers
  = status == 200 && announced == Just (fileSize uri) && fileSize uri <= maxAllowedFileSize
  where
    announced = readMaybe . BS.unpack =<< List.lookup hContentLength headers

-- | Fetch a body on a thread of its own, into a buffer of the file's size. The
-- reader relays from the buffer, and the buffer, once verified, is what the
-- cache keeps, so the file is in memory once. A reader that goes away leaves
-- the download running, as the official ProxyFileDownloader's thread does.
-- @finish@ gets the verified file, or why there is none.
download :: FileURI
         -> HC.Response HC.BodyReader
         -> (Either Text ByteString -> IO ())
         -> IO ((ByteString -> IO ()) -> IO ())
download uri response finish = do
  buffer <- mallocByteString size
  progress <- newTVarIO (Receiving 0)
  let keep offset chunk = do
        copyInto buffer offset chunk
        atomically $ writeTVar progress (Receiving (offset + BS.length chunk))
  void $ forkIO $ do
    -- 30 s for every read and 300 s for the whole (ProxyFileDownloader.java:186-200)
    received <- fromMaybe (Left "it took over 300 s")
      <$> timeout (seconds 300) (receive (seconds 30) size (HC.responseBody response) keep)
      `finally` HC.responseClose response
    atomically $ writeTVar progress (either (const Broken) (const Received) received)
    finish $ received >> verified (fromForeignPtr0 buffer size)
  pure (relay buffer progress)
  where
    size = fileSize uri

    verified content
      | sha1Hex content == fileHash uri = Right content
      | otherwise = Left "its SHA-1 does not match the file id"

    relay buffer progress write = go 0
      where
        go sent = atomically (beyond sent) >>= \case
          Receiving filled -> write (fromForeignPtr buffer sent (filled - sent)) >> go filled
          Received         -> when (sent < size) $ write (fromForeignPtr buffer sent (size - sent))
          -- the kind of error a peer that went away raises: Warp drops the
          -- reader's connection, as it must for a body shorter than announced,
          -- and does not print it (the download logs why)
          Broken           -> ioError
            (mkIOError resourceVanishedErrorType "upstream broke off" Nothing Nothing)

        beyond sent = readTVar progress >>= \case
          Receiving filled
            | filled <= sent -> retrySTM
          settled          -> pure settled

data Progress = Receiving !Int | Received | Broken

copyInto :: ForeignPtr Word8 -> Int -> ByteString -> IO ()
copyInto buffer offset chunk = unsafeUseAsCStringLen chunk $ \( source, len )
  -> withForeignPtr buffer $ \start -> copyBytes (start `plusPtr` offset) (castPtr source) len

-- | In microseconds.
seconds :: Int -> Int
seconds = (* 1000000)

-- | Lower a Sem action into IO so it can run after the request handler has returned.
deferToIO :: Member (Final IO) r => (x -> Sem r ()) -> Sem r (x -> IO ())
deferToIO k = withWeavingToFinal $ \s lower _ -> pure ((\x -> void (lower (k x <$ s))) <$ s)
