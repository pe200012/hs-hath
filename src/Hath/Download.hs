-- | Downloads from other servers: reading a reply's body with a limit on the
-- wait for every read, and whole downloads as the official FileDownloader makes
-- them (proxy tests, gallery files).
module Hath.Download ( fetchFile, receive ) where

import qualified Data.ByteString.Char8   as BS
import qualified Data.List               as List
import           Data.String.Interpolate ( i )

import           Hath.Protocol           ( outgoingHeaders )
import           Hath.RPC                ( connectMicros, httpManager, worthRetrying )

import qualified Network.HTTP.Client     as HC
import           Network.HTTP.Types      ( hContentLength, statusCode )

import           Relude

import           UnliftIO                ( timeout, tryAny )

-- | Download as the official FileDownloader does (FileDownloader.java:147-299):
-- up to three attempts, one right after another, each with 5 s to connect and
-- 10 s for the reply's headers and for every read. An attempt succeeds if its
-- reply has a status below 400 and announces its length, at most
-- @max_allowed_filesize@, and that many bytes arrive. @keep@ gets them chunk by
-- chunk with the offset of each, so a new attempt starts again at 0. A 404 or
-- 410 is not tried again. Right is the length, Left why the last attempt failed.
fetchFile :: Int -> HC.Request -> (Int -> ByteString -> IO ()) -> IO (Either Text Int)
fetchFile maxAllowedFileSize request keep = go (3 :: Int)
  where
    go attemptsLeft = attempt >>= \case
      Left ( status, why )
        | attemptsLeft > 1, worthRetrying status -> go (attemptsLeft - 1)
        | otherwise -> pure (Left why)
      Right size           -> pure (Right size)

    -- the length, or else the status that failed it (none if something else did) and why
    attempt
      = either (\e -> Left ( Nothing, describe e )) id
      <$> tryAny (HC.withResponse sent httpManager answer)

    answer response = case ( statusCode (HC.responseStatus response), announced ) of
      ( status, _ )
        | status >= 400 -> pure $ Left ( Just status, [i|HTTP #{status}|] )
      ( _, Nothing )   -> pure $ Left ( Nothing, "no Content-Length" )
      ( _, Just size )
        | size > maxAllowedFileSize
          -> pure $ Left ( Nothing, [i|#{size} bytes, more than max_allowed_filesize|] )
        | otherwise -> receive readMicros size (HC.responseBody response) keep <&> \case
          Left why -> Left ( Nothing, why )
          Right () -> Right size
      where
        announced
          = readMaybe . BS.unpack =<< List.lookup hContentLength (HC.responseHeaders response)

    sent
      = request { HC.requestHeaders  = outgoingHeaders [ ( "Connection", "Close" ) ]
                , HC.responseTimeout = HC.responseTimeoutMicro (connectMicros + readMicros)
                }

    readMicros = 10 * 1000000

-- | Read a body of @size@ bytes, handing each chunk to @keep@ with the offset it
-- starts at, and waiting up to @readMicros@ for every read. Left says why it
-- broke off: upstream failed or sent too little or too much, or a read waited
-- too long.
receive :: Int -> Int -> HC.BodyReader -> (Int -> ByteString -> IO ()) -> IO (Either Text ())
receive readMicros size body keep = either (Left . describe) id <$> tryAny (go 0)
  where
    go received = timeout readMicros (HC.brRead body) >>= \case
      Nothing
        -> pure $ Left [i|nothing came for #{readMicros `div` 1000000} s after #{received} bytes|]
      Just chunk
        | BS.null chunk -> pure
          $ if received == size
            then Right ()
            else Left [i|it ended after #{received} of #{size} bytes|]
        -- the caller's buffer may hold no more; a chunked body can bring more than announced
        | BS.length chunk > size - received -> pure $ Left [i|it sent more than #{size} bytes|]
        | otherwise -> keep received chunk >> go (received + BS.length chunk)

-- | Why a download failed, in http-client's words where it has them.
describe :: SomeException -> Text
describe e = case fromException e of
  Just (HC.HttpExceptionRequest _ content) -> show content
  _ -> show e
