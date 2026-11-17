-- | The wire formats and signatures of the Hentai\@Home protocol (official client
-- 1.6.6), as pure functions.
module Hath.Protocol
  ( -- * Identity
    clientBuild
  , responseHeaders
  , outgoingHeaders
    -- * Hashing
  , sha1Hex
  , sha1HexText
    -- * Request signatures
  , rpcActionKey
  , rpcQuery
  , serverCommandKey
  , speedTestKey
  , keystampHash
  , hathRequestToken
    -- * Checking requests
  , keystampValid
  , commandTimeValid
  , fetchParams
    -- * RPC replies
  , verifyRPCResponse
  , rpcPayload
  , retryWithServerTime
    -- * Parameters
  , parseURLParams
  ) where

import           Crypto.Hash             ( SHA1 )
import qualified Crypto.Hash             as Crypto

import           Data.ByteArray.Encoding ( Base(Base16), convertToBase )
import qualified Data.ByteString.Char8   as BS
import           Data.Char               ( isDigit, toLower )
import qualified Data.Map.Strict         as Map

import           Hath.Config             ( ClientConfig(..) )
import           Hath.Error              ( HathError(..) )

import           Network.HTTP.Types      ( Header, urlEncode )

import           Relude

-- | The protocol level this client implements (official 1.6.6), sent as @clientbuild@.
clientBuild :: Int64
clientBuild = 181

userAgent :: Header
userAgent = ( "User-Agent", "Hentai@Home 1.6.6" )

-- | The headers of a response whose body has this Content-Type and length, as
-- the official client writes them (HTTPSession.java:129-139); Warp adds the
-- Date. The official client leaves the length out of an empty response and
-- closes the connection; Warp would send that body chunked, so we say it is empty.
responseHeaders :: ByteString -> Int -> [ Header ]
responseHeaders contentType len
  = [ ( "Server", "Genetic Lifeform and Distributed Open Server 1.6.6" )
    , ( "Connection", "close" )
    , ( "Content-Type", contentType )
    , ( "Content-Length", show len )
    ]
  <> [ ( "Cache-Control", "public, max-age=31536000" ) | len > 0 ]

-- | The headers of a request of ours: the given ones and the official client's
-- User-Agent. The empty Accept-Encoding keeps http-client from adding
-- @gzip@, which the official client never asks for.
outgoingHeaders :: [ Header ] -> [ Header ]
outgoingHeaders headers = headers <> [ userAgent, ( "Accept-Encoding", "" ) ]

-- | SHA-1 in lower-case hex, the form every H\@H signature and file id uses.
sha1Hex :: ByteString -> ByteString
sha1Hex = convertToBase Base16 . Crypto.hash @ByteString @SHA1

sha1HexText :: ByteString -> Text
sha1HexText = decodeUtf8 . sha1Hex

signature :: [ ByteString ] -> ByteString
signature = sha1Hex . BS.intercalate "-"

-- | @actkey@ of an RPC request, for its @act@, @add@ and @acttime@.
rpcActionKey :: ClientConfig -> Text -> Text -> Int64 -> Text
rpcActionKey cfg act add time
  = decodeUtf8
  $ signature
    [ "hentai@home"
    , encodeUtf8 act
    , encodeUtf8 add
    , encodeUtf8 (clientId cfg)
    , show time
    , encodeUtf8 (key cfg)
    ]

-- | The query of a request to the RPC server, in the order the official client
-- writes it (ServerHandler.getURLQueryString), @add@ included even when empty.
-- The official client does not escape the values. We do, except the @;@
-- between the parts of @add@: a legitimate request goes out byte for byte the
-- same, and what a reader puts in @fileindex@ or @xres@ cannot add parameters.
rpcQuery :: ClientConfig -> Text -> Text -> Int64 -> ByteString
rpcQuery cfg act add time
  = BS.intercalate
    "&"
    [ name <> "=" <> escape value
    | ( name, value )
    <- [ ( "clientbuild", show clientBuild )
       , ( "act", encodeUtf8 act )
       , ( "add", encodeUtf8 add )
       , ( "cid", encodeUtf8 (clientId cfg) )
       , ( "acttime", show time )
       , ( "actkey", encodeUtf8 (rpcActionKey cfg act add time) )
       ]
    ]
  where
    escape = BS.intercalate ";" . map (urlEncode True) . BS.split ';'

-- | The key the RPC server puts on a @servercmd@ request. Only it knows our key.
serverCommandKey :: ClientConfig -> Text -> Text -> Int -> Text
serverCommandKey cfg command additional time
  = decodeUtf8
  $ signature
    [ "hentai@home-servercmd"
    , encodeUtf8 command
    , encodeUtf8 additional
    , encodeUtf8 (clientId cfg)
    , show time
    , encodeUtf8 (key cfg)
    ]

-- | @testkey@ of a @\/t\/@ speed test of the given size and time.
speedTestKey :: ClientConfig -> Int -> Int64 -> ByteString
speedTestKey cfg size time
  = signature
    [ "hentai@home-speedtest"
    , show size
    , show time
    , encodeUtf8 (clientId cfg)
    , encodeUtf8 (key cfg)
    ]

-- | The second half of the keystamp that lets a reader fetch a file id at a given time.
keystampHash :: ClientConfig -> Int64 -> ByteString -> ByteString
keystampHash cfg time fileId
  = BS.take 10 $ signature [ show time, fileId, encodeUtf8 (key cfg), "hotlinkthis" ]

-- | The @Hath-Request@ header that authenticates us to an image server, for a
-- file id as the reader asked for it.
hathRequestToken :: ClientConfig -> ByteString -> ByteString
hathRequestToken cfg fileId
  = encodeUtf8 (clientId cfg) <> "-" <> sha1Hex (encodeUtf8 (key cfg) <> fileId)

-- | Whether a keystamp, @\<time>-\<hash>@, entitles a reader to a file id at
-- the server's time @now@ (HTTPResponse.java:174-186): issued less than 15
-- minutes from it either way, with the hash in either case.
keystampValid :: ClientConfig -> Int64 -> ByteString -> ByteString -> Bool
keystampValid cfg now fileId stamp = case BS.split '-' stamp of
  [ time, hash ]
    | Just ( issued, "" ) <- BS.readInt time -> abs (now - fromIntegral issued) < 900
      && BS.map toLower hash == keystampHash cfg (fromIntegral issued) fileId
  _ -> False

-- | Whether the time of a server command or a speed test is close enough to
-- the server's @now@ (Settings.MAX_KEY_TIME_DRIFT).
commandTimeValid :: Int64 -> Int64 -> Bool
commandTimeValid now time = abs (now - time) <= 300

-- | The @fileindex@ and @xres@ of an @\/h\/@ request, which a file missing from
-- the cache is fetched by, if they have the official form: a number, and a
-- number or @org@ (HTTPResponse.java:194).
fetchParams :: Map ByteString ByteString -> Maybe ( ByteString, ByteString )
fetchParams options = do
  fileIndex <- Map.lookup "fileindex" options
  xres <- Map.lookup "xres" options
  guard (number fileIndex && (xres == "org" || number xres))
  pure ( fileIndex, xres )
  where
    number s = not (BS.null s) && BS.all isDigit s

-- | Check the 1.6.6 RPC signature. A signed reply starts with @OK-\<time>-\<sha1>@, the
-- sha1 covering @\<time>-\<client key>@ then @"\\n" ++ line@ for every later line. A
-- valid reply comes back with that first line reduced to @OK@; a forged or unsigned
-- OK becomes @RESPONSE_HASH_FAILED@; any other status (FAIL, KEY_EXPIRED, ...) passes
-- through.
verifyRPCResponse :: ByteString -> ByteString -> ByteString
verifyRPCResponse clientKey bytes = case BS.split '\n' bytes of
  status : rest
    | Just ( time, sig ) <- BS.break (== '-') <$> BS.stripPrefix "OK-" status
    , sig == "-" <> sha1Hex (time <> "-" <> clientKey <> foldMap ("\n" <>) body)
      -> BS.intercalate "\n" ("OK" : body)
    | "OK" `BS.isPrefixOf` status -> "RESPONSE_HASH_FAILED"
    where
      -- Java's split drops trailing empty lines, and the hash is over what it keeps
      body = reverse $ dropWhile BS.null $ reverse rest
  _ -> bytes

-- | The lines after an @OK@ status line; any other status is the error.
rpcPayload :: ByteString -> Either HathError [ ByteString ]
rpcPayload reply = case BS.lines reply of
  [] -> Left EmptyResponse
  "OK" : payload -> Right payload
  status : _ -> Left $ RequestFailure $ decodeUtf8 status

-- | Whether to take the server's time and send the request again. On
-- @KEY_EXPIRED@ the official client refreshes server_stat and retries once,
-- but only a request without @add@ (ServerResponse.java:103-107).
retryWithServerTime :: Text -> ByteString -> Bool
retryWithServerTime add reply = add == "" && BS.takeWhile (/= '\n') reply == "KEY_EXPIRED"

-- | Parse @key1=value1;key2=value2;flag@. A key without a value maps to the
-- empty string, and a value may itself contain @=@.
parseURLParams :: ByteString -> Map ByteString ByteString
parseURLParams = Map.fromList . map keyValue . filter (not . BS.null) . BS.split ';'
  where
    keyValue = second (BS.drop 1) . BS.break (== '=')
