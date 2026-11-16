-- | The wire formats and signatures of the Hentai\@Home protocol (official client
-- 1.6.6), as pure functions.
module Hath.Protocol
  ( -- * Identity
    clientBuild
  , hathHeaders
    -- * Hashing
  , sha1Hex
  , sha1HexText
    -- * Request signatures
  , rpcActionKey
  , serverCommandKey
  , speedTestKey
  , keystampHash
  , parseKeystamp
  , hathRequestToken
    -- * Allowed clock difference
  , maxKeystampDrift
  , maxCommandDrift
    -- * RPC replies
  , verifyRPCResponse
  , rpcPayload
    -- * Parameters
  , parseURLParams
  ) where

import           Crypto.Hash             ( SHA1 )
import qualified Crypto.Hash             as Crypto

import           Data.ByteArray.Encoding ( Base(Base16), convertToBase )
import qualified Data.ByteString.Char8   as BS
import qualified Data.Map.Strict         as Map

import           Hath.Config             ( ClientConfig(..) )
import           Hath.Error              ( HathError(..) )
import           Hath.FileURI            ( FileURI, renderFileURIBytes )

import           Network.HTTP.Types      ( HeaderName )

import           Relude

-- | The protocol level this client implements (official 1.6.6), sent as @clientbuild@.
clientBuild :: Text
clientBuild = "181"

-- | Sent with every response, and with our own requests as well.
hathHeaders :: [ ( HeaderName, ByteString ) ]
hathHeaders
  = [ ( "Connection", "close" )
    , ( "User-Agent", "Hentai@Home 1.6.6" )
    , ( "Cache-Control", "public, max-age=31536000" )
    , ( "Server", "Genetic Lifeform and Distributed Open Server 1.6.6" )
    , ( "X-Content-Type-Options", "nosniff" )
    ]

-- | SHA-1 in lower-case hex, the form every H\@H signature and file id uses.
sha1Hex :: ByteString -> ByteString
sha1Hex = convertToBase Base16 . Crypto.hash @ByteString @SHA1

sha1HexText :: ByteString -> Text
sha1HexText = decodeUtf8 . sha1Hex

-- | Signatures are the SHA-1 of their parts joined by dashes.
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

-- | Split a keystamp @\<time>-\<hash>@. A time that is not a number reads as 0,
-- which no clock is close to.
parseKeystamp :: ByteString -> ( Int64, ByteString )
parseKeystamp stamp = ( maybe 0 (fromIntegral . fst) (BS.readInt time), BS.drop 1 rest )
  where
    ( time, rest ) = BS.span (/= '-') stamp

-- | The @Hath-Request@ header that authenticates us to an image server.
hathRequestToken :: ClientConfig -> FileURI -> ByteString
hathRequestToken cfg uri
  = encodeUtf8 (clientId cfg) <> "-" <> sha1Hex (encodeUtf8 (key cfg) <> renderFileURIBytes uri)

-- | How far the time in a keystamp or a speed test may be from our clock, in seconds.
maxKeystampDrift :: Int64
maxKeystampDrift = 600

-- | The same for @servercmd@ (official @Settings.MAX_KEY_TIME_DRIFT@).
maxCommandDrift :: Int64
maxCommandDrift = 300

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

-- | Parse @key1=value1;key2=value2;flag@. A key without a value maps to the
-- empty string, and a value may itself contain @=@.
parseURLParams :: ByteString -> Map ByteString ByteString
parseURLParams = Map.fromList . map keyValue . filter (not . BS.null) . BS.split ';'
  where
    keyValue = second (BS.drop 1) . BS.break (== '=')
