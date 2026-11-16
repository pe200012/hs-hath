module ProtocolSpec ( protocolSpecs ) where

import qualified Data.ByteString.Char8 as BS
import           Data.Char             ( toUpper )
import qualified Data.Map.Strict       as Map

import           Hath.Config           ( ClientConfig(..), defaultClientConfig )
import           Hath.Error            ( HathError(..) )
import           Hath.Protocol

import           Relude

import           Test.Hspec
import           Test.Hspec.QuickCheck ( prop )
import           Test.QuickCheck

-- | Text free of the separators these formats use.
word :: Gen ByteString
word = BS.pack <$> listOf1 (elements ([ 'a' .. 'z' ] <> [ '0' .. '9' ] <> " ._/"))

-- | A client key, the time of a reply and its payload lines.
reply :: Gen ( ByteString, ByteString, [ ByteString ] )
reply = (,,) <$> word <*> (BS.pack <$> listOf1 (elements [ '0' .. '9' ])) <*> listOf word

-- | A reply as the 1.6.6 server signs it.
signed :: ByteString -> ByteString -> [ ByteString ] -> ByteString
signed clientKey time payload
  = "OK-" <> time <> "-" <> sha1Hex (time <> "-" <> clientKey <> body) <> body
  where
    body = foldMap ("\n" <>) payload

protocolSpecs :: Spec
protocolSpecs = describe "Protocol" $ do
  describe "parseURLParams" $ do
    it "reads pairs, flags, empty values and values containing ="
      $ parseURLParams "a=1;flag;b=x=y;;c="
      `shouldBe` Map.fromList [ ( "a", "1" ), ( "flag", "" ), ( "b", "x=y" ), ( "c", "" ) ]

    prop "reads back every pair, the last of a key winning"
      $ forAll (listOf ((,) <$> word <*> word))
      $ \pairs -> parseURLParams (BS.intercalate ";" [ k <> "=" <> v | ( k, v ) <- pairs ])
      === Map.fromList pairs

  describe "rpcPayload" $ do
    it "is the lines after OK" $ rpcPayload "OK\na=1\nb=2\n" `shouldBe` Right [ "a=1", "b=2" ]

    it "is the status when that is not OK"
      $ rpcPayload "KEY_EXPIRED\nx" `shouldBe` Left (RequestFailure "KEY_EXPIRED")

    it "is an error for an empty reply" $ rpcPayload "" `shouldBe` Left EmptyResponse

  describe "retryWithServerTime" $ do
    it "is for KEY_EXPIRED on a request without add" $ do
      retryWithServerTime "" "KEY_EXPIRED" `shouldBe` True
      retryWithServerTime "" "KEY_EXPIRED\n" `shouldBe` True

    it "is not for a request with add, nor for another status" $ do
      retryWithServerTime "60" "KEY_EXPIRED" `shouldBe` False
      retryWithServerTime "" "FAIL" `shouldBe` False
      retryWithServerTime "" "KEY_EXPIRED_SOON" `shouldBe` False

  describe "verifyRPCResponse" $ do
    it "drops the empty line a reply ends with"
      $ verifyRPCResponse "k" (signed "k" "7" [ "a=1", "b=2" ] <> "\n") `shouldBe` "OK\na=1\nb=2"

    it "refuses an OK that is not signed"
      $ verifyRPCResponse "k" "OK\na=1" `shouldBe` "RESPONSE_HASH_FAILED"

    it "passes any other status through"
      $ verifyRPCResponse "k" "KEY_EXPIRED" `shouldBe` "KEY_EXPIRED"

    prop "accepts a reply signed with the client key"
      $ forAll reply
      $ \( clientKey, time, payload )
      -> verifyRPCResponse clientKey (signed clientKey time payload)
      === BS.intercalate "\n" ("OK" : payload)

    prop "refuses it once a line is added"
      $ forAll ((,) <$> reply <*> word)
      $ \( ( clientKey, time, payload ), extra )
      -> verifyRPCResponse clientKey (signed clientKey time payload <> "\n" <> extra)
      === "RESPONSE_HASH_FAILED"

    prop "refuses a reply signed with another key" $ forAll reply $ \( clientKey, time, payload )
      -> verifyRPCResponse (clientKey <> "x") (signed clientKey time payload)
      === "RESPONSE_HASH_FAILED"

  describe "signatures" $ do
    let cfg = defaultClientConfig { clientId = "12345", key = "secretkey" }

    it "signs an RPC request"
      $ rpcActionKey cfg "srfetch" "1;780;abc" 1700000000
      `shouldBe` sha1HexText "hentai@home-srfetch-1;780;abc-12345-1700000000-secretkey"

    it "knows the key of a server command"
      $ serverCommandKey cfg "speed_test" "testsize=1000" 1700000000
      `shouldBe` sha1HexText
        "hentai@home-servercmd-speed_test-testsize=1000-12345-1700000000-secretkey"

    it "knows the key of a speed test"
      $ speedTestKey cfg 1000 1700000000
      `shouldBe` sha1Hex "hentai@home-speedtest-1000-1700000000-12345-secretkey"

    it "knows the keystamp of a file"
      $ keystampHash cfg 1700000000 "abc-1-2-3-jpg"
      `shouldBe` BS.take 10 (sha1Hex "1700000000-abc-1-2-3-jpg-secretkey-hotlinkthis")

    it "writes an RPC query in the official order, with add= even when empty"
      $ rpcQuery cfg "server_stat" "" 1700000000
      `shouldBe` "clientbuild=181&act=server_stat&add=&cid=12345&acttime=1700000000&actkey="
      <> encodeUtf8 (rpcActionKey cfg "server_stat" "" 1700000000)

    it "keeps the ; between the parts of add and escapes the rest"
      $ rpcQuery cfg "srfetch" "1;org;x&act=y" 1700000000
      `shouldSatisfy` ("&add=1;org;x%26act%3Dy&" `BS.isInfixOf`)

    it "identifies itself to an image server with the file id as it was asked for"
      $ hathRequestToken cfg "abc-01-2-3-jpg" `shouldBe` "12345-"
      <> sha1Hex "secretkeyabc-01-2-3-jpg"

  describe "keystampValid" $ do
    let cfg     = defaultClientConfig { key = "secretkey" }
        fileId  = "abc-1-2-3-jpg"
        issued  = 1700000000
        stamp t = show t <> "-" <> keystampHash cfg t fileId

    it "accepts a keystamp issued less than 900 s from the server's time, either way"
      $ forM_ [ -899, 0, 899 ]
      $ \drift -> keystampValid cfg (issued + drift) fileId (stamp issued) `shouldBe` True

    it "refuses one 900 s or more away" $ forM_ [ -900, 900, 5000 ] $ \drift
      -> keystampValid cfg (issued + drift) fileId (stamp issued) `shouldBe` False

    it "compares the hash in either case"
      $ keystampValid
        cfg
        issued
        fileId
        (show issued <> "-" <> BS.map toUpper (keystampHash cfg issued fileId))
      `shouldBe` True

    it "refuses another file id, and a keystamp that is not <time>-<hash>"
      $ forM_
        [ ( "abc-1-2-3-png", stamp issued )
        , ( fileId, keystampHash cfg issued fileId )
        , ( fileId, stamp issued <> "-x" )
        , ( fileId, "x" <> stamp issued )
        , ( fileId, "" )
        ]
      $ \( requested, given ) -> keystampValid cfg issued requested given `shouldBe` False

  describe "commandTimeValid" $ do
    it "accepts a time up to 300 s from the server's, either way"
      $ map (commandTimeValid 1000) [ 700, 1000, 1300 ] `shouldBe` [ True, True, True ]

    it "refuses one further away"
      $ map (commandTimeValid 1000) [ 699, 1301 ] `shouldBe` [ False, False ]

  describe "fetchParams" $ do
    let params = fetchParams . Map.fromList

    it "takes a numeric fileindex and a numeric or org xres" $ do
      params [ ( "fileindex", "12" ), ( "xres", "org" ) ] `shouldBe` Just ( "12", "org" )
      params [ ( "fileindex", "012" ), ( "xres", "1280" ), ( "keystamp", "x" ) ]
        `shouldBe` Just ( "012", "1280" )

    it "refuses missing, empty or other values"
      $ forM_
        [ [ ( "xres", "org" ) ]
        , [ ( "fileindex", "12" ) ]
        , [ ( "fileindex", "" ), ( "xres", "org" ) ]
        , [ ( "fileindex", "12" ), ( "xres", "" ) ]
        , [ ( "fileindex", "-12" ), ( "xres", "org" ) ]
        , [ ( "fileindex", "12" ), ( "xres", "orgx" ) ]
        , [ ( "fileindex", "12" ), ( "xres", "ORG" ) ]
        ]
      $ \given -> params given `shouldBe` Nothing

  describe "responseHeaders" $ do
    it "are the official client's: no User-Agent, Cache-Control only with a body" $ do
      responseHeaders "image/jpeg" 10
        `shouldBe` [ ( "Server", "Genetic Lifeform and Distributed Open Server 1.6.6" )
                   , ( "Connection", "close" )
                   , ( "Content-Type", "image/jpeg" )
                   , ( "Content-Length", "10" )
                   , ( "Cache-Control", "public, max-age=31536000" )
                   ]
      map fst (responseHeaders "text/html" 0)
        `shouldBe` [ "Server", "Connection", "Content-Type", "Content-Length" ]
