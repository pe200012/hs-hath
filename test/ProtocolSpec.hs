module ProtocolSpec ( protocolSpecs ) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict       as Map

import           Hath.Config           ( ClientConfig(..), defaultClientConfig )
import           Hath.Error            ( HathError(..) )
import           Hath.FileURI          ( FileURI(..) )
import           Hath.Protocol

import           Relude

import           Test.Hspec
import           Test.Hspec.QuickCheck ( prop )
import           Test.QuickCheck

-- | Text free of the separators these formats use.
word :: Gen ByteString
word = BS.pack <$> listOf1 (elements (['a' .. 'z'] <> ['0' .. '9'] <> " ._/"))

-- | A client key, the time of a reply and its payload lines.
reply :: Gen ( ByteString, ByteString, [ ByteString ] )
reply = (,,) <$> word <*> (BS.pack <$> listOf1 (elements ['0' .. '9'])) <*> listOf word

-- | A reply as the 1.6.6 server signs it.
signed :: ByteString -> ByteString -> [ ByteString ] -> ByteString
signed clientKey time payload
  = "OK-" <> time <> "-" <> sha1Hex (time <> "-" <> clientKey <> body) <> body
  where
    body = foldMap ("\n" <>) payload

protocolSpecs :: Spec
protocolSpecs = describe "Protocol" $ do
  describe "parseURLParams" $ do
    it "reads pairs, flags, empty values and values containing =" $
      parseURLParams "a=1;flag;b=x=y;;c="
        `shouldBe` Map.fromList [ ( "a", "1" ), ( "flag", "" ), ( "b", "x=y" ), ( "c", "" ) ]

    prop "reads back every pair, the last of a key winning" $ forAll (listOf ((,) <$> word <*> word)) $ \pairs ->
      parseURLParams (BS.intercalate ";" [ k <> "=" <> v | ( k, v ) <- pairs ]) === Map.fromList pairs

  describe "rpcPayload" $ do
    it "is the lines after OK" $ rpcPayload "OK\na=1\nb=2\n" `shouldBe` Right [ "a=1", "b=2" ]

    it "is the status when that is not OK" $
      rpcPayload "KEY_EXPIRED\nx" `shouldBe` Left (RequestFailure "KEY_EXPIRED")

    it "is an error for an empty reply" $ rpcPayload "" `shouldBe` Left EmptyResponse

  describe "verifyRPCResponse" $ do
    it "drops the empty line a reply ends with" $
      verifyRPCResponse "k" (signed "k" "7" [ "a=1", "b=2" ] <> "\n") `shouldBe` "OK\na=1\nb=2"

    it "refuses an OK that is not signed" $ verifyRPCResponse "k" "OK\na=1" `shouldBe` "RESPONSE_HASH_FAILED"

    it "passes any other status through" $ verifyRPCResponse "k" "KEY_EXPIRED" `shouldBe` "KEY_EXPIRED"

    prop "accepts a reply signed with the client key" $ forAll reply $ \( clientKey, time, payload ) ->
      verifyRPCResponse clientKey (signed clientKey time payload) === BS.intercalate "\n" ("OK" : payload)

    prop "refuses it once a line is added" $ forAll ((,) <$> reply <*> word) $ \( ( clientKey, time, payload ), extra ) ->
      verifyRPCResponse clientKey (signed clientKey time payload <> "\n" <> extra) === "RESPONSE_HASH_FAILED"

    prop "refuses a reply signed with another key" $ forAll reply $ \( clientKey, time, payload ) ->
      verifyRPCResponse (clientKey <> "x") (signed clientKey time payload) === "RESPONSE_HASH_FAILED"

  describe "signatures" $ do
    let cfg = defaultClientConfig { clientId = "12345", key = "secretkey" }

    it "signs an RPC request" $
      rpcActionKey cfg "srfetch" "1;780;abc" 1700000000
        `shouldBe` sha1HexText "hentai@home-srfetch-1;780;abc-12345-1700000000-secretkey"

    it "knows the key of a server command" $
      serverCommandKey cfg "speed_test" "testsize=1000" 1700000000
        `shouldBe` sha1HexText "hentai@home-servercmd-speed_test-testsize=1000-12345-1700000000-secretkey"

    it "knows the key of a speed test" $
      speedTestKey cfg 1000 1700000000 `shouldBe` sha1Hex "hentai@home-speedtest-1000-1700000000-12345-secretkey"

    it "knows the keystamp of a file" $
      keystampHash cfg 1700000000 "abc-1-2-3-jpg"
        `shouldBe` BS.take 10 (sha1Hex "1700000000-abc-1-2-3-jpg-secretkey-hotlinkthis")

    it "identifies itself to an image server" $
      hathRequestToken cfg (FileURI "abc" 1 2 3 "jpg") `shouldBe` "12345-" <> sha1Hex "secretkeyabc-1-2-3-jpg"

  describe "parseKeystamp" $ do
    it "splits the time from the hash" $
      parseKeystamp "1700000000-0123456789" `shouldBe` ( 1700000000, "0123456789" )

    it "reads a keystamp without a time as time 0" $ parseKeystamp "invalid" `shouldBe` ( 0, "" )
