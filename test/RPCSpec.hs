module RPCSpec ( rpcSpecs ) where

import qualified Data.ByteString.Char8  as BS
import qualified Data.Text              as T
import           Data.Time.Clock.System ( SystemTime(systemSeconds), getSystemTime )

import           Hath.Config            ( ClientConfig(..), defaultClientConfig )
import           Hath.Error             ( HathError(..) )
import           Hath.FileURI           ( parseFileURI )
import           Hath.RPC               ( StartFailure(..)
                                        , clientStart
                                        , getBlacklist
                                        , nextGallery
                                        , overload
                                        , runRPCIO
                                        , startFailure
                                        , toldToStop
                                        , worthRetrying
                                        )
import           Hath.ServerTime        ( serverNow, setServerTime )

import           MockRPC

import           Relude

import           Test.Hspec

config :: ClientConfig
config = defaultClientConfig { clientId = "test-client-001", key = "test-secret-key-12345" }

-- | Back to our own clock, so that the other specs sign with it.
useOurClock :: IO ()
useOurClock = setServerTime . systemSeconds =<< getSystemTime

rpcSpecs :: Spec
rpcSpecs = describe "RPC" $ after_ useOurClock $ do
  describe "worthRetrying" $ do
    it "repeats an exchange that broke off or met a server error" $ do
      worthRetrying Nothing `shouldBe` True
      worthRetrying (Just 500) `shouldBe` True
      worthRetrying (Just 403) `shouldBe` True

    it "takes 404 and 410 as final" $ do
      worthRetrying (Just 404) `shouldBe` False
      worthRetrying (Just 410) `shouldBe` False

  describe "a request" $ do
    it "gets through when two attempts fail"
      $ withMockRPC defaultMockRPC { mockOutages = 2 } (runRPCIO config clientStart)
      >>= (`shouldBe` Right ())

    it "fails after three attempts"
      $ withMockRPC defaultMockRPC { mockOutages = 3 } (runRPCIO config clientStart)
      >>= (`shouldBe` Left (TransportError "HTTP 500"))

    it "gives up on a reply of more than 10 MiB" $ do
      queued <- withMockRPC
        defaultMockRPC { mockQueue = const (BS.replicate 10485761 'x') }
        (runRPCIO config (nextGallery Nothing))
      void queued `shouldBe` Left (TransportError "a reply of more than 10 MiB")

  describe "on KEY_EXPIRED" $ do
    it "takes the server's time and signs a request without add again" $ do
      started <- withMockRPC defaultMockRPC { mockClockAhead = 3600 } (runRPCIO config clientStart)
      started `shouldBe` Right ()
      ahead <- (-) <$> serverNow <*> (systemSeconds <$> getSystemTime)
      ahead `shouldSatisfy` \seconds -> abs (seconds - 3600) <= 1

    it "gives up on a request with add, as the official client does"
      $ withMockRPC defaultMockRPC { mockClockAhead = 3600 } (runRPCIO config (getBlacklist 60))
      >>= (`shouldBe` Left (RequestFailure "KEY_EXPIRED"))

  describe "a failed client_start" $ do
    let refused = \case
          Refused _     -> True
          Unannounced _ -> False

    it "ends the client when another client holds the address or the client ID"
      $ forM_ [ "FAIL_OTHER_CLIENT_CONNECTED", "FAIL_CID_IN_USE" ]
      $ \status -> startFailure 443 (RequestFailure status) `shouldSatisfy` refused

    it "leaves it up after a failed connectivity test, naming the port"
      $ startFailure 443 (RequestFailure "FAIL_CONNECT_TEST") `shouldSatisfy` \case
        Unannounced why -> "443" `T.isInfixOf` why
        Refused _       -> False

    it "leaves it up after any other failure"
      $ forM_ [ RequestFailure "FAIL_STARTUP_FLOOD", TransportError "HTTP 500", EmptyResponse ]
      $ \failure -> startFailure 443 failure `shouldSatisfy` (not . refused)

  describe "a failed still_alive" $ do
    it "stops the client on TERM_BAD_NETWORK"
      $ toldToStop (RequestFailure "TERM_BAD_NETWORK") `shouldBe` True

    it "waits for the next one otherwise"
      $ map toldToStop [ RequestFailure "FAIL", TransportError "HTTP 500", EmptyResponse ]
      `shouldBe` [ False, False, False ]

  describe "overload" $ it "is sent as act=overload" $ do
    seen <- newIORef []
    sent <- withMockRPC
      defaultMockRPC { mockSeen = atomicModifyIORef'_ seen . (:) . fst }
      (runRPCIO config overload)
    sent `shouldBe` Right ()
    readIORef seen `shouldReturn` [ "overload" ]

  describe "get_blacklist" $ it "gives the file ids, skipping lines that are not one" $ do
    let fileId = "0123456789abcdef0123456789abcdef01234567-1000-800-600-jpg"
    blacklist <- withMockRPC
      defaultMockRPC { mockBlacklist = [ fileId, "", "not-a-file-id" ] }
      (runRPCIO config (getBlacklist 43200))
    blacklist `shouldBe` Right (maybeToList (parseFileURI fileId))
