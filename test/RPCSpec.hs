-- | The RPC transport against the mock RPC server.
module RPCSpec ( rpcSpecs ) where

import           Data.Time.Clock.System ( SystemTime(systemSeconds), getSystemTime )

import           Hath.Config            ( ClientConfig(..), defaultClientConfig )
import           Hath.Error             ( HathError(..) )
import           Hath.RPC               ( clientStart, getBlacklist, runRPCIO, worthRetrying )
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
      >>= (`shouldBe` Right True)

    it "fails after three attempts"
      $ withMockRPC defaultMockRPC { mockOutages = 3 } (runRPCIO config clientStart)
      >>= (`shouldBe` Left (TransportError "HTTP 500"))

  describe "on KEY_EXPIRED" $ do
    it "takes the server's time and signs a request without add again" $ do
      started <- withMockRPC defaultMockRPC { mockClockAhead = 3600 } (runRPCIO config clientStart)
      started `shouldBe` Right True
      ahead <- (-) <$> serverNow <*> (systemSeconds <$> getSystemTime)
      ahead `shouldSatisfy` \seconds -> abs (seconds - 3600) <= 1

    it "gives up on a request with add, as the official client does"
      $ withMockRPC defaultMockRPC { mockClockAhead = 3600 } (runRPCIO config (getBlacklist 60))
      >>= (`shouldBe` Left (RequestFailure "KEY_EXPIRED"))
