-- | The cap on open connections and the overload notice near it.
module ConnectionLimitSpec ( connectionLimitSpecs ) where

import           Control.Concurrent          ( threadDelay )

import           Hath.Server.ConnectionLimit ( Admission(..)
                                             , admission
                                             , connectionLimit
                                             , overloadDue
                                             )
import           Hath.Settings               ( HathSettings(..), defaultHathSettings )

import           Network.Socket              ( SockAddr(SockAddrInet), tupleToHostAddress )

import           Relude

import           Test.Hspec

connectionLimitSpecs :: Spec
connectionLimitSpecs = describe "Connection limit" $ do
  describe "admission" $ do
    it "admits a connection while no more than the limit are open before it" $ do
      admission 20 20 `shouldBe` AdmittedNearLimit
      admission 20 21 `shouldBe` Refused

    it "is near the limit past 80 % of it" $ do
      admission 20 16 `shouldBe` Admitted
      admission 20 17 `shouldBe` AdmittedNearLimit

  describe "overloadDue" $ do
    it "is due the first time" $ overloadDue 100 Nothing `shouldBe` True

    it "is due again more than 30 s after the last notice" $ do
      overloadDue 100 (Just 70) `shouldBe` False
      overloadDue 100.5 (Just 70) `shouldBe` True

  describe "the Warp hooks"
    $ it
      "refuse readers past the limit, but not the RPC server or local addresses, and notify once"
    $ do
      settings <- newTVarIO
        defaultHathSettings { maxConnectionsOverride = 5, rpcServerIPs = [ "192.0.2.10" ] }
      notices <- newIORef (0 :: Int)
      ( onOpen, onClose ) <- connectionLimit settings (atomicModifyIORef'_ notices (+ 1))
      let peer a b c d = SockAddrInet 443 (tupleToHostAddress ( a, b, c, d ))
          visitor      = peer 198 51 100 7
      -- the sixth finds five open, more than 80 %; the seventh six, more than 5
      replicateM 7 (onOpen visitor) `shouldReturn` replicate 6 True <> [ False ]
      onOpen (peer 192 0 2 10) `shouldReturn` True
      onOpen (peer 127 0 0 1) `shouldReturn` True
      -- back to five open: near the limit again, but within 30 s of the notice
      replicateM_ 4 (onClose visitor)
      onOpen visitor `shouldReturn` True
      threadDelay 100000
      readIORef notices `shouldReturn` 1
