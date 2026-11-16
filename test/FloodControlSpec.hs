module FloodControlSpec ( floodControlSpecs ) where

import qualified Data.HashMap.Strict      as HashMap

import           Hath.Server.FloodControl ( IP(..)
                                          , IPMap
                                          , checkRateLimit
                                          , isLocalIP
                                          , newFloodControl
                                          , parseIPv4
                                          , registerHit
                                          )

import           Network.Socket           ( tupleToHostAddress, tupleToHostAddress6 )

import           Relude

import           Test.Hspec
import           Test.Hspec.QuickCheck    ( prop )
import           Test.QuickCheck

visitor :: IP
visitor = IPv4 (tupleToHostAddress ( 203, 0, 113, 7 ))

-- | The records after hits at these times, and whether each hit was allowed.
hits :: [ Double ] -> ( IPMap, [ Bool ] )
hits = foldl' hit ( HashMap.empty, [] )
  where
    hit ( records, verdicts ) time = let
        ( records', allowed ) = registerHit time visitor records
      in
        ( records', verdicts <> [ allowed ] )

floodControlSpecs :: Spec
floodControlSpecs = describe "Flood control" $ do
  it "matches the official client" $ do
    floodControl <- newFloodControl
    let other = IPv4 (tupleToHostAddress ( 203, 0, 113, 8 ))
        t0    = 1000 :: Double
    -- ten quick hits pass, the eleventh blocks
    firstTen <- forM [ 0 .. 9 ] $ \k -> checkRateLimit floodControl visitor (t0 + 0.1 * k)
    firstTen `shouldBe` replicate 10 True
    checkRateLimit floodControl visitor (t0 + 1.0) `shouldReturn` False
    -- blocked for 60 s, then counting starts over
    checkRateLimit floodControl visitor (t0 + 59) `shouldReturn` False
    checkRateLimit floodControl visitor (t0 + 62) `shouldReturn` True
    -- one hit per second drains as fast as it fills
    steady <- forM [ 0 .. 29 ] $ \k -> checkRateLimit floodControl other (t0 + k)
    steady `shouldBe` replicate 30 True

  it "treats loopback, private and IPv4-mapped local addresses as local" $ do
    isLocalIP (IPv4 (tupleToHostAddress ( 127, 0, 0, 1 ))) `shouldBe` True
    isLocalIP (IPv4 (tupleToHostAddress ( 172, 20, 1, 1 ))) `shouldBe` True
    isLocalIP (IPv4 (tupleToHostAddress ( 172, 32, 1, 1 ))) `shouldBe` False
    isLocalIP visitor `shouldBe` False
    isLocalIP (IPv6 (tupleToHostAddress6 ( 0, 0, 0, 0, 0, 0xffff, 0x7f00, 1 ))) `shouldBe` True
    isLocalIP (IPv6 (tupleToHostAddress6 ( 0, 0, 0, 0, 0, 0xffff, 0xcb00, 0x7107 ))) `shouldBe` False
    isLocalIP (IPv6 (tupleToHostAddress6 ( 0xfd12, 0, 0, 0, 0, 0, 0, 1 ))) `shouldBe` True

  it "parses rpc_server_ip entries" $ do
    parseIPv4 "::ffff:192.0.2.10" `shouldBe` Just (IPv4 (tupleToHostAddress ( 192, 0, 2, 10 )))
    parseIPv4 "198.51.100.20" `shouldBe` Just (IPv4 (tupleToHostAddress ( 198, 51, 100, 20 )))
    parseIPv4 "300.1.1.1" `shouldBe` Nothing
    parseIPv4 "fe80::1" `shouldBe` Nothing

  prop "never blocks a visitor who asks at most once a second" $ forAll (listOf (choose ( 1, 30 ))) $ \gaps ->
    snd (hits (scanl (+) 1000 gaps)) === (True <$ (1000 : gaps))

  it "allows ten hits at once and refuses the eleventh" $
    snd (hits (replicate 11 1000)) `shouldBe` replicate 10 True <> [ False ]

  it "refuses a blocked visitor without counting the request" $ do
    let blocked = fst (hits (replicate 11 1000))
    registerHit 1030 visitor blocked `shouldBe` ( blocked, False )
