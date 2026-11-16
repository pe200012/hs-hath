module FloodControlSpec ( floodControlSpecs ) where

import qualified Data.HashMap.Strict      as HashMap

import           Hath.Server.FloodControl ( IP(..), IPMap, registerHit )

import           Network.Socket           ( tupleToHostAddress )

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
floodControlSpecs = describe "Flood control rule" $ do
  prop "never blocks a visitor who asks at most once a second" $ forAll (listOf (choose ( 1, 30 ))) $ \gaps ->
    snd (hits (scanl (+) 1000 gaps)) === (True <$ (1000 : gaps))

  it "allows ten hits at once and refuses the eleventh" $
    snd (hits (replicate 11 1000)) `shouldBe` replicate 10 True <> [ False ]

  it "refuses a blocked visitor without counting the request" $ do
    let blocked = fst (hits (replicate 11 1000))
    registerHit 1030 visitor blocked `shouldBe` ( blocked, False )
