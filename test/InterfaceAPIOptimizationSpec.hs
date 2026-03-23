{-# LANGUAGE OverloadedStrings #-}

module InterfaceAPIOptimizationSpec
  ( interfaceAPIOptimizationSpecs
  , main
  ) where

import qualified Data.IORef            as IORef

import           Relude                hiding ( readFile )

import           Test.Hspec

import           Utils                 ( firstJustM )

main :: IO ()
main = hspec interfaceAPIOptimizationSpecs

interfaceAPIOptimizationSpecs :: Spec
interfaceAPIOptimizationSpecs = describe "Interface.API optimization" $ do
  it "stops after the first Just" $ do
    visitedRef <- IORef.newIORef ([] :: [ Text ])
    let visit label = do
          IORef.modifyIORef' visitedRef (<> [ label ])
          pure $ if label == "hit-2" then Just label else Nothing

    result <- firstJustM visit [ "miss-1", "hit-2", "miss-3" ]
    visited <- IORef.readIORef visitedRef

    result `shouldBe` Just "hit-2"
    visited `shouldBe` [ "miss-1", "hit-2" ]

  it "returns Nothing after visiting every candidate when all fail" $ do
    visitedRef <- IORef.newIORef ([] :: [ Text ])
    let visit label = do
          IORef.modifyIORef' visitedRef (<> [ label ])
          pure Nothing

    result <- firstJustM visit [ "miss-1", "miss-2", "miss-3" ]
    visited <- IORef.readIORef visitedRef

    result `shouldBe` (Nothing :: Maybe Text)
    visited `shouldBe` [ "miss-1", "miss-2", "miss-3" ]

  it "preserves left-to-right evaluation order" $ do
    visitedRef <- IORef.newIORef ([] :: [ Text ])
    let visit label = do
          IORef.modifyIORef' visitedRef (<> [ label ])
          pure $ if label == "hit-3" then Just label else Nothing

    _ <- firstJustM visit [ "miss-1", "miss-2", "hit-3", "miss-4" ]
    visited <- IORef.readIORef visitedRef

    visited `shouldBe` [ "miss-1", "miss-2", "hit-3" ]
