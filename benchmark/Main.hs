module Main ( main ) where

import           Criterion.Main

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map              as Map

import           Relude

import           Utils

main :: IO ()
main
    = defaultMain
        [ bgroup
              "hathHash"
              [ bench "small" $ nf hathHash "hello"
              , bench "medium" $ nf hathHash (BS.replicate 1000 'x')
              , bench "large" $ nf hathHash (BS.replicate 100000 'x')
              ]
        , bgroup
              "parseOptions"
              [ bench "empty" $ nf parseOptions ""
              , bench "single" $ nf parseOptions "key=value"
              , bench "multiple" $ nf parseOptions "key1=value1;key2=value2;key3=value3"
              , bench "no values" $ nf parseOptions "key1;key2;key3"
              , bench "mixed" $ nf parseOptions "key1=value1;key2;key3=value3"
              , bench "large" $ nf parseOptions (BS.intercalate ";" $ replicate 100 "key=value")
              ]
        ]
