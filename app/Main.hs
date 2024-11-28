module Main where

import           Dhall

import           Relude

import           Server

import           Types

main :: IO ()
main = do
    config <- readClientConfig "./client-login"
    startServer 3000
