{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module FileURIRenderSpec ( fileURIRenderSpecs, main ) where

import           Relude

import           Test.Hspec

import           Types            ( FileURI, parseFileURI, renderFileURIBytes, renderFileURIText )

main :: IO ()
main = hspec fileURIRenderSpecs

fileURIRenderSpecs :: Spec
fileURIRenderSpecs = describe "FileURI rendering" $ do
  it "round-trips a canonical file id through parse and byte rendering" $ do
    let fid = "abcdef1234567890-12345-1024-768-jpg"
    renderFileURIBytes (parseFileURI fid) `shouldBe` fid

  it "keeps text rendering consistent with byte rendering" $ do
    let uri = parseFileURI "deadbeefcafebabe-42-640-480-png"
    renderFileURIText uri `shouldBe` decodeUtf8 (renderFileURIBytes uri)

  it "keeps Show consistent with canonical text rendering" $ do
    let uri = parseFileURI "feedface12345678-2048-1920-1080-webp"
    show uri `shouldBe` toString (renderFileURIText uri)
