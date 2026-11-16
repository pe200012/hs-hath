module FileURISpec ( fileURISpecs ) where

import qualified Data.ByteString.Char8 as BS

import           Hath.FileURI          ( FileURI(..)
                                       , mimeType
                                       , parseFileURI
                                       , renderFileURIBytes
                                       , renderFileURIText
                                       )

import           Relude

import           Test.Hspec
import           Test.Hspec.QuickCheck ( prop )
import           Test.QuickCheck

-- | A file the official client would accept, with or without a resolution.
fileURI :: Gen FileURI
fileURI = do
  digest <- BS.pack <$> vectorOf 40 (elements ([ '0' .. '9' ] <> [ 'a' .. 'f' ]))
  size <- choose ( 0, 2147483647 )
  ( xres, yres ) <- oneof [ pure ( 0, 0 ), (,) <$> choose ( 1, 99999 ) <*> choose ( 0, 99999 ) ]
  FileURI digest size xres yres
    <$> elements [ "jpg", "png", "gif", "mp4", "wbm", "wbp", "avf", "jxl" ]

hash :: ByteString
hash = "0123456789abcdef0123456789abcdef01234567"

fileURISpecs :: Spec
fileURISpecs = describe "FileURI" $ do
  prop "parses back what it renders" $ forAll fileURI $ \uri
    -> parseFileURI (renderFileURIBytes uri) === Just uri

  it "reads both forms of a file id"
    $ map
      parseFileURI
      [ hash <> "-1000-1280-1707-jpg", hash <> "-1000-wbm", hash <> "-0001000-0-0-png" ]
    `shouldBe` [ Just (FileURI hash 1000 1280 1707 "jpg")
               , Just (FileURI hash 1000 0 0 "wbm")
               , Just (FileURI hash 1000 0 0 "png")
               ]

  it "renders an id without a resolution in three parts"
    $ renderFileURIBytes (FileURI hash 1000 0 0 "mp4") `shouldBe` hash <> "-1000-mp4"

  it "refuses what the official client refuses (HVFile.isValidHVFileid)"
    $ forM_
      [ "0123456789ABCDEF0123456789abcdef01234567-1000-1280-1707-jpg" -- upper-case hash
      , BS.drop 1 hash <> "-1000-1280-1707-jpg"                       -- 39 characters
      , hash <> "-1000-1280-1707-txt"                                 -- unknown type
      , hash <> "-1000-1280-1707-JPG"
      , hash <> "-12345678901-1280-1707-jpg"                          -- 11 digits
      , hash <> "-2147483648-1280-1707-jpg"                           -- more than a Java int
      , hash <> "-1000-128000-1707-jpg"                               -- 6 digits
      , hash <> "-1000-1280-jpg"                                      -- four parts
      , hash <> "-1000--1707-jpg"
      , hash <> "-+1000-1280-1707-jpg"
      , hash <> "-big-1280-1707-jpg"
      , hash <> "-1000-1280-1707-jpg-"
      , ""
      ]
    $ \fileId -> parseFileURI fileId `shouldBe` Nothing

  it "knows the Content-Type of each type"
    $ map (mimeType . FileURI hash 1 0 0) [ "jpg", "wbm", "wbp", "avf", "jxl" ]
    `shouldBe` [ "image/jpeg", "video/webm", "image/webp", "image/avif", "image/jxl" ]

  prop "renders the same id as text and as bytes" $ forAll fileURI $ \uri -> renderFileURIText uri
    === decodeUtf8 (renderFileURIBytes uri)

  prop "shows as its id" $ forAll fileURI $ \uri -> show uri === toString (renderFileURIText uri)
