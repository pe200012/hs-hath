module FileURISpec ( fileURISpecs ) where

import qualified Data.ByteString.Char8 as BS

import           Hath.FileURI          ( FileURI(..), parseFileURI, renderFileURIBytes, renderFileURIText )

import           Relude

import           Test.Hspec
import           Test.Hspec.QuickCheck ( prop )
import           Test.QuickCheck

-- | A field of a file id: anything without a dash.
field :: Gen ByteString
field = BS.pack <$> listOf (elements (['a' .. 'f'] <> ['0' .. '9'] <> "._ /"))

fileURI :: Gen FileURI
fileURI = FileURI <$> field <*> size <*> size <*> size <*> field
  where
    size = getNonNegative <$> arbitrary

fileURISpecs :: Spec
fileURISpecs = describe "FileURI" $ do
  prop "parses back what it renders" $ forAll fileURI $ \uri ->
    parseFileURI (renderFileURIBytes uri) === Just uri

  prop "is a file id exactly when it has five fields" $ forAll (BS.pack <$> listOf (elements "ab1-")) $ \bytes ->
    isJust (parseFileURI bytes) === (BS.count '-' bytes == 4)

  it "renders a canonical file id as it was written" $ do
    let fid = "abcdef1234567890-12345-1024-768-jpg"
    renderFileURIBytes <$> parseFileURI fid `shouldBe` Just fid

  it "reads a field that is not a number as 0" $
    fileSize <$> parseFileURI "abcdef-big-1024-768-jpg" `shouldBe` Just 0

  prop "renders the same id as text and as bytes" $ forAll fileURI $ \uri ->
    renderFileURIText uri === decodeUtf8 (renderFileURIBytes uri)

  prop "shows as its id" $ forAll fileURI $ \uri -> show uri === toString (renderFileURIText uri)
