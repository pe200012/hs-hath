module GallerySpec ( gallerySpecs ) where

import qualified Data.ByteString.Char8   as BS

import           Hath.Gallery            ( GalleryFile(..), GalleryMetadata(..), parseMetadata )
import           Hath.Gallery.Downloader ( galleryFilePath )
import           Hath.Protocol           ( sha1Hex )

import           Relude

import           Test.Hspec

clientKey :: ByteString
clientKey = "test-secret-key-12345"

-- | A download list whose DLHASH signs the first list of file lines and whose
-- FILELIST holds the second.
downloadList :: [ ByteString ] -> [ ByteString ] -> ByteString
downloadList signedLines fileLines
  = BS.intercalate "\n"
  $ [ "GID 123"
    , "FILECOUNT 2"
    , "MINXRES org"
    , "DLHASH " <> sha1Hex (clientKey <> foldMap ("\n" <>) signedLines)
    , "TITLE t"
    , ""
    , "FILELIST"
    ]
  <> fileLines
  <> [ "", "INFORMATION", "not a file line, not signed" ]

-- | A list signed as the server signs it.
signedList :: [ ByteString ] -> ByteString
signedList fileLines = downloadList fileLines fileLines

goodLines :: [ ByteString ]
goodLines = [ "1 99 org " <> sha1Hex "x" <> " jpg 001", "2 98 780 unknown webp p_2" ]

refused :: ByteString -> Bool
refused = isLeft . parseMetadata clientKey

gallerySpecs :: Spec
gallerySpecs = describe "Gallery downloader" $ do
  describe "download lists (1.6.6)" $ do
    it "reads a signed list; a file whose hash is unknown has none"
      $ fmap (\file -> ( galleryFileName file, galleryFileHash file )) . galleryFileList
      <$> parseMetadata clientKey (signedList goodLines)
      `shouldBe` Right [ ( "001", Just (sha1Hex "x") ), ( "p_2", Nothing ) ]

    it "refuses a list that differs from what was signed"
      $ downloadList goodLines (take 1 goodLines) `shouldSatisfy` refused

    it "refuses file names that leave the gallery directory" $ do
      signedList [ "1 99 org h jpg ../../.ssh/x" ] `shouldSatisfy` refused
      signedList [ "1 99 org h jpg .." ] `shouldSatisfy` refused

    it "refuses file types the gallery system does not produce"
      $ signedList [ "1 99 org h exe 001" ] `shouldSatisfy` refused

    it "gives the server's reason when there is no list"
      $ galleryID
      <$> parseMetadata clientKey "NO_PENDING_DOWNLOADS\n" `shouldBe` Left "NO_PENDING_DOWNLOADS"

  it "puts a file under the download directory, in a directory named for its gallery" $ do
    let gallery
          = GalleryMetadata { galleryID        = 123
                            , galleryFileCount = 1
                            , galleryMinXRes   = "org"
                            , galleryTitle     = "t"
                            , galleryFileList  = [ file ]
                            }
        file
          = GalleryFile { galleryFilePage  = 1
                        , galleryFileIndex = 99
                        , galleryFileName  = "001"
                        , galleryFileXRes  = "org"
                        , galleryFileHash  = Nothing
                        , galleryFileExt   = "jpg"
                        }
    galleryFilePath "/srv/hath/downloads" gallery file `shouldBe` "/srv/hath/downloads/123/001.jpg"
    galleryFilePath "./download" gallery file `shouldBe` "./download/123/001.jpg"
