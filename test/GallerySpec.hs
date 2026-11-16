module GallerySpec ( gallerySpecs ) where

import           Hath.Gallery            ( GalleryFile(..), GalleryMetadata(..) )
import           Hath.Gallery.Downloader ( galleryFilePath )

import           Relude

import           Test.Hspec

gallerySpecs :: Spec
gallerySpecs = describe "Gallery downloader" $
  it "puts a file under the download directory, in a directory named for its gallery" $ do
    let gallery
          = GalleryMetadata
          { galleryID = 123, galleryFileCount = 1, galleryMinXRes = "org", galleryTitle = "t", galleryFileList = [ file ] }
        file
          = GalleryFile
          { galleryFilePage  = 1
          , galleryFileIndex = 99
          , galleryFileName  = "001"
          , galleryFileXRes  = "org"
          , galleryFileHash  = Nothing
          , galleryFileExt   = "jpg"
          }
    galleryFilePath "/srv/hath/downloads" gallery file `shouldBe` "/srv/hath/downloads/123/001.jpg"
    galleryFilePath "./download" gallery file `shouldBe` "./download/123/001.jpg"
