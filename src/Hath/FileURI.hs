-- | File ids as the H\@H network writes them:
-- @\<sha1>-\<size>-\<xres>-\<yres>-\<type>@, or @\<sha1>-\<size>-\<type>@ for a
-- file without a resolution.
module Hath.FileURI
  ( FileURI(..)
  , parseFileURI
  , renderFileURIBytes
  , renderFileURIText
  , mimeType
  ) where

import qualified Data.ByteString.Char8 as BS
import           Data.Char             ( isDigit )
import qualified Data.List             as List

import           Prelude               ( Show(show) )

import           Relude                hiding ( show )
import qualified Relude

data FileURI
  = FileURI { fileHash :: !ByteString
              -- ^ SHA-1 of the content, in hex
            , fileSize :: !Int
            , fileXRes :: !Int
              -- ^ 0, as is 'fileYRes', for an id without a resolution
            , fileYRes :: !Int
            , fileExt  :: !ByteString
            }
  deriving ( Eq, Ord )

-- | Shows the file id itself, which is what logs want.
instance Show FileURI where
  show = toString . renderFileURIText

-- | A file id the official client accepts (HVFile.isValidHVFileid and
-- getHVFileFromFileid): a lower-case SHA-1, a size of up to 10 digits that fits
-- a Java int, a resolution of up to 5 digits a side or none, and a known type.
parseFileURI :: ByteString -> Maybe FileURI
parseFileURI bytes = case BS.split '-' bytes of
  [ hash, size, ext ] -> fileURI hash size (Just 0) (Just 0) ext
  [ hash, size, xres, yres, ext ] -> fileURI hash size (number 5 xres) (number 5 yres) ext
  _ -> Nothing
  where
    fileURI hash size xres yres ext = do
      guard (BS.length hash == 40 && BS.all isHex hash && isJust (List.lookup ext fileTypes))
      FileURI hash <$> mfilter (<= 2147483647) (number 10 size) <*> xres <*> yres <*> pure ext

    isHex c = isDigit c || (c >= 'a' && c <= 'f')

    number digits s = do
      guard (not (BS.null s) && BS.length s <= digits && BS.all isDigit s)
      fst <$> BS.readInt s

-- | The id of a file, as the official client writes it (HVFile.getFileid).
renderFileURIBytes :: FileURI -> ByteString
renderFileURIBytes uri
  | fileXRes uri > 0
    = BS.intercalate
      "-"
      [ fileHash uri
      , Relude.show (fileSize uri)
      , Relude.show (fileXRes uri)
      , Relude.show (fileYRes uri)
      , fileExt uri
      ]
  | otherwise = BS.intercalate "-" [ fileHash uri, Relude.show (fileSize uri), fileExt uri ]

renderFileURIText :: FileURI -> Text
renderFileURIText = decodeUtf8 . renderFileURIBytes

mimeType :: FileURI -> ByteString
mimeType uri = fromMaybe "application/octet-stream" (List.lookup (fileExt uri) fileTypes)

-- | The types a file id may have, and their Content-Type (HVFile.getMimeType).
fileTypes :: [ ( ByteString, ByteString ) ]
fileTypes
  = [ ( "jpg", "image/jpeg" )
    , ( "png", "image/png" )
    , ( "gif", "image/gif" )
    , ( "mp4", "video/mp4" )
    , ( "wbm", "video/webm" )
    , ( "wbp", "image/webp" )
    , ( "avf", "image/avif" )
    , ( "jxl", "image/jxl" )
    ]
