-- | File ids as the H\@H network writes them:
-- @\<sha1>-\<size>-\<xres>-\<yres>-\<type>@.
module Hath.FileURI
  ( FileURI(..)
  , parseFileURI
  , renderFileURIBytes
  , renderFileURIText
  , mimeType
  ) where

import qualified Data.ByteString.Char8 as BS

import           Prelude               ( Show(show) )

import           Relude                hiding ( show )
import qualified Relude

data FileURI
  = FileURI { fileHash :: !ByteString
              -- ^ SHA-1 of the content, in hex
            , fileSize :: !Int
            , fileXRes :: !Int
            , fileYRes :: !Int
            , fileExt  :: !ByteString
            }
  deriving ( Eq, Ord )

-- | Shows the file id itself, which is what logs want.
instance Show FileURI where
  show = toString . renderFileURIText

-- | Five fields separated by dashes. The numbers are read leniently, as they
-- always were: a field that is not a number counts as 0.
parseFileURI :: ByteString -> Maybe FileURI
parseFileURI bytes = case BS.split '-' bytes of
  [ hash, size, xres, yres, ext ] -> Just
    FileURI { fileHash = hash
            , fileSize = number size
            , fileXRes = number xres
            , fileYRes = number yres
            , fileExt  = ext
            }
  _ -> Nothing
  where
    number = maybe 0 fst . BS.readInt

renderFileURIBytes :: FileURI -> ByteString
renderFileURIBytes uri
  = BS.intercalate
    "-"
    [ fileHash uri
    , Relude.show (fileSize uri)
    , Relude.show (fileXRes uri)
    , Relude.show (fileYRes uri)
    , fileExt uri
    ]

renderFileURIText :: FileURI -> Text
renderFileURIText = decodeUtf8 . renderFileURIBytes

-- | Content-Type of a file, from the type in its id (as the official HVFile).
mimeType :: FileURI -> ByteString
mimeType uri = case fileExt uri of
  "jpg" -> "image/jpeg"
  "png" -> "image/png"
  "gif" -> "image/gif"
  "mp4" -> "video/mp4"
  "wbm" -> "video/webm"
  "wbp" -> "image/webp"
  "avf" -> "image/avif"
  "jxl" -> "image/jxl"
  _     -> "application/octet-stream"
