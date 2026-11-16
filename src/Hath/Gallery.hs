-- | What the gallery downloader is told to fetch.
module Hath.Gallery ( GalleryMetadata(..), GalleryFile(..), parseMetadata ) where

import qualified Data.ByteString.Char8 as BS
import           Data.Char             ( isAsciiLower, isAsciiUpper, isDigit )

import           Hath.Protocol         ( sha1Hex )

import           Relude

data GalleryMetadata
  = GalleryMetadata
  { galleryID :: !Int, galleryMinXRes :: !ByteString, galleryFileList :: ![ GalleryFile ] }
  deriving ( Eq )

data GalleryFile
  = GalleryFile { galleryFilePage  :: !Int
                , galleryFileIndex :: !Int
                , galleryFileName  :: !ByteString
                , galleryFileXRes  :: !ByteString
                , galleryFileHash  :: !(Maybe ByteString)
                  -- ^ Nothing: generated on demand, so there is nothing to verify against
                , galleryFileExt   :: !ByteString
                }
  deriving ( Eq )

-- | Parse a @\/16\/dl@ fetchqueue reply (hathdl format). Left carries the reason it was
-- refused: @NO_PENDING_DOWNLOADS@ and other non-lists, or a list failing the 1.6.6 checks.
-- The file list must carry @DLHASH@ = sha1(client key, then @"\\n" ++ line@ for every
-- file line), and names and types must be ones the gallery system can produce, which
-- keeps a forged list from writing outside the download directory.
parseMetadata :: ByteString -> ByteString -> Either Text GalleryMetadata
parseMetadata clientKey bytes = case break (== "FILELIST") (BS.split '\n' bytes) of
  ( _, [] ) -> Left $ decodeUtf8 $ BS.strip $ BS.take 64 bytes
  ( header, _ : rest ) -> do
    let fileLines  = filter (not . BS.null) (takeWhile (/= "INFORMATION") rest)
        field name
          = listToMaybe
            [ BS.drop 1 value
            | line <- header
            , let ( f, value ) = BS.break (== ' ') line
            , f == name
            ]
    gid <- maybeToRight "invalid GID" $ mfilter (> 0) $ readIntExact =<< field "GID"
    minXRes <- maybeToRight "invalid MINXRES" $ mfilter validXRes $ field "MINXRES"
    when (field "DLHASH" /= Just (sha1Hex (clientKey <> foldMap ("\n" <>) fileLines)))
      $ Left "DLHASH does not match the file list"
    files <- traverse parseFile fileLines
    pure GalleryMetadata { galleryID = gid, galleryMinXRes = minXRes, galleryFileList = files }
  where
    parseFile line = case BS.split ' ' line of
      [ page, index, xres, sha1, ext, name ]
        | Just p <- readIntExact page, Just ix <- readIntExact index, validName name
        , ext `elem` [ "jpg", "gif", "png", "webp" ] -> Right
          GalleryFile { galleryFilePage  = p
                      , galleryFileIndex = ix
                      , galleryFileName  = name
                      , galleryFileXRes  = xres
                      , galleryFileHash  = if sha1 == "unknown"
                          then Nothing
                          else Just sha1
                      , galleryFileExt   = ext
                      }
      _ -> Left $ "invalid file line: " <> decodeUtf8 line

    validName n
      = not (BS.null n)
      && BS.length n <= 250
      && not (".." `BS.isInfixOf` n)
      && BS.all
        (\c -> isAsciiUpper c || isAsciiLower c || isDigit c || c `elem` ("._-" :: String))
        n

    validXRes x = x == "org" || (not (BS.null x) && BS.all isDigit x)

    readIntExact s = case BS.readInt s of
      Just ( n, "" ) -> Just n
      _ -> Nothing
