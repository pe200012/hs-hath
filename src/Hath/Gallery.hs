-- | What the gallery downloader is told to fetch, and the directory it goes into.
module Hath.Gallery
  ( GalleryMetadata(..)
  , GalleryFile(..)
  , parseMetadata
  , galleryDirectoryName
  , fallbackDirectoryName
  ) where

import qualified Data.ByteString.Char8 as BS
import           Data.Char             ( isAsciiLower, isAsciiUpper, isDigit )
import           Data.List             ( dropWhileEnd )
import qualified Data.Text             as T

import           Hath.Protocol         ( sameSecret, sha1Hex )

import           Relude

data GalleryMetadata
  = GalleryMetadata
  { galleryID          :: !Int
  , galleryMinXRes     :: !ByteString
  , galleryTitle       :: !Text
    -- ^ cleaned up as the official client cleans it for a directory name
  , galleryInformation :: !ByteString
    -- ^ the INFORMATION section, which goes into @galleryinfo.txt@
  , galleryFileList    :: ![ GalleryFile ]
  }
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
    let ( fileSection, informationSection ) = break (== "INFORMATION") rest
        fileLines = filter (not . BS.null) fileSection
        listHash = sha1Hex (clientKey <> foldMap ("\n" <>) fileLines)
        field name
          = listToMaybe
            [ BS.drop 1 value
            | line <- header
            , let ( f, value ) = BS.break (== ' ') line
            , f == name
            ]
    gid <- maybeToRight "invalid GID" $ mfilter (> 0) $ readIntExact =<< field "GID"
    minXRes <- maybeToRight "invalid MINXRES" $ mfilter validXRes $ field "MINXRES"
    title <- maybeToRight "no TITLE" $ field "TITLE"
    unless (any (sameSecret listHash) (field "DLHASH"))
      $ Left "DLHASH does not match the file list"
    files <- traverse parseFile fileLines
    pure
      GalleryMetadata
      { galleryID          = gid
      , galleryMinXRes     = minXRes
      , galleryTitle       = cleanTitle (decodeUtf8 title)
        -- Java's split drops the empty lines at the end
      , galleryInformation = foldMap (<> "\n") $ dropWhileEnd BS.null $ drop 1 informationSection
      , galleryFileList    = files
      }
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

-- | The title as the official client cleans it (GalleryDownloader.java:256). Its
-- pattern takes out @*@, @"@, @\\@, @<@, @>@ and the sequence @:|?@ (the @|@ there
-- is escaped, so a lone @:@, @|@ or @?@ stays), then makes every run of ASCII
-- whitespace one space and trims the ends. Taking out the sequence first gives
-- what Java's single pass gives: none of its characters is taken out alone.
cleanTitle :: Text -> Text
cleanTitle
  = T.intercalate " "
  . filter (not . T.null)
  . T.split (`elem` (" \t\n\v\f\r" :: String))
  . T.filter (`notElem` ("*\"\\<>" :: String))
  . T.replace ":|?" ""

-- | The name of the gallery's directory in the download directory
-- (GalleryDownloader.java:259-285): the title, then @[gid]@, or @[gid-<minxres>x]@
-- unless the original size was asked for, so that each size gets a directory of
-- its own. A title that would make the name longer than @max_filename_length@
-- UTF-16 code units, as Java counts its length, is cut short and ends in
-- "...". Nothing if the name would leave the download directory: the official
-- client refuses such a list.
galleryDirectoryName :: Int -> GalleryMetadata -> Maybe Text
galleryDirectoryName maxLength gallery
  | "/" `T.isInfixOf` name = Nothing
  | otherwise = Just name
  where
    title   = galleryTitle gallery

    postfix = " [" <> show (galleryID gallery) <> xresSuffix gallery <> "]"

    name
      | utf16Length title + T.length postfix > maxLength
        = takeUtf16 (maxLength - T.length postfix - 3) title <> "..." <> postfix
      | otherwise = title <> postfix

-- | The name to use when the file system will not take 'galleryDirectoryName'
-- (GalleryDownloader.java:294-298): the same without the title.
fallbackDirectoryName :: GalleryMetadata -> Text
fallbackDirectoryName gallery = show (galleryID gallery) <> xresSuffix gallery

xresSuffix :: GalleryMetadata -> Text
xresSuffix gallery
  | galleryMinXRes gallery == "org" = ""
  | otherwise = "-" <> decodeUtf8 (galleryMinXRes gallery) <> "x"

utf16Length :: Text -> Int
utf16Length = sum . map utf16Units . toString

-- | The characters that start within the first @n@ UTF-16 code units. Like
-- Java's @offsetByCodePoints(0, codePointCount(0, n))@, this keeps whole a
-- character whose surrogate pair the cut would split.
takeUtf16 :: Int -> Text -> Text
takeUtf16 n = toText . go 0 . toString
  where
    go used (c : rest)
      | used < n = c : go (used + utf16Units c) rest
    go _ _ = []

utf16Units :: Char -> Int
utf16Units c
  | ord c > 0xFFFF = 2
  | otherwise = 1
