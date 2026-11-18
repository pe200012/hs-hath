module GallerySpec ( gallerySpecs ) where

import qualified Data.ByteString.Char8   as BS
import qualified Data.Text               as T

import           Hath.Config             ( ClientConfig(..), defaultClientConfig )
import           Hath.Gallery            ( GalleryFile(..)
                                         , GalleryMetadata(..)
                                         , fallbackDirectoryName
                                         , galleryDirectoryName
                                         , parseMetadata
                                         )
import           Hath.Gallery.Downloader ( anotherRound, downloadQueue )
import           Hath.Protocol           ( sha1Hex )
import           Hath.RPC                ( failureReport )
import           Hath.Settings           ( HathSettings(..), defaultHathSettings, parseSettings )
import           Hath.Stats              ( newStatsEnv )

import           MockRPC

import           Relude

import           System.Directory        ( doesPathExist )
import           System.FilePath         ( (</>) )
import           System.IO.Temp          ( withSystemTempDirectory )

import           Test.Hspec

clientKey :: ByteString
clientKey = mockClientKey defaultMockRPC

-- | A download list: the given header lines, a DLHASH that signs the first list
-- of file lines, a FILELIST that holds the second, and 'information'.
galleryList :: [ ByteString ] -> [ ByteString ] -> [ ByteString ] -> ByteString
galleryList header signedLines fileLines
  = BS.intercalate "\n"
  $ header
  <> [ "DLHASH " <> sha1Hex (clientKey <> foldMap ("\n" <>) signedLines), "", "FILELIST" ]
  <> fileLines
  <> [ "", "INFORMATION" ]
  <> information
  <> [ "", "" ]

-- | What follows INFORMATION: not signed, empty lines included.
information :: [ ByteString ]
information = [ "Title: t", "", "Tags: x" ]

downloadList :: [ ByteString ] -> [ ByteString ] -> ByteString
downloadList = galleryList [ "GID 123", "FILECOUNT 2", "MINXRES org", "TITLE t" ]

-- | A list signed as the server signs it.
signedList :: [ ByteString ] -> ByteString
signedList fileLines = downloadList fileLines fileLines

goodLines :: [ ByteString ]
goodLines = [ "1 99 org " <> sha1Hex "x" <> " jpg 001", "2 98 780 unknown webp p_2" ]

refused :: ByteString -> Bool
refused = isLeft . parseMetadata clientKey

gallery :: Int -> ByteString -> Text -> GalleryMetadata
gallery gid minXRes title
  = GalleryMetadata
  { galleryID          = gid
  , galleryMinXRes     = minXRes
  , galleryTitle       = title
  , galleryInformation = ""
  , galleryFileList    = []
  }

gallerySpecs :: Spec
gallerySpecs = describe "Gallery downloader" $ do
  describe "download lists (1.6.6)" $ do
    it "reads a signed list; a file whose hash is unknown has none"
      $ fmap (\file -> ( galleryFileName file, galleryFileHash file )) . galleryFileList
      <$> parseMetadata clientKey (signedList goodLines)
      `shouldBe` Right [ ( "001", Just (sha1Hex "x") ), ( "p_2", Nothing ) ]

    it "keeps the INFORMATION section, as the official client writes it to galleryinfo.txt"
      $ galleryInformation
      <$> parseMetadata clientKey (signedList goodLines) `shouldBe` Right "Title: t\n\nTags: x\n"

    it "cleans up the title as the official client does: the quirks of its pattern included"
      $ galleryTitle
      <$> parseMetadata
        clientKey
        (galleryList
           [ "GID 1", "MINXRES org", "TITLE \t a*b\"c\\d<e>f  g:|?h ? : | :| x\vy\194\160 " ]
           goodLines
           goodLines)
      `shouldBe` Right "abcdef gh ? : | :| x y\160"

    it "refuses a list without a TITLE"
      $ galleryList [ "GID 1", "MINXRES org" ] goodLines goodLines `shouldSatisfy` refused

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

  describe "the gallery's directory" $ do
    it "is named for the title and the gallery, and the resolution unless it is the original" $ do
      galleryDirectoryName 125 (gallery 123 "org" "t") `shouldBe` Just "t [123]"
      galleryDirectoryName 125 (gallery 123 "780" "t") `shouldBe` Just "t [123-780x]"

    it "cuts a long title short, to max_filename_length UTF-16 code units" $ do
      let named maxLength title = galleryDirectoryName maxLength (gallery 123 "org" title)
      named 125 (T.replicate 200 "a") `shouldBe` Just (T.replicate 116 "a" <> "... [123]")
      named 20 (T.replicate 200 "a") `shouldBe` Just "aaaaaaaaaaa... [123]"
      named 125 (T.replicate 119 "a") `shouldBe` Just (T.replicate 119 "a" <> " [123]")
      -- the official client keeps a character whole that the cut would split, one unit over
      named 125 (T.replicate 115 "a" <> "\128512" <> T.replicate 20 "b")
        `shouldBe` Just (T.replicate 115 "a" <> "\128512... [123]")
      named 125 (T.replicate 116 "a" <> "\128512" <> T.replicate 20 "b")
        `shouldBe` Just (T.replicate 116 "a" <> "... [123]")

    it "is not made for a title that would leave the download directory"
      $ galleryDirectoryName 125 (gallery 123 "org" "a/b") `shouldBe` Nothing

    it "falls back on the gallery and the resolution" $ do
      fallbackDirectoryName (gallery 123 "org" "t") `shouldBe` "123"
      fallbackDirectoryName (gallery 123 "780" "t") `shouldBe` "123-780x"

    it "takes max_filename_length from the settings"
      $ maxFilenameLength (parseSettings defaultHathSettings [ "max_filename_length=60" ])
      `shouldBe` 60

  describe "rounds" $ do
    it "go on for at most nine rounds" $ do
      anotherRound 10 8 0 `shouldBe` True
      anotherRound 10 9 0 `shouldBe` False

    it "stop once the failures reach twice the number of files" $ do
      anotherRound 3 1 5 `shouldBe` True
      anotherRound 3 1 6 `shouldBe` False

  describe "the failure report" $ do
    it "lists each failure once"
      $ failureReport [ "a-1-org", "b-2-org", "a-1-org" ] `shouldBe` Just "a-1-org;b-2-org"

    it "is not sent without failures, or with more than 50" $ do
      failureReport [] `shouldBe` Nothing
      failureReport (map show [ 1 .. 50 :: Int ]) `shouldSatisfy` isJust
      failureReport (map show [ 1 .. 51 :: Int ]) `shouldBe` Nothing

  it "works through the queue: each gallery is marked done, whether or not its files came"
    $ withSystemTempDirectory "hath-download"
    $ \downloads -> do
      seen <- newIORef []
      let content = "GALLERY_FILE"
          -- the first gallery's file comes; the second's has another hash
          queue   = \case
            ""      -> galleryList
              [ "GID 1", "MINXRES org", "TITLE A <Test>  Gallery" ]
              [ "1 11 org " <> sha1Hex content <> " jpg 001" ]
              [ "1 11 org " <> sha1Hex content <> " jpg 001" ]
            "1;org" -> galleryList
              [ "GID 2", "MINXRES 780", "TITLE B" ]
              [ "1 21 780 " <> sha1Hex "other" <> " jpg 001" ]
              [ "1 21 780 " <> sha1Hex "other" <> " jpg 001" ]
            _       -> "NO_PENDING_DOWNLOADS"
          config
            = defaultClientConfig { key = decodeUtf8 clientKey, downloadDir = toText downloads }
      stats <- newStatsEnv
      settings <- newTVarIO defaultHathSettings
      outcome <- withMockRPC
        defaultMockRPC
        { mockFileContent = content, mockQueue = queue, mockSeen = atomicModifyIORef'_ seen . (:) }
        (downloadQueue config stats settings)
      outcome `shouldBe` Right ()
      -- the second file fails twice, twice its gallery's number of files, and
      -- is reported once
      reverse
        <$> readIORef seen
        `shouldReturn` [ ( "fetchqueue", "" )
                       , ( "dlfetch", "1;1;11;org;1" )
                       , ( "fetchqueue", "1;org" )
                       , ( "dlfetch", "2;1;21;780;1" )
                       , ( "dlfetch", "2;1;21;780;2" )
                       , ( "dlfails", "localhost-21-780" )
                       , ( "fetchqueue", "2;780" )
                       ]
      readFileBS (downloads </> "A Test Gallery [1]" </> "001.jpg") `shouldReturn` content
      readFileBS (downloads </> "A Test Gallery [1]" </> "galleryinfo.txt")
        `shouldReturn` "Title: t\n\nTags: x\n"
      doesPathExist (downloads </> "B [2-780x]") `shouldReturn` True
      doesPathExist (downloads </> "B [2-780x]" </> "001.jpg") `shouldReturn` False
      doesPathExist (downloads </> "B [2-780x]" </> "galleryinfo.txt") `shouldReturn` False
