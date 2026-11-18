module LocateSpec ( locateSpecs ) where

import           Control.Exception  ( throwIO )

import           Hath.Download      ( receive )
import           Hath.FileURI       ( FileURI(..) )
import           Hath.Locate        ( usableReply )

import           Network.HTTP.Types ( hContentLength )

import           Relude

import           System.IO.Error    ( userError )

import           Test.Hspec

thousandBytes :: FileURI
thousandBytes
  = FileURI { fileHash = "0123456789abcdef0123456789abcdef01234567"
            , fileSize = 1000
            , fileXRes = 800
            , fileYRes = 600
            , fileExt  = "jpg"
            }

locateSpecs :: Spec
locateSpecs = describe "Locate" $ do
  describe "usableReply" $ do
    let announcing status len
          = usableReply 1073741824 thousandBytes status [ ( hContentLength, len ) ]

    it "takes a 200 that announces the file's length" $ announcing 200 "1000" `shouldBe` True

    it "refuses another status, another length, or none" $ do
      announcing 404 "1000" `shouldBe` False
      announcing 200 "999" `shouldBe` False
      usableReply 1073741824 thousandBytes 200 [] `shouldBe` False

    it "refuses a file larger than max_allowed_filesize"
      $ usableReply 999 thousandBytes 200 [ ( hContentLength, "1000" ) ] `shouldBe` False

  describe "receive" $ do
    it "hands over each chunk with its offset"
      $ receiving 6 [ "abc", "def" ] `shouldReturn` ( True, [ ( 0, "abc" ), ( 3, "def" ) ] )

    it "fails a body that ends short of its length"
      $ receiving 6 [ "abc" ] `shouldReturn` ( False, [ ( 0, "abc" ) ] )

    it "fails a body that brings more than its length, keeping nothing past it"
      $ receiving 4 [ "abc", "def" ] `shouldReturn` ( False, [ ( 0, "abc" ) ] )

    it "fails when reading fails" $ do
      outcome <- receive 1000000 6 (throwIO (userError "connection reset")) (\_ _ -> pass)
      outcome `shouldSatisfy` isLeft
  where
    receiving size chunks = do
      rest <- newIORef chunks
      kept <- newIORef []
      let body = atomicModifyIORef' rest $ \case
            []          -> ( [], "" )
            next : more -> ( more, next )
      outcome
        <- receive 1000000 size body (\offset chunk -> modifyIORef' kept (( offset, chunk ) :))
      chunksKept <- reverse <$> readIORef kept
      pure ( isRight outcome, chunksKept )
