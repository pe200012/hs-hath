{-# LANGUAGE DataKinds #-}

module R2Spec ( r2Specs ) where

import           Colog                    ( LogAction(..), Message )
import           Colog.Polysemy           ( Log, runLogAction )

import           Hath.Cache               ( Cache, CacheHit(..), lookupFile, storeFile )
import           Hath.Cache.R2            ( R2Connection, mkR2Connection, runCacheR2 )
import           Hath.Config              ( R2Config(..) )
import           Hath.Error               ( HathError )
import           Hath.FileURI             ( FileURI(..) )

import           Network.HTTP.Types       ( status200 )
import           Network.Wai              ( Application
                                          , requestMethod
                                          , responseLBS
                                          , strictRequestBody
                                          )
import           Network.Wai.Handler.Warp ( testWithApplication )

import           Polysemy
import           Polysemy.Error           ( Error, runError )

import           Relude

import           System.Environment       ( setEnv )

import           Test.Hspec

-- | No such account: requests to it fail, which is all these tests need.
testR2Config :: R2Config
testR2Config
  = R2Config { r2Endpoint = "https://00000000000000000000000000000000.r2.cloudflarestorage.com"
             , r2Bucket   = "hath"
             }

sampleURI :: FileURI
sampleURI
  = FileURI
  { fileHash = "testhash1234", fileSize = 1024, fileXRes = 800, fileYRes = 600, fileExt = "jpg" }

sampleContent :: ByteString
sampleContent = "test content for R2 storage"

-- | Run against a bucket, without the log.
runR2Cache :: R2Connection
           -> Sem '[ Cache, Log Message, Error HathError, Embed IO ] a
           -> IO (Either HathError a)
runR2Cache conn
  = runM
  . runError @HathError
  . runLogAction @IO @Message (LogAction $ const pass)
  . runCacheR2 conn

redirected :: Either HathError (Maybe CacheHit) -> Bool
redirected (Right (Just (Redirect _))) = True
redirected _ = False

r2Specs :: Spec
r2Specs = describe "R2 cache" $ beforeAll setupEnv $ do
  describe "mkR2Connection" $ do
    it "connects when the credentials are in the environment"
      $ \_ -> mkR2Connection testR2Config >>= \case
        Left err -> expectationFailure $ "Connection failed: " <> toString err
        Right _  -> pass

    it "fails without R2_ACCESS_KEY" $ \_ -> do
      setEnv "R2_ACCESS_KEY" ""
      result <- mkR2Connection testR2Config
      setEnv "R2_ACCESS_KEY" "12345678901234567890123456789012"
      isLeft result `shouldBe` True

  describe "runCacheR2" $ do
    -- these two need a real bucket
    xit "stores a file" $ \conn -> runR2Cache conn (storeFile sampleURI sampleContent)
      >>= (`shouldBe` Right ())

    xit "redirects to a file it stored" $ \conn -> do
      _ <- runR2Cache conn $ storeFile sampleURI sampleContent
      runR2Cache conn (lookupFile sampleURI) >>= (`shouldSatisfy` redirected)

    it "misses a file the bucket does not answer for"
      $ \conn -> runR2Cache conn (lookupFile sampleURI { fileHash = "nonexistent999" })
      >>= (`shouldSatisfy` (not . redirected))

  describe "link cache" $ do
    it "asks the bucket once per object, then answers from memory"
      $ \_ -> withFakeBucket $ \heads conn -> do
        a <- runR2Cache conn $ lookupFile sampleURI
        b <- runR2Cache conn $ lookupFile sampleURI
        ( redirected a, redirected b ) `shouldBe` ( True, True )
        readIORef heads `shouldReturn` 1

    it "needs no HEAD for an object it just stored" $ \_ -> withFakeBucket $ \heads conn -> do
      runR2Cache conn (storeFile sampleURI sampleContent) >>= (`shouldBe` Right ())
      runR2Cache conn (lookupFile sampleURI) >>= (`shouldSatisfy` redirected)
      readIORef heads `shouldReturn` 0

-- | A bucket that has every object and counts the HEADs it is asked.
fakeBucket :: IORef Int -> Application
fakeBucket heads req respond = do
  when (requestMethod req == "HEAD") $ atomicModifyIORef' heads (\n -> ( n + 1, () ))
  _ <- strictRequestBody req
  respond
    $ responseLBS
      status200
      [ ( "ETag", "\"0\"" )
      , ( "Last-Modified", "Mon, 09 Nov 2026 00:00:00 GMT" )
      , ( "Content-Length", "0" )
      ]
      ""

withFakeBucket :: (IORef Int -> R2Connection -> IO a) -> IO a
withFakeBucket k = do
  heads <- newIORef 0
  testWithApplication (pure (fakeBucket heads)) $ \port
    -> mkR2Connection testR2Config { r2Endpoint = "http://127.0.0.1:" <> show port } >>= \case
      Left err   -> error err
      Right conn -> k heads conn

-- | Put credentials (of the right lengths) in the environment and connect.
setupEnv :: IO R2Connection
setupEnv = do
  setEnv "R2_ACCESS_KEY" "12345678901234567890123456789012"
  setEnv "R2_SECRET_KEY" "1234567890123456789012345678901234567890"
  mkR2Connection testR2Config
    >>= either (\err -> error $ "Failed to create R2 connection: " <> err) pure
