-- | Reading request paths and server commands as the official client does.
module APISpec ( apiSpecs ) where

import qualified Data.Map.Strict    as Map

import           Hath.Server.API    ( ProxyTest(..)
                                    , Route(..)
                                    , ServerCommand(..)
                                    , parseServerCommand
                                    , route
                                    )

import           Network.HTTP.Types ( Status, status400, status403, status404, statusCode )

import           Relude

import           Test.Hspec

-- | What a path comes to, shown so that failures can be read.
routed :: ByteString -> Text
routed path = case route path of
  Favicon -> "favicon"
  Robots -> "robots"
  File fileId options -> "file " <> fileId <> " " <> options
  ServerCmd command additional time commandKey
    -> unwords [ "servercmd", command, additional, show time, commandKey ]
  Test size time testKey -> unwords [ "t", show size, show time, testKey ]
  Metrics -> "metrics"
  AdminSettings -> "admin"
  Refuse status -> refused status

refused :: Status -> Text
refused status = "refuse " <> show (statusCode status)

apiSpecs :: Spec
apiSpecs = describe "API" $ do
  describe "route" $ do
    it "finds the official paths and ours"
      $ map routed [ "/favicon.ico", "/robots.txt", "/metrics", "/admin/settings" ]
      `shouldBe` [ "favicon", "robots", "metrics", "admin" ]

    it "takes /h/<file id>/<options>, the file name or not"
      $ map
        routed
        [ "/h/id/keystamp=1-a;xres=org/name.jpg", "/h/id/keystamp=1-a", "/h/id/opts/name/more" ]
      `shouldBe` [ "file id keystamp=1-a;xres=org", "file id keystamp=1-a", "file id opts" ]

    it "decodes %3d, as the official client, and nothing else"
      $ map
        routed
        [ "/h/id/keystamp%3d1-a%3Bxres%3dorg", "/servercmd/speed_test/testsize%3d5/7/k" ]
      `shouldBe` [ "file id keystamp=1-a%3Bxres=org", "servercmd speed_test testsize=5 7 k" ]

    it "takes /servercmd/<command>/<additional>/<time>/<key>, additional empty or not"
      $ map routed [ "/servercmd/still_alive//1700000000/k", "/servercmd/STILL_ALIVE/a=1/-5/k/x" ]
      `shouldBe` [ "servercmd still_alive  1700000000 k", "servercmd STILL_ALIVE a=1 -5 k" ]

    it "takes /t/<size>/<time>/<key>, the last part or not"
      $ map routed [ "/t/1000/1700000000/k/0", "/t/1000/1700000000/k" ]
      `shouldBe` [ "t 1000 1700000000 k", "t 1000 1700000000 k" ]

    it "refuses what the official client refuses, as it does" $ do
      map routed [ "/h/id", "/h/id/", "/h", "/t/1000/1700000000", "/t/size/1700000000/k" ]
        `shouldBe` replicate 5 (refused status400)
      map routed [ "/servercmd/still_alive/a/1700000000", "/servercmd/still_alive/a/soon/k" ]
        `shouldBe` replicate 2 (refused status403)
      map routed [ "/", "", "/anything", "/favicon.ico/x", "//h/id/opts", "/H/id/opts" ]
        `shouldBe` replicate 6 (refused status404)

  describe "parseServerCommand" $ do
    let params    = Map.fromList
        proxyTest
          = params
            [ ( "hostname", "192.0.2.1" )
            , ( "port", "443" )
            , ( "testsize", "1000" )
            , ( "testcount", "3" )
            , ( "testtime", "1700000000" )
            , ( "testkey", "k" )
            ]

    it "reads a command name in any case"
      $ map
        (`parseServerCommand` mempty)
        [ "still_alive", "STILL_ALIVE", "Refresh_Settings", "start_downloader", "refresh_certs" ]
      `shouldBe` map
        Just
        [ StillAlive, StillAlive, RefreshSettings, StartDownloader, RefreshCerts ]

    it "knows no other command" $ parseServerCommand "stillalive" mempty `shouldBe` Nothing

    it "sends 1000000 bytes for a speed_test without testsize" $ do
      parseServerCommand "speed_test" mempty `shouldBe` Just (SpeedTest 1000000)
      parseServerCommand "speed_test" (params [ ( "testsize", "25" ) ])
        `shouldBe` Just (SpeedTest 25)

    it "does not run a speed_test whose testsize is not a number"
      $ forM_ [ "", "lots", "25x" ]
      $ \size -> parseServerCommand "speed_test" (params [ ( "testsize", size ) ])
      `shouldBe` Nothing

    it "aims a threaded_proxy_test at the other client's /t/, by http unless told otherwise" $ do
      parseServerCommand "threaded_proxy_test" proxyTest
        `shouldBe` Just
          (ThreadedProxyTest (ProxyTest 3 1000 "http://192.0.2.1:443/t/1000/1700000000/k/0"))
      (fmap proxyTestURL . testOf
       <$> parseServerCommand "threaded_proxy_test" (Map.insert "protocol" "https" proxyTest))
        `shouldBe` Just (Just "https://192.0.2.1:443/t/1000/1700000000/k/0")

    it "does not run a threaded_proxy_test with a parameter missing or not a number"
      $ forM_ [ Map.delete "port" proxyTest, Map.insert "testcount" "x" proxyTest ]
      $ \given -> parseServerCommand "threaded_proxy_test" given `shouldBe` Nothing
  where
    testOf = \case
      ThreadedProxyTest test -> Just test
      _ -> Nothing
