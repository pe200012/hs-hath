{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Cache ( module Cache ) where

import           Colog                   ( Message, WithLog, logInfo )

import           Control.Monad.Reader

import qualified Data.ByteString.Char8   as BS
import qualified Data.HashSet            as HashSet
import           Data.String.Interpolate ( i )

import           Database.SQLite.Simple  ( FromRow, Only(Only), ToRow )

import           Relude

import           Types                   ( HathM, HathSettings(staticRanges), Singleton(..) )

import           Utils

data HathFile
    = HathFile { fileHash :: {-# UNPACK #-} !ByteString
               , fileSize :: {-# UNPACK #-} !Int
               , fileXRes :: {-# UNPACK #-} !Int
               , fileYRes :: {-# UNPACK #-} !Int
               , fileType :: {-# UNPACK #-} !ByteString
               }
    deriving ( Show )

toFileId :: HathFile -> ByteString
toFileId (HathFile h s xres yres typ) = [i|#{h}-#{s}-#{xres}-#{yres}-#{typ}|]

{-# INLINE toFileId #-}

parseHathFile :: ByteString -> Maybe HathFile
parseHathFile param = case BS.split '-' param of
    [ h, size, xres, yres, typ ] -> do
        siz <- readMaybe $ BS.unpack size
        xre <- readMaybe $ BS.unpack xres
        yre <- readMaybe $ BS.unpack yres
        return $ HathFile h siz xre yre typ
    [ h, size, typ ] -> do
        siz <- readMaybe $ BS.unpack size
        return $ HathFile h siz 0 0 typ
    _ -> Nothing

{-# INLINE parseHathFile #-}

data Cache
    = Cache { cacheLRUCounter :: {-# UNPACK #-} !Int
            , cacheS4         :: {-# UNPACK #-} !Text
            , cacheFileId     :: {-# UNPACK #-} !Text
            , cacheFileName   :: {-# UNPACK #-} !(Maybe Text)
            , cacheBytes      :: {-# UNPACK #-} !ByteString
            }
    deriving ( Generic )

instance FromRow Cache

instance ToRow Cache

verifyCache :: ( WithLog (Singleton (HathM m) Message) Message (HathM m), MonadIO m ) => HathM m ()
verifyCache = do
    logInfo "Verifying cache"
    srs <- staticRanges <$> getHathSettings
    res <- query_ [i|SELECT count(*) FROM files WHERE #{buildExpr srs}|]
    case res of
        [ Only cnt ] -> do
            logInfo [i|#{cnt::Int} records found not in static ranges|]
            when (cnt > 0) $ do
                logInfo "deleting"
                execute_ [i|DELETE FROM files WHERE #{buildExpr srs}|]
        _ -> pure ()
  where
                -- execute_ [i|"DELETE FROM files WHERE fileId = #{fid}|]

    buildExpr :: HashSet ByteString -> ByteString
    buildExpr srs
        = BS.intercalate " AND " $ map (\x -> [i|s4 NOT LIKE '#{x}%'|]) (HashSet.toList srs)

lookupCache :: MonadIO m => ByteString -> HathM m (Maybe ByteString)
lookupCache (decodeUtf8 @Text -> fileId)
    = query "SELECT * FROM files WHERE file_id = ?" (Only fileId) >>= \case
        []      -> return Nothing
        (c : _) -> do
            execute
                "UPDATE files SET lru_counter = lru_counter + 1 WHERE file_id = ?"
                (Only fileId)
            return $ Just $ cacheBytes c

{-# INLINE lookupCache #-}

updateFilenameIfMissing :: MonadIO m => ByteString -> ByteString -> HathM m ()
updateFilenameIfMissing (decodeUtf8 @Text -> fileId) (decodeUtf8 @Text -> filename)
    = execute
        "UPDATE files SET file_name = ? WHERE file_id = ? AND file_name IS NULL"
        ( filename, fileId )

{-# INLINE updateFilenameIfMissing #-}

storeCache :: MonadIO m => HathFile -> ByteString -> ByteString -> HathM m ()
storeCache hf bytes (decodeUtf8 @Text -> filename)
    = execute
        "INSERT INTO files (lru_counter, s4, file_id, file_name, bytes) VALUES (?, ?, ?, ?, ?)"
        (Cache { cacheLRUCounter = 1
               , cacheS4         = decodeUtf8 $ BS.take 4 $ fileHash hf
               , cacheFileId     = decodeUtf8 $ toFileId hf
               , cacheFileName   = Just filename
               , cacheBytes      = bytes
               })

{-# INLINE storeCache #-}