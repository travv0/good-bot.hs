{-# LANGUAGE NamedFieldPuns #-}

module Database
    ( Db(..)
    , defaultDb
    , updateDb
    , loadDb
    , saveDb
    ) where

import           Control.Concurrent.STM         ( TVar
                                                , atomically
                                                , modifyTVar'
                                                , readTVar
                                                )
import           Control.Monad.Catch            ( catchIOError )
import           Data.Maybe                     ( fromMaybe )
import           Data.Text                      ( Text )
import qualified Discord                       as D
import qualified Discord.Types                 as D
import           Text.Read                      ( readMaybe )
import           Types                          ( MeannessLevel
                                                , mkMeannessLevel
                                                )

data Db = Db
    { dbResponses :: [Text]
    , dbActivity  :: Maybe (D.ActivityType, Text)
    , dbMeanness  :: MeannessLevel
    }
    deriving (Show, Read)

defaultDb :: Db
defaultDb = Db { dbResponses = ["hi"], dbActivity = Nothing, dbMeanness = mkMeannessLevel 5 }

loadDb :: FilePath -> IO Db
loadDb dbFile = do
    result <- (Right <$> readFile dbFile) `catchIOError` \e -> 
        pure $ Left $ "Failed to read database file: " ++ show e
    case result of
        Left _ -> pure defaultDb  -- Use default if file doesn't exist
        Right content -> case readMaybe content of
            Just db -> pure db
            Nothing -> do
                putStrLn $ "Warning: Database file is corrupted, using default database"
                pure defaultDb

saveDb :: FilePath -> Db -> IO (Either String ())
saveDb dbFile db = do
    result <- (Right <$> writeFile dbFile (show db)) `catchIOError` \e ->
        pure $ Left $ "Failed to save database: " ++ show e
    pure result

updateDb :: TVar Db -> FilePath -> (Db -> Db) -> IO ()
updateDb dbRef dbFileName f = do
    db <- atomically $ do
        modifyTVar' dbRef f
        readTVar dbRef
    result <- saveDb dbFileName db
    case result of
        Left err -> putStrLn err  -- Log error but don't crash
        Right () -> pure ()