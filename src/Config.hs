{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Config
    ( Config(..)
    , UserConfig(..)
    , defaultConfigFile
    , defaultDbFile
    , loadConfig
    ) where

import           Control.Concurrent.STM         ( TVar )
import           Data.Aeson                     ( FromJSON
                                                , defaultOptions
                                                , fieldLabelModifier
                                                , genericParseJSON
                                                , parseJSON
                                                )
import           Data.List                      ( stripPrefix )
import           Data.Char                      ( toLower )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Yaml                      ( decodeFileEither )
import           Database                       ( Db )
import           GHC.Generics                   ( Generic )
import           RateLimit                      ( RateLimiter )
import           System.Environment             ( lookupEnv )

data Config = Config
    { configDictKey       :: Maybe Text
    , configUrbanKey      :: Maybe Text
    , configCommandPrefix :: Text
    , configDb            :: TVar Db
    , configDbFile        :: FilePath
    , configRateLimiter   :: RateLimiter
    }

data UserConfig = UserConfig
    { userConfigDiscordToken  :: Text
    , userConfigDictKey       :: Maybe Text
    , userConfigUrbanKey      :: Maybe Text
    , userConfigCommandPrefix :: Maybe Text
    , userConfigDbFile        :: Maybe FilePath
    }
    deriving (Generic, Show)

instance FromJSON UserConfig where
    parseJSON = genericParseJSON defaultOptions
        { fieldLabelModifier = stripJSONPrefix "userConfig"
        }

stripJSONPrefix :: String -> String -> String
stripJSONPrefix prefix s = case stripPrefix prefix s of
    Just (c : rest) -> toLower c : rest
    _               -> s

defaultConfigFile :: FilePath
defaultConfigFile = "config.yaml"

defaultDbFile :: FilePath
defaultDbFile = "db"

loadConfig :: FilePath -> IO (Either String UserConfig)
loadConfig configFile = do
    result <- decodeFileEither configFile
    case result of
        Left err -> pure $ Left $ "Failed to load config from " ++ configFile ++ ": " ++ show err
        Right config -> Right <$> applyEnvOverrides config

applyEnvOverrides :: UserConfig -> IO UserConfig
applyEnvOverrides config = do
    -- Override with environment variables if they exist
    envToken <- lookupEnv "DISCORD_TOKEN"
    envDictKey <- lookupEnv "DICTIONARY_API_KEY"
    envUrbanKey <- lookupEnv "URBAN_API_KEY"
    envPrefix <- lookupEnv "COMMAND_PREFIX"
    envDbFile <- lookupEnv "DB_FILE"
    
    pure config
        { userConfigDiscordToken = maybe (userConfigDiscordToken config) T.pack envToken
        , userConfigDictKey = maybe (userConfigDictKey config) (Just . T.pack) envDictKey
        , userConfigUrbanKey = maybe (userConfigUrbanKey config) (Just . T.pack) envUrbanKey
        , userConfigCommandPrefix = maybe (userConfigCommandPrefix config) (Just . T.pack) envPrefix
        , userConfigDbFile = maybe (userConfigDbFile config) Just envDbFile
        }