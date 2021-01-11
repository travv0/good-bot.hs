{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Control.Applicative (Alternative (empty))
import Control.Lens ((&), (.~), (^.))
import Control.Monad (when)
import Control.Monad.Reader (MonadIO (..), MonadReader (ask), ReaderT (runReaderT), forM_)
import Data.Aeson (FromJSON (parseJSON), Value (Object), eitherDecode, (.:))
import qualified Data.ByteString.Lazy as BSL
import Data.Text (Text, chunksOf)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.IO as T
import qualified Discord as D
import qualified Discord.Requests as D
import qualified Discord.Types as D
import GHC.Generics (Generic)
import Network.Wreq (Response, defaults, get, getWith, header, param, responseBody)
import qualified Network.Wreq as W
import System.Environment (getEnv)
import System.Random (Random (randomIO))

bigbot :: IO ()
bigbot = do
    token <- T.pack <$> getEnv "BIGBOT_TOKEN"
    userFacingError <-
        D.runDiscord $
            D.def{D.discordToken = token, D.discordOnEvent = eventHandler}
    T.putStrLn userFacingError

eventHandler :: D.DiscordHandle -> D.Event -> IO ()
eventHandler dis event = flip runReaderT dis $ case event of
    D.MessageCreate message -> messageCreate message
    D.TypingStart typingInfo -> typingStart typingInfo
    _ -> pure ()

messageCreate :: (MonadIO m, MonadReader D.DiscordHandle m) => D.Message -> m ()
messageCreate message
    | not (fromBot message) && isRussianRoulette (D.messageText message) = russianRoulette message
    | not (fromBot message) && isDefine (D.messageText message) = define message
    | otherwise = return ()

typingStart :: (MonadIO m, MonadReader D.DiscordHandle m) => D.TypingInfo -> m ()
typingStart (D.TypingInfo userId channelId _utcTime) = do
    shouldReply <- liftIO $ (== 0) . (`mod` 1000) <$> (randomIO :: IO Int)
    when shouldReply $
        createMessage channelId $ T.pack $ "shut up <@" <> show userId <> ">"

createMessage :: (MonadReader D.DiscordHandle m, MonadIO m) => D.ChannelId -> Text -> m ()
createMessage channelId message = do
    dis <- ask
    let chunks = chunksOf 2000 message
    forM_ chunks $ \chunk -> do
        r <- liftIO $ D.restCall dis $ D.CreateMessage channelId chunk
        case r of
            Right _ -> return ()
            Left err -> liftIO $ print err

createGuildBan ::
    (MonadReader D.DiscordHandle m, MonadIO m) =>
    D.GuildId ->
    D.UserId ->
    Text ->
    m ()
createGuildBan guildId userId banMessage = do
    dis <- ask
    r <-
        liftIO $
            D.restCall dis $
                D.CreateGuildBan
                    guildId
                    userId
                    (D.CreateGuildBanOpts Nothing (Just banMessage))
    case r of
        Right _ -> return ()
        Left err -> liftIO $ print err

fromBot :: D.Message -> Bool
fromBot m = D.userIsBot (D.messageAuthor m)

russianRoulette :: (MonadIO m, MonadReader D.DiscordHandle m) => D.Message -> m ()
russianRoulette message = do
    chamber <- liftIO $ (`mod` 6) <$> (randomIO :: IO Int)
    case (chamber, D.messageGuild message) of
        (0, Just gId) -> do
            createMessage (D.messageChannel message) response
            createGuildBan gId (D.userId $ D.messageAuthor message) response
          where
            response = "Bang!"
        _ -> createMessage (D.messageChannel message) "Click."

isRussianRoulette :: Text -> Bool
isRussianRoulette = ("!rr" `T.isPrefixOf`) . T.toLower

dictionaryKey :: IO Text
dictionaryKey = T.pack <$> getEnv "BIGBOT_DICT_KEY"

urbanKey :: IO Text
urbanKey = T.pack <$> getEnv "BIGBOT_URBAN_KEY"

data Definition = Definition
    { defPartOfSpeech :: Maybe Text
    , defDefinitions :: [Text]
    }
    deriving (Generic, Show)

instance FromJSON Definition where
    parseJSON (Object v) = do
        partOfSpeech <- v .: "fl"
        definitions <- v .: "shortdef"
        return Definition{defPartOfSpeech = partOfSpeech, defDefinitions = definitions}
    parseJSON _ = empty

define :: (MonadIO m, MonadReader D.DiscordHandle m) => D.Message -> m ()
define message = do
    apiKey <- liftIO dictionaryKey
    urbanApiKey <- liftIO urbanKey
    let (_ : wordsToDefine) = words $ T.unpack $ D.messageText message
    forM_ wordsToDefine $ \word -> do
        moutput <- getOutput apiKey urbanApiKey word
        case moutput of
            Just output -> createMessage (D.messageChannel message) output
            Nothing ->
                createMessage (D.messageChannel message) $
                    "No definition found for **" <> T.pack word <> "**"

isDefine :: Text -> Bool
isDefine = ("!define " `T.isPrefixOf`) . T.toLower

buildOutput :: String -> Definition -> Text
buildOutput word definition = do
    let shortDefinition = defDefinitions definition
        mpartOfSpeech = defPartOfSpeech definition
        definitions = case shortDefinition of
            [def] -> def
            defs ->
                T.intercalate "\n\n" $
                    zipWith
                        (\i def -> T.pack (show i) <> ". " <> def)
                        [1 :: Int ..]
                        defs
        formattedOutput =
            "**" <> T.pack word <> "**"
                <> ( case mpartOfSpeech of
                        Just partOfSpeech -> " *" <> partOfSpeech <> "*"
                        Nothing -> ""
                   )
                <> "\n"
                <> definitions
     in formattedOutput

getOutput :: MonadIO m => Text -> Text -> String -> m (Maybe Text)
getOutput apiKey urbanApiKey word = do
    response <- getDictionaryResponse apiKey word
    urbanResponse <- getUrbanResponse urbanApiKey word
    case eitherDecode (response ^. responseBody) <> decodeUrban (urbanResponse ^. responseBody) of
        Right defs
            | not (null defs) ->
                return $
                    Just $
                        T.intercalate "\n\n" $
                            map (buildOutput word) defs
        Left err -> liftIO (print err) >> return Nothing
        _ -> return Nothing

getDictionaryResponse :: MonadIO m => Text -> String -> m (Response BSL.ByteString)
getDictionaryResponse apiKey word =
    liftIO $
        get $
            T.unpack $
                "https://dictionaryapi.com/api/v3/references/collegiate/json/"
                    <> T.pack word
                    <> "?key="
                    <> apiKey

getUrbanResponse :: MonadIO m => Text -> String -> m (Response BSL.ByteString)
getUrbanResponse apiKey word =
    liftIO $
        getWith
            (urbanOpts apiKey word)
            "https://mashape-community-urban-dictionary.p.rapidapi.com/define"

urbanOpts :: Text -> String -> W.Options
urbanOpts apiKey term =
    defaults
        & header "x-rapidapi-key" .~ [encodeUtf8 apiKey]
        & header "x-rapidapi-host" .~ ["mashape-community-urban-dictionary.p.rapidapi.com"]
        & header "useQueryString" .~ ["true"]
        & param "term" .~ [T.pack term]

newtype UrbanDefinition = UrbanDefinition {urbanDefDefinition :: [Text]}
    deriving (Generic, Show)

instance FromJSON UrbanDefinition where
    parseJSON (Object v) = do
        list <- v .: "list"
        defs <- mapM (.: "definition") list
        return UrbanDefinition{urbanDefDefinition = defs}
    parseJSON _ = empty

decodeUrban :: BSL.ByteString -> Either String [Definition]
decodeUrban = fmap urbanToDictionary . eitherDecode

urbanToDictionary :: UrbanDefinition -> [Definition]
urbanToDictionary (UrbanDefinition def) =
    [Definition Nothing def | not (null def)]