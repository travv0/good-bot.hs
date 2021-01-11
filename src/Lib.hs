{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Control.Lens ((^.))
import Control.Monad (void, when)
import Control.Monad.Reader (MonadIO (..), MonadReader (ask), ReaderT (runReaderT), forM_)
import Data.Aeson (FromJSON, decode)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Discord as D
import qualified Discord.Requests as D
import qualified Discord.Types as D
import GHC.Generics (Generic)
import Network.Wreq (Response, get, responseBody)
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
    liftIO $ void $ D.restCall dis $ D.CreateMessage channelId message

createGuildBan ::
    (MonadReader D.DiscordHandle m, MonadIO m) =>
    D.GuildId ->
    D.UserId ->
    Text ->
    m ()
createGuildBan guildId userId banMessage = do
    dis <- ask
    liftIO $
        void $
            D.restCall dis $
                D.CreateGuildBan
                    guildId
                    userId
                    (D.CreateGuildBanOpts Nothing (Just banMessage))

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

data Definition = Definition {fl :: Text, shortdef :: [Text]}
    deriving (Generic, Show)

instance FromJSON Definition

define :: (MonadIO m, MonadReader D.DiscordHandle m) => D.Message -> m ()
define message = do
    apiKey <- liftIO dictionaryKey
    let (_ : wordsToDefine) = words $ T.unpack $ D.messageText message
    forM_ wordsToDefine $ \word -> do
        moutput <- getOutput apiKey word
        case moutput of
            Just output -> createMessage (D.messageChannel message) output
            Nothing ->
                createMessage (D.messageChannel message) $
                    "No definition found for **" <> T.pack word <> "**"

isDefine :: Text -> Bool
isDefine = ("!define " `T.isPrefixOf`) . T.toLower

buildOutput :: String -> Definition -> Text
buildOutput word definition = do
    let shortDefinition = shortdef definition
        partOfSpeech = fl definition
        definitions = case shortDefinition of
            [def] -> def
            defs ->
                T.intercalate "\n\n" $
                    zipWith
                        (\i def -> T.pack (show i) <> ". " <> def)
                        [1 :: Int ..]
                        defs
        formattedOutput =
            "**" <> T.pack word <> "** *" <> partOfSpeech <> "*\n" <> definitions
     in formattedOutput

getOutput :: MonadIO m => Text -> String -> m (Maybe Text)
getOutput apiKey word = do
    response <- getDictionaryResponse apiKey word
    case decode (response ^. responseBody) of
        Just defs -> return $ Just $ T.intercalate "\n\n" $ map (buildOutput word) defs
        Nothing -> return Nothing

getDictionaryResponse :: MonadIO m => Text -> String -> m (Response ByteString)
getDictionaryResponse apiKey word =
    liftIO $
        get $
            T.unpack $
                "https://dictionaryapi.com/api/v3/references/collegiate/json/"
                    <> T.pack word
                    <> "?key="
                    <> apiKey