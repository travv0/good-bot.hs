{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib (bigbot) where

import Control.Applicative (Alternative (empty))
import Control.Lens ((&), (.~), (^.))
import Control.Monad (when)
import Control.Monad.Reader (MonadIO (..), MonadReader, ReaderT (runReaderT), asks, forM_)
import Data.Aeson (FromJSON (parseJSON), Value (Object), eitherDecode, (.:))
import qualified Data.ByteString.Lazy as BSL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Discord as D
import qualified Discord.Internal.Rest as D
import qualified Discord.Requests as D
import GHC.Generics (Generic)
import Network.Wreq (Response, defaults, get, getWith, header, param, responseBody)
import qualified Network.Wreq as W
import System.Environment (getEnv)
import System.Random (Random (randomIO))

data Config = Config
    { configDiscordHandle :: D.DiscordHandle
    , configDictKey :: Text
    , configUrbanKey :: Text
    }

bigbot :: IO ()
bigbot = do
    token <- T.pack <$> getEnv "BIGBOT_TOKEN"
    dictKey <- T.pack <$> getEnv "BIGBOT_DICT_KEY"
    urbanKey <- T.pack <$> getEnv "BIGBOT_URBAN_KEY"
    userFacingError <-
        D.runDiscord $
            D.def
                { D.discordToken = token
                , D.discordOnEvent = eventHandler dictKey urbanKey
                }
    T.putStrLn userFacingError

eventHandler :: Text -> Text -> D.DiscordHandle -> D.Event -> IO ()
eventHandler dictKey urbanKey dis event = do
    let config =
            Config
                { configDiscordHandle = dis
                , configDictKey = dictKey
                , configUrbanKey = urbanKey
                }
    flip runReaderT config $ case event of
        D.MessageCreate message -> messageCreate message
        D.TypingStart typingInfo -> typingStart typingInfo
        _ -> pure ()

messageCreate :: (MonadIO m, MonadReader Config m) => D.Message -> m ()
messageCreate message
    | not (fromBot message) = do
        forMe <- mentionsMe message
        if
                | isRussianRoulette (D.messageText message) -> russianRoulette message
                | isDefine (D.messageText message) -> define message
                | forMe -> respond message
                | otherwise -> return ()
    | otherwise = return ()

typingStart :: (MonadIO m, MonadReader Config m) => D.TypingInfo -> m ()
typingStart (D.TypingInfo userId channelId _utcTime) = do
    shouldReply <- liftIO $ (== 0) . (`mod` 1000) <$> (randomIO :: IO Int)
    when shouldReply $
        createMessage channelId $ T.pack $ "shut up <@" <> show userId <> ">"

restCall :: (MonadReader Config m, MonadIO m, FromJSON a, D.Request (r a)) => r a -> m ()
restCall request = do
    dis <- asks configDiscordHandle
    r <- liftIO $ D.restCall dis request
    case r of
        Right _ -> return ()
        Left err -> liftIO $ print err

createMessage :: (MonadReader Config m, MonadIO m) => D.ChannelId -> Text -> m ()
createMessage channelId message = do
    let chunks = T.chunksOf 2000 message
    forM_ chunks $ \chunk -> restCall $ D.CreateMessage channelId chunk

createGuildBan ::
    (MonadReader Config m, MonadIO m) =>
    D.GuildId ->
    D.UserId ->
    Text ->
    m ()
createGuildBan guildId userId banMessage =
    restCall $
        D.CreateGuildBan
            guildId
            userId
            (D.CreateGuildBanOpts Nothing (Just banMessage))

fromBot :: D.Message -> Bool
fromBot m = D.userIsBot (D.messageAuthor m)

russianRoulette :: (MonadIO m, MonadReader Config m) => D.Message -> m ()
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

define :: (MonadIO m, MonadReader Config m) => D.Message -> m ()
define message = do
    let (_ : wordsToDefine) = words $ T.unpack $ D.messageText message
    forM_ wordsToDefine $ \word -> do
        moutput <- getOutput word
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

getOutput :: (MonadIO m, MonadReader Config m) => String -> m (Maybe Text)
getOutput word = do
    response <- getDictionaryResponse word
    buildOutputHandleFail word (eitherDecode (response ^. responseBody)) $
        Just $ do
            urbanResponse <- getUrbanResponse word
            buildOutputHandleFail word (decodeUrban (urbanResponse ^. responseBody)) Nothing

buildOutputHandleFail :: MonadIO m => String -> Either String [Definition] -> Maybe (m (Maybe Text)) -> m (Maybe Text)
buildOutputHandleFail word (Right defs) _
    | not (null defs) =
        return $
            Just $
                T.intercalate "\n\n" $
                    map (buildOutput word) defs
buildOutputHandleFail _ (Left err) Nothing = liftIO (print err) >> return Nothing
buildOutputHandleFail _ (Left _) (Just fallback) = fallback
buildOutputHandleFail _ _ (Just fallback) = fallback
buildOutputHandleFail _ (Right _) Nothing = return Nothing

getDictionaryResponse :: (MonadIO m, MonadReader Config m) => String -> m (Response BSL.ByteString)
getDictionaryResponse word = do
    apiKey <- asks configDictKey
    liftIO $
        get $
            T.unpack $
                "https://dictionaryapi.com/api/v3/references/collegiate/json/"
                    <> T.pack word
                    <> "?key="
                    <> apiKey

getUrbanResponse :: (MonadIO m, MonadReader Config m) => String -> m (Response BSL.ByteString)
getUrbanResponse word = do
    apiKey <- asks configUrbanKey
    liftIO $
        getWith
            (urbanOpts apiKey word)
            "https://mashape-community-urban-dictionary.p.rapidapi.com/define"

urbanOpts :: Text -> String -> W.Options
urbanOpts apiKey term =
    defaults
        & header "x-rapidapi-key" .~ [T.encodeUtf8 apiKey]
        & header "x-rapidapi-host" .~ ["mashape-community-urban-dictionary.p.rapidapi.com"]
        & header "useQueryString" .~ ["true"]
        & param "term" .~ [T.pack term]

newtype UrbanDefinition = UrbanDefinition {urbanDefDefinition :: [Text]}
    deriving (Generic, Show)

instance FromJSON UrbanDefinition where
    parseJSON (Object v) = do
        list <- v .: "list"
        defs <- traverse (.: "definition") list
        return UrbanDefinition{urbanDefDefinition = defs}
    parseJSON _ = empty

decodeUrban :: BSL.ByteString -> Either String [Definition]
decodeUrban = fmap urbanToDictionary . eitherDecode

urbanToDictionary :: UrbanDefinition -> [Definition]
urbanToDictionary (UrbanDefinition def) =
    [Definition Nothing def | not (null def)]

mentionsMe :: (MonadReader Config m, MonadIO m) => D.Message -> m Bool
mentionsMe message = do
    dis <- asks configDiscordHandle
    cache <- liftIO $ D.readCache dis
    return $ D.userId (D._currentUser cache) `elem` map D.userId (D.messageMentions message)

respond :: (MonadReader Config m, MonadIO m) => D.Message -> m ()
respond message
    | "thanks" `T.isInfixOf` T.toLower (D.messageText message)
        || "thank you" `T.isInfixOf` T.toLower (D.messageText message)
        || "thx" `T.isInfixOf` T.toLower (D.messageText message)
        || "thk" `T.isInfixOf` T.toLower (D.messageText message) =
        createMessage (D.messageChannel message) "u r welcome"
    | "hi" `T.isInfixOf` T.toLower (D.messageText message)
        || "hello" `T.isInfixOf` T.toLower (D.messageText message)
        || "sup" `T.isInfixOf` T.toLower (D.messageText message)
        || "what" `T.isInfixOf` T.toLower (D.messageText message) && "up" `T.isInfixOf` T.toLower (D.messageText message)
        || "howdy" `T.isInfixOf` T.toLower (D.messageText message) =
        createMessage (D.messageChannel message) "hi"
    | "wb" `T.isInfixOf` T.toLower (D.messageText message)
        || "welcom" `T.isInfixOf` T.toLower (D.messageText message)
        || "welcum" `T.isInfixOf` T.toLower (D.messageText message) =
        createMessage (D.messageChannel message) "thx"
    | otherwise = do
        let responses = ["what u want", "stfu"] :: [Text]
        responseNum <- liftIO $ (`mod` length responses) <$> (randomIO :: IO Int)
        createMessage (D.messageChannel message) $ responses !! responseNum