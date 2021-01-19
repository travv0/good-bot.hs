{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Lib (bigbot) where

import Control.Lens ((&), (.~), (^.))
import Control.Monad (filterM, when)
import Control.Monad.Reader (ReaderT (runReaderT), asks, forM_, liftIO)
import Data.Aeson (FromJSON (parseJSON), defaultOptions, eitherDecode, fieldLabelModifier, genericParseJSON, withObject, (.:))
import qualified Data.ByteString.Lazy as BSL
import Data.Char (toLower)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Data.Yaml (decodeFileEither)
import qualified Discord as D
import qualified Discord.Internal.Rest as D
import qualified Discord.Requests as D
import GHC.Generics (Generic)
import Network.Wreq (Response, defaults, get, getWith, header, param, responseBody)
import qualified Network.Wreq as W
import System.Random (Random (randomIO))

type App a = ReaderT Config IO a

data Config = Config
    { configDiscordHandle :: D.DiscordHandle
    , configDictKey :: Text
    , configUrbanKey :: Text
    , configCommandPrefix :: Text
    }

data UserConfig = UserConfig
    { userConfigDiscordToken :: Text
    , userConfigDictKey :: Text
    , userConfigUrbanKey :: Text
    , userConfigActivity :: Maybe Text
    , userConfigCommandPrefix :: Maybe Text
    }
    deriving (Generic, Show)

instance FromJSON UserConfig where
    parseJSON =
        genericParseJSON
            defaultOptions
                { fieldLabelModifier = stripUserConfigPrefix
                }

stripUserConfigPrefix :: String -> String
stripUserConfigPrefix s =
    case stripPrefix "userConfig" s of
        Just (c : rest) -> toLower c : rest
        _ -> s

bigbot :: IO ()
bigbot = do
    config@UserConfig{..} <-
        either (error . show) id <$> decodeFileEither "config.yaml"
    userFacingError <-
        D.runDiscord $
            D.def
                { D.discordToken = userConfigDiscordToken
                , D.discordOnStart = onStart config
                , D.discordOnEvent = eventHandler config
                , D.discordOnLog = T.putStrLn
                }
    T.putStrLn userFacingError

onStart :: UserConfig -> D.DiscordHandle -> IO ()
onStart config@UserConfig{userConfigActivity = mactivity} dis = do
    D.sendCommand dis $
        D.UpdateStatus $
            D.UpdateStatusOpts
                { D.updateStatusOptsSince = Nothing
                , D.updateStatusOptsGame = case mactivity of
                    Just activity -> Just $ D.Activity activity D.ActivityTypeGame Nothing
                    Nothing -> Nothing
                , D.updateStatusOptsNewStatus = D.UpdateStatusOnline
                , D.updateStatusOptsAFK = False
                }
    T.putStrLn $ "bot started with config " <> T.pack (show config)

eventHandler :: UserConfig -> D.DiscordHandle -> D.Event -> IO ()
eventHandler UserConfig{..} dis event = do
    let config =
            Config
                { configDiscordHandle = dis
                , configDictKey = userConfigDictKey
                , configUrbanKey = userConfigUrbanKey
                , configCommandPrefix = fromMaybe "!" userConfigCommandPrefix
                }
    flip runReaderT config $ case event of
        D.MessageCreate message -> messageCreate message
        D.TypingStart typingInfo -> typingStart typingInfo
        _ -> pure ()

type Command = D.Message -> App ()
type CommandPredicate = D.Message -> App Bool

commands :: [(CommandPredicate, Command)]
commands =
    [ (isCommand "rr", russianRoulette)
    , (isCommand "define", define)
    , (mentionsMe, respond)
    ]

messageCreate :: D.Message -> App ()
messageCreate message
    | not (fromBot message) = do
        matches <- filterM (\(p, _) -> p message) commands
        case matches of
            ((_, cmd) : _) -> cmd message
            _ -> pure ()
    | D.userId (D.messageAuthor message) == 235148962103951360 =
        simpleReply "Carl is a cuck" message
    | otherwise = pure ()

typingStart :: D.TypingInfo -> App ()
typingStart (D.TypingInfo userId channelId _utcTime) = do
    shouldReply <- liftIO $ (== 0) . (`mod` 1000) <$> (randomIO :: IO Int)
    when shouldReply $
        createMessage channelId $ T.pack $ "shut up <@" <> show userId <> ">"

restCall :: (FromJSON a, D.Request (r a)) => r a -> App ()
restCall request = do
    dis <- asks configDiscordHandle
    r <- liftIO $ D.restCall dis request
    case r of
        Right _ -> pure ()
        Left err -> liftIO $ print err

createMessage :: D.ChannelId -> Text -> App ()
createMessage channelId message = do
    let chunks = T.chunksOf 2000 message
    forM_ chunks $ \chunk -> restCall $ D.CreateMessage channelId chunk

createGuildBan :: D.GuildId -> D.UserId -> Text -> App ()
createGuildBan guildId userId banMessage =
    restCall $
        D.CreateGuildBan
            guildId
            userId
            (D.CreateGuildBanOpts Nothing (Just banMessage))

fromBot :: D.Message -> Bool
fromBot m = D.userIsBot (D.messageAuthor m)

russianRoulette :: Command
russianRoulette message = do
    chamber <- liftIO $ (`mod` 6) <$> (randomIO :: IO Int)
    case (chamber, D.messageGuild message) of
        (0, Just gId) -> do
            createMessage (D.messageChannel message) response
            createGuildBan gId (D.userId $ D.messageAuthor message) response
          where
            response = "Bang!"
        _ -> createMessage (D.messageChannel message) "Click."

data Definition = Definition
    { defPartOfSpeech :: Maybe Text
    , defDefinitions :: [Text]
    }
    deriving (Show)

instance FromJSON Definition where
    parseJSON = withObject "Definition" $ \v -> do
        partOfSpeech <- v .: "fl"
        definitions <- v .: "shortdef"
        pure Definition{defPartOfSpeech = partOfSpeech, defDefinitions = definitions}

define :: Command
define message = do
    let (_ : wordsToDefine) = words $ T.unpack $ D.messageText message
    case wordsToDefine of
        [] -> createMessage (D.messageChannel message) "Missing word/phrase to define"
        wtd -> do
            let phrase = unwords wtd
            moutput <- getDefineOutput phrase
            case moutput of
                Just output -> createMessage (D.messageChannel message) output
                Nothing ->
                    createMessage (D.messageChannel message) $
                        "No definition found for **" <> T.pack phrase <> "**"

buildDefineOutput :: String -> Definition -> Text
buildDefineOutput word definition = do
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

getDefineOutput :: String -> App (Maybe Text)
getDefineOutput word = do
    response <- getDictionaryResponse word
    buildDefineOutputHandleFail word (eitherDecode (response ^. responseBody)) $
        Just $ do
            urbanResponse <- getUrbanResponse word
            buildDefineOutputHandleFail word (decodeUrban (urbanResponse ^. responseBody)) Nothing

buildDefineOutputHandleFail :: String -> Either String [Definition] -> Maybe (App (Maybe Text)) -> App (Maybe Text)
buildDefineOutputHandleFail word (Right defs) _
    | not (null defs) =
        pure $
            Just $
                T.intercalate "\n\n" $
                    map (buildDefineOutput word) defs
buildDefineOutputHandleFail _ (Left err) Nothing = liftIO (print err) >> pure Nothing
buildDefineOutputHandleFail _ (Left _) (Just fallback) = fallback
buildDefineOutputHandleFail _ _ (Just fallback) = fallback
buildDefineOutputHandleFail _ (Right _) Nothing = pure Nothing

getDictionaryResponse :: String -> App (Response BSL.ByteString)
getDictionaryResponse word = do
    apiKey <- asks configDictKey
    liftIO $
        get $
            T.unpack $
                "https://dictionaryapi.com/api/v3/references/collegiate/json/"
                    <> T.pack word
                    <> "?key="
                    <> apiKey

getUrbanResponse :: String -> App (Response BSL.ByteString)
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
    deriving (Show)

instance FromJSON UrbanDefinition where
    parseJSON = withObject "UrbanDefinition" $ \v -> do
        list <- v .: "list"
        defs <- traverse (.: "definition") list
        pure UrbanDefinition{urbanDefDefinition = defs}

decodeUrban :: BSL.ByteString -> Either String [Definition]
decodeUrban = fmap urbanToDictionary . eitherDecode

urbanToDictionary :: UrbanDefinition -> [Definition]
urbanToDictionary (UrbanDefinition def) =
    [Definition Nothing def | not (null def)]

mentionsMe :: D.Message -> App Bool
mentionsMe message = do
    dis <- asks configDiscordHandle
    cache <- liftIO $ D.readCache dis
    pure $ D.userId (D._currentUser cache) `elem` map D.userId (D.messageMentions message)

respond :: Command
respond message
    | "thanks" `T.isInfixOf` T.toLower (D.messageText message)
        || "thank you" `T.isInfixOf` T.toLower (D.messageText message)
        || "thx" `T.isInfixOf` T.toLower (D.messageText message)
        || "thk" `T.isInfixOf` T.toLower (D.messageText message) =
        createMessage (D.messageChannel message) "u r welcome"
    | "hi" `T.isInfixOf` T.toLower (D.messageText message)
        || "hello" `T.isInfixOf` T.toLower (D.messageText message)
        || "yo" `T.isInfixOf` T.toLower (D.messageText message)
        || "sup" `T.isInfixOf` T.toLower (D.messageText message)
        || ( "what" `T.isInfixOf` T.toLower (D.messageText message)
                && "up" `T.isInfixOf` T.toLower (D.messageText message)
           )
        || "howdy" `T.isInfixOf` T.toLower (D.messageText message) =
        createMessage (D.messageChannel message) "hi"
    | "wb" `T.isInfixOf` T.toLower (D.messageText message)
        || "welcom" `T.isInfixOf` T.toLower (D.messageText message)
        || "welcum" `T.isInfixOf` T.toLower (D.messageText message) =
        createMessage (D.messageChannel message) "thx"
    | "mornin" `T.isInfixOf` T.toLower (D.messageText message)
        || "gm" `T.isInfixOf` T.toLower (D.messageText message) =
        createMessage (D.messageChannel message) "gm"
    | "night" `T.isInfixOf` T.toLower (D.messageText message)
        || "gn" `T.isInfixOf` T.toLower (D.messageText message) =
        createMessage (D.messageChannel message) "gn"
    | "how" `T.isInfixOf` T.toLower (D.messageText message)
        && ( " u" `T.isInfixOf` T.toLower (D.messageText message)
                || " you" `T.isInfixOf` T.toLower (D.messageText message)
           ) =
        createMessage (D.messageChannel message) "i am fine thank u and u?"
    | otherwise = do
        let responses = ["what u want", "stfu", "u r ugly", "i love u"]
        responseNum <- liftIO $ (`mod` length responses) <$> (randomIO :: IO Int)
        createMessage (D.messageChannel message) $ responses !! responseNum

isCommand :: Text -> CommandPredicate
isCommand command message = do
    prefix <- asks configCommandPrefix
    pure $ messageStartsWith (prefix <> command) message

messageStartsWith :: Text -> D.Message -> Bool
messageStartsWith text = (text `T.isPrefixOf`) . T.toLower . D.messageText

simpleReply :: Text -> Command
simpleReply replyText message =
    createMessage
        (D.messageChannel message)
        replyText
