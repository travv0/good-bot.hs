{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
    ( goodbot
    ) where

import           Control.Concurrent.STM         ( TVar
                                                , atomically
                                                , modifyTVar'
                                                , newTVarIO
                                                , readTVar
                                                , readTVarIO
                                                )
import           Control.Lens                   ( (&)
                                                , (.~)
                                                , view
                                                )
import           Control.Monad                  ( filterM
                                                , when
                                                )
import           Control.Monad.Catch            ( catchAll
                                                , catchIOError
                                                )
import           Control.Monad.Reader           ( MonadTrans(lift)
                                                , ReaderT(runReaderT)
                                                , asks
                                                , liftIO
                                                )
import           Data.Aeson                     ( (.:)
                                                , FromJSON(parseJSON)
                                                , defaultOptions
                                                , eitherDecode
                                                , fieldLabelModifier
                                                , genericParseJSON
                                                , withObject
                                                )
import qualified Data.ByteString.Lazy          as BSL
import           Data.Char                      ( isSpace
                                                , toLower
                                                )
import           Data.Foldable                  ( find
                                                , foldl'
                                                , for_
                                                )
import           Data.List                      ( delete
                                                , nub
                                                , stripPrefix
                                                )
import           Data.Maybe                     ( fromMaybe )
import           Data.Text                      ( Text
                                                , intercalate
                                                )
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import qualified Data.Text.IO                  as T
import           Data.Time                      ( defaultTimeLocale
                                                , formatTime
                                                , getCurrentTime
                                                , getCurrentTimeZone
                                                , utcToLocalTime
                                                )
import           Data.Yaml                      ( decodeFileEither )
import qualified Discord                       as D
import qualified Discord.Internal.Rest         as D
import qualified Discord.Requests              as D
import           GHC.Generics                   ( Generic )
import           Network.Wreq                   ( Response
                                                , defaults
                                                , get
                                                , getWith
                                                , header
                                                , param
                                                , responseBody
                                                )
import qualified Network.Wreq                  as W
import           System.Environment             ( getArgs )
import           System.Random                  ( randomIO )
import           Text.Read                      ( readMaybe )

type App a = ReaderT Config D.DiscordHandler a

data Db = Db
    { dbResponses :: [Text]
    , dbActivity  :: Maybe (D.ActivityType, Text)
    , dbMeanness  :: Int
    }
    deriving (Show, Read)

defaultDb :: Db
defaultDb = Db { dbResponses = ["hi"], dbActivity = Nothing, dbMeanness = 5 }

data Config = Config
    { configDictKey       :: Maybe Text
    , configUrbanKey      :: Maybe Text
    , configCommandPrefix :: Text
    , configDb            :: TVar Db
    , configDbFile        :: FilePath
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

logText :: Text -> IO ()
logText t = do
    now <- getCurrentTime
    tz  <- getCurrentTimeZone
    T.putStrLn
        $  T.pack (formatTime defaultTimeLocale "%F %T" $ utcToLocalTime tz now)
        <> ": "
        <> t

logError :: Text -> IO ()
logError t = logText $ "Error: " <> t

goodbot :: IO ()
goodbot = do
    args <- getArgs
    let configFile = case args of
            []     -> defaultConfigFile
            [path] -> path
            _      -> error "too many arguments provided: expected at most 1"
    config@UserConfig {..} <- either (error . show) id
        <$> decodeFileEither configFile
    dbStr <- (Just <$> readFile (fromMaybe defaultDbFile userConfigDbFile))
        `catchIOError` \_ -> pure Nothing
    let db = fromMaybe defaultDb $ dbStr >>= readMaybe
    dbRef           <- newTVarIO db
    userFacingError <-
        D.runDiscord
                (D.def { D.discordToken   = userConfigDiscordToken
                       , D.discordOnStart = onStart config
                       , D.discordOnEvent = eventHandler config dbRef
                       , D.discordOnLog   = logText
                       }
                )
            `catchAll` \e -> pure $ T.pack $ show e
    T.putStrLn userFacingError

onStart :: UserConfig -> D.DiscordHandler ()
onStart config =
    liftIO $ logText $ "bot started with config " <> T.pack (show config)

updateStatus :: D.ActivityType -> Maybe Text -> D.DiscordHandler ()
updateStatus activityType mactivity =
    D.sendCommand $ D.UpdateStatus $ D.UpdateStatusOpts
        { D.updateStatusOptsSince     = Nothing
        , D.updateStatusOptsGame      = case mactivity of
            Just activity -> Just $ D.Activity activity activityType Nothing
            Nothing       -> Nothing
        , D.updateStatusOptsNewStatus = D.UpdateStatusOnline
        , D.updateStatusOptsAFK       = False
        }

eventHandler :: UserConfig -> TVar Db -> D.Event -> D.DiscordHandler ()
eventHandler UserConfig {..} dbRef event =
    let config = Config
            { configDictKey       = userConfigDictKey
            , configUrbanKey      = userConfigUrbanKey
            , configCommandPrefix = fromMaybe "!" userConfigCommandPrefix
            , configDb            = dbRef
            , configDbFile        = fromMaybe defaultDbFile userConfigDbFile
            }
    in  flip runReaderT config $ case event of
            D.Ready{}                  -> ready dbRef
            D.MessageCreate message    -> messageCreate message
            D.TypingStart   typingInfo -> typingStart typingInfo
            _                          -> pure ()

ready :: TVar Db -> App ()
ready dbRef = do
    Db { dbActivity = mactivity } <- liftIO $ readTVarIO dbRef
    case mactivity of
        Just (activityType, activity) ->
            lift $ updateStatus activityType $ Just activity
        Nothing -> pure ()

data CommandArg = String Text | IntMaybe (Maybe Int)
data ArgType = Required | Optional | Multi
type CommandFunc = D.Message -> App ()
type Predicate = D.Message -> App Bool
type CommandArgInfo = [(ArgType, Text, Text, CommandParser)]
type CommandParser = Text -> Either Text (CommandArg, Text)

restStr :: CommandParser
restStr "" = Left "No remaining args"
restStr s  = Right (String s, "")

intMaybe :: CommandParser
intMaybe s = case s of
    "" -> Right (IntMaybe Nothing, s)
    _  -> case reads $ T.unpack s of
        []         -> Left $ "Couldn't parse int: " <> s
        (n, r) : _ -> Right (IntMaybe (Just n), T.pack r)

parseArgs :: Command -> D.Message -> App (Either Text [CommandArg])
parseArgs command message = do
    msg <- stripCommand message
    return $ reverse . fst <$> foldl'
        (\acc (_, _, _, parse) -> case acc of
            Right (args, text) ->
                (case parse text of
                    Right (arg, newText) -> Right (arg : args, newText)
                    Left  e              -> Left e
                )
            e -> e
        )
        (Right (([], ) $ fromMaybe "" msg))
        (commandArgInfo command)

commandName :: Command -> Text
commandName RR          = "rr"
commandName Define      = "define"
commandName Urban       = "urban"
commandName Add         = "add"
commandName Remove      = "remove"
commandName List        = "list"
commandName Playing     = "playing"
commandName ListeningTo = "listeningto"
commandName CompetingIn = "competingin"
commandName Meanness    = "meanness"
commandName Help        = "help"

commandArgInfo :: Command -> CommandArgInfo
commandArgInfo RR = []
commandArgInfo Define =
    [(Multi, "term", "The word or phrase to look up.", restStr)]
commandArgInfo Urban =
    [(Multi, "term", "The word or phrase to look up.", restStr)]
commandArgInfo Add = [(Multi, "response", "The response to add.", restStr)]
commandArgInfo Remove =
    [(Multi, "response", "The response to remove.", restStr)]
commandArgInfo List    = []
commandArgInfo Playing = [(Multi, "name", "What's the bot playing?", restStr)]
commandArgInfo ListeningTo =
    [(Multi, "name", "What's the bot listening to?", restStr)]
commandArgInfo CompetingIn =
    [(Multi, "name", "What's the bot competing in?", restStr)]
commandArgInfo Meanness =
    [ ( Optional
      , "level"
      , "The number between 0 and 10 to set the bot's meanness to. Higher is meaner. Leave blank to view current meanness."
      , intMaybe
      )
    ]
commandArgInfo Help =
    [(Optional, "command", "Command to show help for.", restStr)]

commandHelpText :: Command -> Text
commandHelpText RR = "Play Russian Roulette!"
commandHelpText Define =
    "Look up the definition of a word or phrase, using Urban Dictionary as a backup."
commandHelpText Urban =
    "Look up the definition of a word or phrase on Urban Dictionary."
commandHelpText Add =
    "Add a response to be randomly selected when the bot replies after being pinged."
commandHelpText Remove      = "Remove a response from the bot's response pool."
commandHelpText List        = "List all responses in the response pool."
commandHelpText Playing     = "Set bot's activity to Playing."
commandHelpText ListeningTo = "Set bot's activity to Listening To."
commandHelpText CompetingIn = "Set bot's activity to Competing In."
commandHelpText Meanness
    = "Set bot's meanness from 0-10 or display current meanness if no argument given."
commandHelpText Help =
    "Show this help or show detailed help for a given command."

commandFunc :: Command -> CommandFunc
commandFunc RR          = russianRoulette
commandFunc Define      = define getDefineOutput
commandFunc Urban       = define getUrbanOutput
commandFunc Add         = addResponse
commandFunc Remove      = removeResponse
commandFunc List        = listResponses
commandFunc Playing     = setActivity D.ActivityTypeGame
commandFunc ListeningTo = setActivity D.ActivityTypeListening
commandFunc CompetingIn = setActivity D.ActivityTypeCompeting
commandFunc Meanness    = setMeanness
commandFunc Help        = showHelp

data Command =
      RR
    | Define
    | Urban
    | Add
    | Remove
    | List
    | Playing
    | ListeningTo
    | CompetingIn
    | Meanness
    | Help
    deriving (Show, Eq, Enum, Bounded)

commands :: [Command]
commands = [minBound .. maxBound]

predicates :: [(Predicate, CommandFunc)]
predicates =
    [ (isCarl, simpleReply "Carl is a cuck")
    , ( mentionsMe ||| messageContains "@everyone" ||| messageContains "@here"
      , respond
      )
    ]

messageCreate :: D.Message -> App ()
messageCreate message = do
    self <- isSelf message
    if self
        then pure ()
        else do
            commandMatches <- filterM
                (\command -> isCommand (commandName command) message)
                commands
            case commandMatches of
                (command : _) -> commandFunc command message
                _             -> do
                    predicateMatches <- filterM (\(p, _) -> p message)
                                                predicates
                    case predicateMatches of
                        ((_, c) : _) -> c message
                        _            -> pure ()

isSelf :: Predicate
isSelf message = do
    cache <- lift D.readCache
    pure $ D.userId (D.cacheCurrentUser cache) == D.userId
        (D.messageAuthor message)

isUser :: D.UserId -> Predicate
isUser userId message = pure $ D.userId (D.messageAuthor message) == userId

isCarl :: Predicate
isCarl = isUser 235148962103951360

typingStart :: D.TypingInfo -> App ()
typingStart (D.TypingInfo userId channelId _utcTime) = do
    db       <- asks configDb
    meanness <- liftIO $ dbMeanness <$> readTVarIO db
    when (meanness > 0) $ do
        shouldReply <-
            liftIO
            $   (== 0)
            .   (`mod` meannessRatio meanness)
            <$> (randomIO :: IO Int)
        when shouldReply
            $  createMessage channelId Nothing
            $  T.pack
            $  "shut up <@"
            <> show userId
            <> ">"
  where
    meannessRatio 11 = 1
    meannessRatio n  = 2000 `div` n

restCall :: (FromJSON a, D.Request (r a)) => r a -> App ()
restCall request = do
    r <- lift $ D.restCall request
    case r of
        Right _   -> pure ()
        Left  err -> liftIO $ logError $ T.pack $ show err

replyTo :: D.Message -> Text -> App ()
replyTo replyingTo =
    createMessage (D.messageChannel replyingTo) (Just $ D.messageId replyingTo)

createMessage :: D.ChannelId -> Maybe D.MessageId -> Text -> App ()
createMessage channelId replyingToId message =
    let chunks = T.chunksOf 2000 message
    in
        for_ chunks $ \chunk -> restCall $ D.CreateMessageDetailed
            channelId
            D.def
                { D.messageDetailedContent         = chunk
                , D.messageDetailedAllowedMentions = Just D.def
                    { D.mentionRepliedUser = False
                    }
                , D.messageDetailedReference       = fmap
                    (\mId -> D.def { D.referenceMessageId = Just mId })
                    replyingToId
                }

createGuildBan :: D.GuildId -> D.UserId -> Text -> App ()
createGuildBan guildId userId banMessage = restCall $ D.CreateGuildBan
    guildId
    userId
    (D.CreateGuildBanOpts Nothing (Just banMessage))

fromBot :: D.Message -> Bool
fromBot m = D.userIsBot (D.messageAuthor m)

russianRoulette :: CommandFunc
russianRoulette message = do
    chamber <- liftIO $ (`mod` 6) <$> (randomIO :: IO Int)
    case (chamber, D.messageGuild message) of
        (0, Just gId) -> do
            replyTo message response
            createGuildBan gId (D.userId $ D.messageAuthor message) response
            where response = "Bang!"
        _ -> replyTo message "Click."

data Definition = Definition
    { defPartOfSpeech :: Maybe Text
    , defDefinitions  :: [Text]
    }
    deriving Show

instance FromJSON Definition where
    parseJSON = withObject "Definition" $ \v -> do
        partOfSpeech <- v .: "fl"
        definitions  <- v .: "shortdef"
        pure Definition { defPartOfSpeech = partOfSpeech
                        , defDefinitions  = definitions
                        }

stripCommand :: D.Message -> App (Maybe Text)
stripCommand message = do
    prefix <- asks configCommandPrefix
    case T.stripPrefix prefix $ D.messageText message of
        Nothing -> pure Nothing
        Just withoutPrefix ->
            let withoutCommand = T.dropWhile isSpace
                    $ T.dropWhile (not . isSpace) withoutPrefix
            in  case withoutCommand of
                    "" -> pure Nothing
                    m  -> pure $ Just m

define :: (Text -> App (Maybe Text)) -> CommandFunc
define getOutput message = do
    args <- parseArgs Define message
    case args of
        Right [String phrase] -> do
            moutput <- getOutput phrase
            case moutput of
                Just output -> replyTo message output
                Nothing ->
                    replyTo message
                        $  "No definition found for **"
                        <> phrase
                        <> "**"
        _ -> replyTo message "Missing word/phrase to define"

buildDefineOutput :: Text -> Definition -> Text
buildDefineOutput word definition =
    let definitions = case defDefinitions definition of
            [def] -> def
            defs  -> T.intercalate "\n\n" $ zipWith
                (\i def -> T.pack (show i) <> ". " <> def)
                [1 :: Int ..]
                defs
    in  "**"
            <> word
            <> "**"
            <> (case defPartOfSpeech definition of
                   Just partOfSpeech -> " *" <> partOfSpeech <> "*"
                   Nothing           -> ""
               )
            <> "\n"
            <> definitions

getDefineOutput :: Text -> App (Maybe Text)
getDefineOutput word = do
    response <- getDictionaryResponse word
    buildDefineOutputHandleFail
            word
            (response >>= eitherDecode . view responseBody)
        $ Just
        $ getUrbanOutput word

getUrbanOutput :: Text -> App (Maybe Text)
getUrbanOutput word = do
    urbanResponse <- getUrbanResponse word
    buildDefineOutputHandleFail
        word
        (urbanResponse >>= decodeUrban . view responseBody)
        Nothing

buildDefineOutputHandleFail
    :: Text
    -> Either String [Definition]
    -> Maybe (App (Maybe Text))
    -> App (Maybe Text)
buildDefineOutputHandleFail word (Right defs) _ | not (null defs) =
    pure $ Just $ T.intercalate "\n\n" $ map (buildDefineOutput word) defs
buildDefineOutputHandleFail _ (Left err) Nothing =
    liftIO (logError $ T.pack err) >> pure Nothing
buildDefineOutputHandleFail _ (Left err) (Just fallback) =
    liftIO (logError $ T.pack err) >> fallback
buildDefineOutputHandleFail _ _         (Just fallback) = fallback
buildDefineOutputHandleFail _ (Right _) Nothing         = pure Nothing

getDictionaryResponse :: Text -> App (Either String (Response BSL.ByteString))
getDictionaryResponse word = do
    mapiKey <- asks configDictKey
    case mapiKey of
        Nothing -> pure $ Left "no dictionary.com api key set"
        Just apiKey ->
            liftIO
                $   fmap Right
                <$> get
                $   T.unpack
                $ "https://dictionaryapi.com/api/v3/references/collegiate/json/"
                <>  word
                <>  "?key="
                <>  apiKey

getUrbanResponse :: Text -> App (Either String (Response BSL.ByteString))
getUrbanResponse word = do
    mapiKey <- asks configUrbanKey
    case mapiKey of
        Nothing     -> pure $ Left "no urban dictionary api key set"
        Just apiKey -> liftIO $ Right <$> getWith
            (urbanOpts apiKey word)
            "https://mashape-community-urban-dictionary.p.rapidapi.com/define"

urbanOpts :: Text -> Text -> W.Options
urbanOpts apiKey term =
    defaults
        &  header "x-rapidapi-key"
        .~ [T.encodeUtf8 apiKey]
        &  header "x-rapidapi-host"
        .~ ["mashape-community-urban-dictionary.p.rapidapi.com"]
        &  header "useQueryString"
        .~ ["true"]
        &  param "term"
        .~ [term]

newtype UrbanDefinition = UrbanDefinition {urbanDefDefinition :: [Text]}
    deriving (Show)

instance FromJSON UrbanDefinition where
    parseJSON = withObject "UrbanDefinition" $ \v -> do
        list <- v .: "list"
        defs <- traverse (.: "definition") list
        pure UrbanDefinition { urbanDefDefinition = defs }

decodeUrban :: BSL.ByteString -> Either String [Definition]
decodeUrban = fmap urbanToDictionary . eitherDecode

urbanToDictionary :: UrbanDefinition -> [Definition]
urbanToDictionary (UrbanDefinition def) =
    [ Definition Nothing def | not (null def) ]

mentionsMe :: Predicate
mentionsMe message = do
    cache <- lift D.readCache
    pure $ D.userId (D.cacheCurrentUser cache) `elem` map
        D.userId
        (D.messageMentions message)

respond :: CommandFunc
respond message = do
    responses   <- getResponses
    responseNum <- liftIO $ (`mod` length responses) <$> (randomIO :: IO Int)
    replyTo message $ responses !! responseNum

showHelp :: CommandFunc
showHelp message = do
    prefix <- asks configCommandPrefix
    args   <- parseArgs Help message
    let
        helpText = case args of
            Right [String commandStr] ->
                case find ((== commandStr) . commandName) commands of
                    Nothing -> "Command not found: **" <> commandStr <> "**"
                    Just command ->
                        "```\n"
                            <> commandStr
                            <> " help\n-----------------------------\n"
                            <> commandHelpText command
                            <> "\n\nUsage: "
                            <> commandStr
                            <> case commandArgInfo command of
                                   [] -> ""
                                   argInfo ->
                                       argStr argInfo
                                           <> "\n\n"
                                           <> ( T.intercalate "\n"
                                              . map
                                                    (\(_, a, help, _) ->
                                                        let arg =
                                                                T.toUpper a
                                                        in  " "
                                                                <> arg
                                                                <> " - "
                                                                <> help
                                                    )
                                              )
                                                  argInfo
                            <> "\n```"
            _ ->
                "```\nCommands (prefix with "
                    <> prefix
                    <> ")\n-----------------------------\n"
                    <> intercalate
                           "\n"
                           (map
                               (\command ->
                                   commandName command
                                       <> argStr (commandArgInfo command)
                                       <> " - "
                                       <> commandHelpText command
                               )
                               commands
                           )
                    <> "\n```"

    replyTo message helpText
  where
    argStr = mconcat . map (\(t, a, _, _) -> " " <> modifyArg t (T.toUpper a))
    modifyArg typ arg = case typ of
        Required -> arg
        Optional -> "[" <> arg <> "]"
        Multi    -> arg <> "..."



infixl 6 |||
(|||) :: Predicate -> Predicate -> Predicate
(pred1 ||| pred2) message = do
    p1 <- pred1 message
    p2 <- pred2 message
    pure $ p1 || p2

isCommand :: Text -> Predicate
isCommand command message = if fromBot message
    then pure False
    else do
        prefix <- asks configCommandPrefix
        (   messageEquals (prefix <> command)
            ||| messageStartsWith (prefix <> command <> " ")
            )
            message

messageStartsWith :: Text -> Predicate
messageStartsWith text =
    pure . (text `T.isPrefixOf`) . T.toLower . D.messageText

messageEquals :: Text -> Predicate
messageEquals text = pure . (text ==) . T.toLower . D.messageText

messageContains :: Text -> Predicate
messageContains text = pure . (text `T.isInfixOf`) . T.toLower . D.messageText

simpleReply :: Text -> CommandFunc
simpleReply replyText message = replyTo message replyText

addResponse :: CommandFunc
addResponse message = do
    args <- parseArgs Add message
    case args of
        Right [String response] -> do
            updateDb (\d -> d { dbResponses = nub $ response : dbResponses d })
            replyTo message $ "Added **" <> response <> "** to responses"
        _ -> replyTo message "Missing response to add"

getResponses :: App [Text]
getResponses = do
    dbRef <- asks configDb
    liftIO $ dbResponses <$> readTVarIO dbRef

removeResponse :: CommandFunc
removeResponse message = do
    args <- parseArgs Remove message
    case args of
        Right [String response] -> do
            oldResponses <- getResponses
            if response `elem` oldResponses
                then do
                    updateDb
                        (\d -> d { dbResponses = delete response $ dbResponses d
                                 }
                        )
                    replyTo message
                        $  "Removed **"
                        <> response
                        <> "** from responses"
                else
                    replyTo message
                    $  "Response **"
                    <> response
                    <> "** not found"
        _ -> replyTo message "Missing response to remove"

listResponses :: CommandFunc
listResponses message = do
    responses <- intercalate "\n" <$> getResponses
    replyTo message responses

setActivity :: D.ActivityType -> CommandFunc
setActivity activityType message = do
    args <- parseArgs activityTypeCommand message
    case args of
        Right [String status] -> do
            lift $ updateStatus activityType $ Just status
            updateDb (\d -> d { dbActivity = Just (activityType, status) })
            replyTo message
                $  "Updated status to **"
                <> activityTypeText
                <> " "
                <> status
                <> "**"
        _ -> do
            lift $ updateStatus activityType Nothing
            updateDb (\d -> d { dbActivity = Nothing })
            replyTo message "Removed status"
  where
    activityTypeText = case activityType of
        D.ActivityTypeGame      -> "Playing"
        D.ActivityTypeListening -> "Listening to"
        D.ActivityTypeStreaming -> "Streaming"
        D.ActivityTypeCompeting -> "Competing in"
    activityTypeCommand = case activityType of
        D.ActivityTypeGame      -> Playing
        D.ActivityTypeListening -> ListeningTo
        D.ActivityTypeCompeting -> CompetingIn
        D.ActivityTypeStreaming -> error "Streaming command not implemented"

setMeanness :: CommandFunc
setMeanness message = do
    args <- parseArgs Meanness message
    case args of
        Left  e                   -> replyTo message $ "Invalid meanness: " <> e
        Right [IntMaybe (Just m)] -> do
            let meanness = min 11 . max 0 $ m
            updateDb (\d -> d { dbMeanness = meanness })
            replyTo message
                $  "Set meanness to **"
                <> T.pack (show meanness)
                <> "**"
        Right _ -> do
            dbRef    <- asks configDb
            meanness <- liftIO $ dbMeanness <$> readTVarIO dbRef
            replyTo message
                $  "Current meanness is **"
                <> T.pack (show meanness)
                <> "**"

updateDb :: (Db -> Db) -> App ()
updateDb f = do
    dbRef      <- asks configDb
    dbFileName <- asks configDbFile
    db         <- liftIO $ atomically $ do
        modifyTVar' dbRef f
        readTVar dbRef
    liftIO $ writeFile dbFileName $ show db
