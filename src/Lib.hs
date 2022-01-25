{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Lib where

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
                                                , for_
                                                )
import           Data.List                      ( delete
                                                , nub
                                                , stripPrefix
                                                )
import           Data.Maybe                     ( fromMaybe )
import           Data.String                    ( IsString )
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
import qualified Text.Parsec                   as P
import           Text.Parsec                    ( ParseError )
import           Text.Parsec.Text               ( Parser )
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

data ArgArity = Single | Multi
data ArgReq = Required | Optional
type ArgType = (ArgReq, ArgArity)
type CommandFunc = D.Message -> App ()
type Predicate = D.Message -> App Bool
data Arg = Arg
    { argType        :: ArgType
    , argName        :: Text
    , argDescription :: Text
    }
data CommandArgInfo = ArgInfo
    { argInfoArgs   :: [Arg]
    , argInfoParser :: Parser CommandArgs
    }

restStr :: Parser Text
restStr = do
    cs <- P.many P.anyChar
    return $ T.pack cs

restStr1 :: Parser Text
restStr1 = do
    cs <- P.many1 P.anyChar
    return $ T.pack cs

int :: Parser Int
int = do
    d <- P.many1 P.digit
    return $ read d

str :: Parser Text
str = do
    cs <- P.many1 (P.satisfy (not . isSpace))
    return $ T.pack cs

ints :: Parser [Int]
ints = do
    ds <- P.many (P.spaces *> P.many1 P.digit)
    return $ map read ds

parseArgs :: Command -> D.Message -> App (Either ParseError CommandArgs)
parseArgs command message = do
    prefix <- asks configCommandPrefix
    return
        $ P.parse
              (  P.string (T.unpack prefix)
              *> P.string (T.unpack $ commandName command)
              *> P.spaces
              *> argInfoParser (commandArgInfo command)
              )
              ""
        $ D.messageText message

data Command
    = RR
    | Define
    | Urban
    | Add
    | Remove
    | List
    | Playing
    | ListeningTo
    | CompetingIn
    | Meanness
    | Sum
    | Help
    deriving (Show, Eq, Enum, Bounded)

data CommandArgs
    = RRArgs
    | DefineArgs Text
    | UrbanArgs Text
    | AddArgs Text
    | RemoveArgs Text
    | ListArgs
    | PlayingArgs (Maybe Text)
    | ListeningToArgs (Maybe Text)
    | CompetingInArgs (Maybe Text)
    | MeannessArgs (Maybe Int)
    | SumArgs [Int]
    | HelpArgs (Maybe Text)
    deriving (Show, Eq)

commandName :: Command -> Text
commandName = T.toLower . T.pack . head . words . show

commandArgInfo :: Command -> CommandArgInfo
commandArgInfo RR = ArgInfo { argInfoArgs = [], argInfoParser = pure RRArgs }
commandArgInfo Define = ArgInfo
    { argInfoArgs   = [ Arg { argType        = (Required, Multi)
                            , argName        = "term"
                            , argDescription = "The word or phrase to look up."
                            }
                      ]
    , argInfoParser = DefineArgs <$> restStr1
    }
commandArgInfo Urban = ArgInfo
    { argInfoArgs   = [ Arg { argType        = (Required, Multi)
                            , argName        = "term"
                            , argDescription = "The word or phrase to look up."
                            }
                      ]
    , argInfoParser = UrbanArgs <$> restStr1
    }
commandArgInfo Add = ArgInfo
    { argInfoArgs   = [ Arg { argType        = (Required, Multi)
                            , argName        = "response"
                            , argDescription = "The response to add."
                            }
                      ]
    , argInfoParser = AddArgs <$> restStr1
    }
commandArgInfo Remove = ArgInfo
    { argInfoArgs   = [ Arg { argType        = (Required, Multi)
                            , argName        = "response"
                            , argDescription = "The response to remove."
                            }
                      ]
    , argInfoParser = RemoveArgs <$> restStr1
    }
commandArgInfo List =
    ArgInfo { argInfoArgs = [], argInfoParser = pure ListArgs }
commandArgInfo Playing = ArgInfo
    { argInfoArgs   = [ Arg { argType        = (Optional, Multi)
                            , argName        = "name"
                            , argDescription = "What's the bot playing?"
                            }
                      ]
    , argInfoParser = PlayingArgs <$> P.optionMaybe restStr
    }
commandArgInfo ListeningTo = ArgInfo
    { argInfoArgs   = [ Arg { argType        = (Optional, Multi)
                            , argName        = "name"
                            , argDescription = "What's the bot listening to?"
                            }
                      ]
    , argInfoParser = ListeningToArgs <$> P.optionMaybe restStr
    }
commandArgInfo CompetingIn = ArgInfo
    { argInfoArgs   = [ Arg { argType        = (Optional, Multi)
                            , argName        = "name"
                            , argDescription = "What's the bot competing in?"
                            }
                      ]
    , argInfoParser = CompetingInArgs <$> P.optionMaybe restStr
    }
commandArgInfo Meanness = ArgInfo
    { argInfoArgs   =
        [ Arg
              { argType        = (Optional, Single)
              , argName        = "level"
              , argDescription =
                  "The number between 0 and 10 to set the bot's meanness to. Higher is meaner. Leave blank to view current meanness."
              }
        ]
    , argInfoParser = MeannessArgs <$> P.optionMaybe int
    }
commandArgInfo Sum = ArgInfo
    { argInfoArgs   = [ Arg { argType        = (Optional, Multi)
                            , argName        = "nums"
                            , argDescription = "Some integers to sum."
                            }
                      ]
    , argInfoParser = SumArgs <$> ints
    }
commandArgInfo Help = ArgInfo
    { argInfoArgs   = [ Arg { argType        = (Optional, Single)
                            , argName        = "command"
                            , argDescription = "Command to show help for."
                            }
                      ]
    , argInfoParser = HelpArgs <$> P.optionMaybe str
    }

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
commandHelpText Sum = "Sum some integers."
commandHelpText Help =
    "Show this help or show detailed help for a given command."

commandFunc :: CommandArgs -> CommandFunc
commandFunc RRArgs                = russianRoulette
commandFunc (DefineArgs term    ) = define getDefineOutput term
commandFunc (UrbanArgs  term    ) = define getUrbanOutput term
commandFunc (AddArgs    response) = addResponse response
commandFunc (RemoveArgs response) = removeResponse response
commandFunc ListArgs              = listResponses
commandFunc (PlayingArgs status)  = setActivity D.ActivityTypeGame status
commandFunc (ListeningToArgs status) =
    setActivity D.ActivityTypeListening status
commandFunc (CompetingInArgs status) =
    setActivity D.ActivityTypeCompeting status
commandFunc (MeannessArgs meanness) = setMeanness meanness
commandFunc (SumArgs      xs      ) = sumInts xs
commandFunc (HelpArgs     command ) = showHelp command

commands :: [Command]
commands = [minBound .. maxBound]

predicates :: [(Predicate, CommandFunc)]
predicates =
    [ (isCarl, simpleReply "Carl is a cuck")
    , ( mentionsMe ||| messageContains "@everyone" ||| messageContains "@here"
      , respond
      )
    ]

handleParseError :: D.Message -> Command -> ParseError -> App ()
handleParseError message command e =
    replyTo message
        $  "```\nInvalid args:\n\n"
        <> D.messageText message
        <> "\n"
        <> T.pack (replicate (P.sourceColumn (P.errorPos e) - 1) ' ')
        <> "^\n\n"
        <> T.pack (show e)
        <> "\n\n"
        <> showUsage command
        <> "\n```"

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
                (command : _) ->
                    parseArgs command message
                        >>= (\case
                                Left  e   -> handleParseError message command e
                                Right cas -> commandFunc cas message
                            )
                _ -> do
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

define :: (Text -> App (Maybe Text)) -> Text -> CommandFunc
define getOutput phrase message = do
    moutput <- getOutput phrase
    case moutput of
        Just output -> replyTo message output
        Nothing ->
            replyTo message $ "No definition found for **" <> phrase <> "**"

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

argStr :: CommandArgInfo -> Text
argStr ArgInfo { argInfoArgs } = mconcat $ map
    (\Arg { argType, argName } -> " " <> modifyArg argType (T.toUpper argName))
    argInfoArgs

modifyArg :: (Semigroup a, IsString a) => (ArgReq, ArgArity) -> a -> a
modifyArg (r, s) arg =
    let arg1 = case s of
            Single -> arg
            Multi  -> arg <> "..."
    in  case r of
            Required -> arg1
            Optional -> "[" <> arg1 <> "]"

showUsage :: Command -> Text
showUsage command =
    "Usage: " <> commandName command <> argStr (commandArgInfo command)

showHelp :: Maybe Text -> CommandFunc
showHelp mcommand message = do
    prefix <- asks configCommandPrefix
    let
        helpText = case mcommand of
            (Just commandStr) ->
                case find ((== commandStr) . commandName) commands of
                    Nothing -> "Command not found: **" <> commandStr <> "**"
                    Just command ->
                        "```\n"
                            <> commandStr
                            <> " help\n-----------------------------\n"
                            <> commandHelpText command
                            <> "\n\n"
                            <> showUsage command
                            <> case argInfoArgs $ commandArgInfo command of
                                   [] -> ""
                                   argInfo ->
                                       "\n\n"
                                           <> (T.intercalate "\n" . map
                                                  (\Arg { argName, argDescription } ->
                                                      let arg = T.toUpper
                                                              argName
                                                      in  " "
                                                              <> arg
                                                              <> " - "
                                                              <> argDescription
                                                  )
                                              )
                                                  argInfo
                            <> "\n```"
            Nothing ->
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

addResponse :: Text -> CommandFunc
addResponse response message = do
    updateDb (\d -> d { dbResponses = nub $ response : dbResponses d })
    replyTo message $ "Added **" <> response <> "** to responses"

getResponses :: App [Text]
getResponses = do
    dbRef <- asks configDb
    liftIO $ dbResponses <$> readTVarIO dbRef

removeResponse :: Text -> CommandFunc
removeResponse response message = do
    oldResponses <- getResponses
    if response `elem` oldResponses
        then do
            updateDb (\d -> d { dbResponses = delete response $ dbResponses d })
            replyTo message $ "Removed **" <> response <> "** from responses"
        else replyTo message $ "Response **" <> response <> "** not found"

listResponses :: CommandFunc
listResponses message = do
    responses <- intercalate "\n" <$> getResponses
    replyTo message responses

setActivity :: D.ActivityType -> Maybe Text -> CommandFunc

setActivity activityType Nothing message = do
    lift $ updateStatus activityType Nothing
    updateDb (\d -> d { dbActivity = Nothing })
    replyTo message "Removed status"

setActivity activityType (Just status) message = do
    lift $ updateStatus activityType $ Just status
    updateDb (\d -> d { dbActivity = Just (activityType, status) })
    replyTo message
        $  "Updated status to **"
        <> activityTypeText
        <> " "
        <> status
        <> "**"
  where
    activityTypeText = case activityType of
        D.ActivityTypeGame      -> "Playing"
        D.ActivityTypeListening -> "Listening to"
        D.ActivityTypeStreaming -> "Streaming"
        D.ActivityTypeCompeting -> "Competing in"

setMeanness :: Maybe Int -> CommandFunc

setMeanness (Just m) message = do
    let meanness = min 11 . max 0 $ m
    updateDb (\d -> d { dbMeanness = meanness })
    replyTo message $ "Set meanness to **" <> T.pack (show meanness) <> "**"

setMeanness Nothing message = do
    dbRef    <- asks configDb
    meanness <- liftIO $ dbMeanness <$> readTVarIO dbRef
    replyTo message $ "Current meanness is **" <> T.pack (show meanness) <> "**"

sumInts :: (Show a, Foldable t, Num a) => t a -> CommandFunc
sumInts xs message = replyTo message $ T.pack (show $ sum xs)

updateDb :: (Db -> Db) -> App ()
updateDb f = do
    dbRef      <- asks configDb
    dbFileName <- asks configDbFile
    db         <- liftIO $ atomically $ do
        modifyTVar' dbRef f
        readTVar dbRef
    liftIO $ writeFile dbFileName $ show db
