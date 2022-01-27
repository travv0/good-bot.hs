{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lib
    ( goodbot
    ) where

import           Commands                       ( ArgParser
                                                , customRestArg
                                                , defaultHelpText
                                                , handleCommand
                                                , int
                                                , num
                                                , optArg
                                                , optMultiArg
                                                , optRestArg
                                                , restArg
                                                , str
                                                )
import           Control.Applicative            ( (<|>) )
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
                                                , unless
                                                , when
                                                )
import           Control.Monad.Catch            ( catchAll
                                                , catchIOError
                                                )
import           Control.Monad.Reader           ( ReaderT
                                                , ask
                                                , asks
                                                , lift
                                                , liftIO
                                                , runReaderT
                                                )
import           Data.Aeson                     ( (.:)
                                                , FromJSON
                                                , defaultOptions
                                                , eitherDecode
                                                , fieldLabelModifier
                                                , genericParseJSON
                                                , parseJSON
                                                , withObject
                                                )
import qualified Data.ByteString.Lazy          as BSL
import           Data.Char                      ( toLower )
import           Data.Functor                   ( ($>) )
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
import           DiscordHelper                  ( Predicate
                                                , createGuildBan
                                                , createMessage
                                                , isFromSelf
                                                , isFromUser
                                                , messageContains
                                                , replyTo
                                                , updateStatus
                                                , writeError
                                                , writeLog
                                                , (|||)
                                                )
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
onStart config = do
    writeLog $ "bot started with config " <> T.pack (show config)

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

type CommandFunc = D.Message -> App ()

data Command
    = Define
    | Urban
    | Add
    | Remove
    | List
    | Playing
    | ListeningTo
    | CompetingIn
    | Meanness
    | Calc
    | Sum
    | RR
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
    | SumArgs (Maybe [Double])
    | CalcArgs CalcExpr
    | HelpArgs (Maybe Text)
    deriving (Show, Eq)

data CalcOp
    = Plus
    | Minus
    | Times
    | Divide
    deriving (Show, Eq)

data CalcExpr
    = CalcExpr CalcExpr CalcOp CalcExpr
    | CalcVal Double
    deriving (Show, Eq)

calcExpr :: Parser CalcExpr
calcExpr = calcExpr' Nothing <* P.eof
  where
    calcExpr' :: Maybe CalcExpr -> Parser CalcExpr

    calcExpr' Nothing = do
        P.spaces
        first <- single Nothing
        P.try $ calcExpr' (Just first) <|> pure first

    calcExpr' (Just prev) = do
        P.spaces
        P.choice
            [ P.choice
                [ P.try $ do
                    op     <- calcOp
                    second <- single Nothing
                    calcExpr' . Just $ CalcExpr prev op second
                , P.try . single $ Just prev
                ]
            , pure prev
            ]

    single prev = P.spaces *> (calcVal <|> calcParenExpr prev)

    calcVal = P.spaces *> (CalcVal <$> num)

    calcParenExpr prev =
        P.spaces *> P.between (P.char '(') (P.char ')') (calcExpr' prev)

    calcOp :: Parser CalcOp
    calcOp = P.spaces *> P.choice
        [ P.char '+' $> Plus
        , P.char '-' $> Minus
        , P.char '*' $> Times
        , P.char '/' $> Divide
        ]


commandName :: Command -> Text
commandName = T.toLower . T.pack . head . words . show

commandArgs :: Command -> ArgParser CommandArgs
commandArgs RR = pure RRArgs
commandArgs Define =
    DefineArgs <$> restArg "term" "The word or phrase to look up."
commandArgs Urban =
    UrbanArgs <$> restArg "term" "The word or phrase to look up."
commandArgs Add = AddArgs <$> restArg "response" "The response to add."
commandArgs Remove =
    RemoveArgs <$> restArg "response" "The response to remove."
commandArgs List = pure ListArgs
commandArgs Playing =
    PlayingArgs <$> optRestArg "name" "What's the bot playing?"
commandArgs ListeningTo =
    ListeningToArgs <$> optRestArg "name" "What's the bot listening to?"
commandArgs CompetingIn =
    CompetingInArgs <$> optRestArg "name" "What's the bot competing in?"
commandArgs Meanness =
    MeannessArgs
        <$> optArg
                "level"
                "The number between 0 and 10 to set the bot's meanness to. Higher is meaner. Leave blank to view current meanness."
                int
commandArgs Calc =
    CalcArgs
        <$> customRestArg "input"
                          "Expression for calculator to evaluate."
                          calcExpr
commandArgs Sum = SumArgs <$> optMultiArg "nums" "Some numbers to sum." num
commandArgs Help =
    HelpArgs <$> optArg "command" "Command to show help for." str

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
commandHelpText Calc = "A basic calculator."
commandHelpText Sum  = "Sum some numbers."
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
commandFunc (CalcArgs     expr    ) = calc expr
commandFunc (SumArgs      xs      ) = sumInts xs
commandFunc (HelpArgs     command ) = showHelp command

commands :: [Command]
commands = [minBound .. maxBound]

isFromCarl :: Predicate
isFromCarl = isFromUser 235148962103951360

predicates :: [(Predicate, CommandFunc)]
predicates =
    [ (isFromCarl, simpleReply "Carl is a cuck")
    , ( mentionsMe ||| messageContains "@everyone" ||| messageContains "@here"
      , respond
      )
    ]

messageCreate :: D.Message -> App ()
messageCreate message = do
    self <- lift $ isFromSelf message
    if self
        then pure ()
        else do
            prefix         <- asks configCommandPrefix
            config         <- ask
            commandHandled <- lift $ handleCommand
                prefix
                commandName
                commandArgs
                (\args m -> flip runReaderT config $ commandFunc args m)
                Nothing
                message
                commands
            unless commandHandled $ do
                predicateMatches <- lift
                    $ filterM (\(p, _) -> p message) predicates
                case predicateMatches of
                    ((_, c) : _) -> c message
                    _            -> pure ()

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
            .  lift
            .  createMessage channelId Nothing
            .  T.pack
            $  "shut up <@"
            <> show userId
            <> ">"
  where
    meannessRatio 11 = 1
    meannessRatio n  = 2000 `div` n

russianRoulette :: CommandFunc
russianRoulette message = do
    chamber <- liftIO $ (`mod` 6) <$> (randomIO :: IO Int)
    lift $ case (chamber, D.messageGuild message) of
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
    lift . replyTo message $ case moutput of
        Just output -> output
        Nothing     -> "No definition found for **" <> phrase <> "**"

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
    lift (writeError $ T.pack err) >> pure Nothing
buildDefineOutputHandleFail _ (Left err) (Just fallback) =
    lift (writeError $ T.pack err) >> fallback
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
    cache <- D.readCache
    pure $ D.userId (D.cacheCurrentUser cache) `elem` map
        D.userId
        (D.messageMentions message)

respond :: CommandFunc
respond message = do
    responses   <- getResponses
    responseNum <- liftIO $ (`mod` length responses) <$> (randomIO :: IO Int)
    lift $ replyTo message $ responses !! responseNum

showHelp :: Maybe Text -> CommandFunc
showHelp mcommand message = do
    prefix <- asks configCommandPrefix
    lift . replyTo message $ defaultHelpText prefix
                                             commandName
                                             commandHelpText
                                             commandArgs
                                             mcommand
                                             commands

simpleReply :: Text -> CommandFunc
simpleReply replyText message = lift $ replyTo message replyText

addResponse :: Text -> CommandFunc
addResponse response message = do
    updateDb (\d -> d { dbResponses = nub $ response : dbResponses d })
    lift . replyTo message $ "Added **" <> response <> "** to responses"

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
            lift
                .  replyTo message
                $  "Removed **"
                <> response
                <> "** from responses"
        else
            lift . replyTo message $ "Response **" <> response <> "** not found"

listResponses :: CommandFunc
listResponses message = do
    responses <- intercalate "\n" <$> getResponses
    lift $ replyTo message responses

setActivity :: D.ActivityType -> Maybe Text -> CommandFunc

setActivity activityType Nothing message = do
    lift $ updateStatus activityType Nothing
    updateDb (\d -> d { dbActivity = Nothing })
    lift $ replyTo message "Removed status"

setActivity activityType (Just status) message = do
    lift $ updateStatus activityType $ Just status
    updateDb (\d -> d { dbActivity = Just (activityType, status) })
    lift
        $  replyTo message
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
    lift
        .  replyTo message
        $  "Set meanness to **"
        <> T.pack (show meanness)
        <> "**"

setMeanness Nothing message = do
    dbRef    <- asks configDb
    meanness <- liftIO $ dbMeanness <$> readTVarIO dbRef
    lift
        .  replyTo message
        $  "Current meanness is **"
        <> T.pack (show meanness)
        <> "**"

reduceExpr :: CalcExpr -> Double
reduceExpr (CalcVal v            ) = v
reduceExpr (CalcExpr e1 Plus   e2) = reduceExpr e1 + reduceExpr e2
reduceExpr (CalcExpr e1 Minus  e2) = reduceExpr e1 - reduceExpr e2
reduceExpr (CalcExpr e1 Times  e2) = reduceExpr e1 * reduceExpr e2
reduceExpr (CalcExpr e1 Divide e2) = reduceExpr e1 / reduceExpr e2

eval :: CalcExpr -> Double
eval = reduceExpr

calc :: CalcExpr -> CommandFunc
calc expr message = lift . replyTo message . T.pack . show . eval $ expr

sumInts :: (Foldable t, Show a, Num a) => Maybe (t a) -> CommandFunc
sumInts (Just xs) message = lift . replyTo message . T.pack . show $ sum xs
sumInts Nothing   message = lift . replyTo message . T.pack $ show (0 :: Int)

updateDb :: (Db -> Db) -> App ()
updateDb f = do
    dbRef      <- asks configDb
    dbFileName <- asks configDbFile
    db         <- liftIO $ atomically $ do
        modifyTVar' dbRef f
        readTVar dbRef
    liftIO $ writeFile dbFileName $ show db
