{-# LANGUAGE OverloadedStrings #-}

module CommandHandlers
    ( CommandFunc
    , Command(..)
    , CommandArgs(..)
    , commandName
    , commandNames
    , commandArgs
    , commandHelpText
    , commandFunc
    , commands
    , predicates
    , isFromCarl
    , mentionsMe
    , respond
    ) where

import           ApiClient                      ( getDefineOutput
                                                , getUrbanOutput
                                                )
import           Calculator                     ( CalcExpr
                                                , CalcError(..)
                                                , calcExpr
                                                , eval
                                                )
import           Commands                       ( ArgParser
                                                , customRestArg
                                                , defaultHelpText
                                                , int
                                                , optArg
                                                , optRestArg
                                                , restArg
                                                , str
                                                )
import           Config                         ( Config(..) )
import           Control.Concurrent.STM         ( readTVarIO )
import           Control.Monad                  ( when )
import           Control.Monad.Reader           ( ReaderT
                                                , asks
                                                , lift
                                                , liftIO
                                                )
import           Data.List                      ( delete
                                                , nub
                                                )
import           Data.Maybe                     ( fromMaybe )
import           Data.Text                      ( Text
                                                , intercalate
                                                )
import qualified Data.Text                     as T
import           Database                       ( Db(..)
                                                , updateDb
                                                )
import qualified Discord                       as D
import qualified Discord.Types                 as D
import           DiscordHelper                  ( Predicate
                                                , createGuildBan
                                                , createMessage
                                                , isFromBot
                                                , isFromUser
                                                , messageContains
                                                , replyTo
                                                , updateStatus
                                                , (|||)
                                                )
import           System.Random                  ( randomIO )
import           Types                          ( mkMeannessLevel
                                                , unMeannessLevel
                                                )

type App a = ReaderT Config D.DiscordHandler a
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
    | CalcArgs CalcExpr
    | HelpArgs (Maybe Text)
    deriving (Show, Eq)

commandName :: Command -> Text
commandName = T.toLower . T.pack . head . words . show

commandAliases :: Command -> [Text]
commandAliases Calc = ["calculate", "math"]
commandAliases RR = ["roulette"]
commandAliases _ = []

commandNames :: Command -> [Text]
commandNames cmd = commandName cmd : commandAliases cmd

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
                "The number between 0 and 11 to set the bot's meanness to. Higher is meaner. Leave blank to view current meanness."
                int
commandArgs Calc =
    CalcArgs
        <$> customRestArg "input"
                          "Expression for calculator to evaluate."
                          calcExpr
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
    = "Set bot's meanness from 0-11 or display current meanness if no argument given."
commandHelpText Calc = "A basic calculator."
commandHelpText Help =
    "Show this help or show detailed help for a given command."

commandFunc :: CommandArgs -> CommandFunc
commandFunc RRArgs                = russianRoulette
commandFunc (DefineArgs term    ) = define True term
commandFunc (UrbanArgs  term    ) = define False term
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

mentionsMe :: Predicate
mentionsMe message = do
    cache <- D.readCache
    pure $ D.userId (D.cacheCurrentUser cache) `elem` map
        D.userId
        (D.messageMentions message)

russianRoulette :: CommandFunc
russianRoulette message = do
    chamber <- liftIO $ (`mod` 6) <$> (randomIO :: IO Int)
    lift $ case (chamber, D.messageGuild message) of
        (0, Just gId) -> do
            replyTo message response
            createGuildBan gId (D.userId $ D.messageAuthor message) response
            where response = "Bang!"
        _ -> replyTo message "Click."

define :: Bool -> Text -> CommandFunc
define useDict phrase message = do
    dictKey <- asks configDictKey
    urbanKey <- asks configUrbanKey
    moutput <- lift $ if useDict 
        then getDefineOutput dictKey phrase
        else getUrbanOutput urbanKey phrase
    lift . replyTo message $ case moutput of
        Just output -> output
        Nothing     -> "No definition found for **" <> phrase <> "**"

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
    db <- asks configDb
    dbFile <- asks configDbFile
    liftIO $ updateDb db dbFile (\d -> d { dbResponses = nub $ response : dbResponses d })
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
            db <- asks configDb
            dbFile <- asks configDbFile
            liftIO $ updateDb db dbFile (\d -> d { dbResponses = delete response $ dbResponses d })
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
    db <- asks configDb
    dbFile <- asks configDbFile
    liftIO $ updateDb db dbFile (\d -> d { dbActivity = Nothing })
    lift $ replyTo message "Removed status"

setActivity activityType (Just status) message = do
    lift $ updateStatus activityType $ Just status
    db <- asks configDb
    dbFile <- asks configDbFile
    liftIO $ updateDb db dbFile (\d -> d { dbActivity = Just (activityType, status) })
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
    let meanness = mkMeannessLevel m
    db <- asks configDb
    dbFile <- asks configDbFile
    liftIO $ updateDb db dbFile (\d -> d { dbMeanness = meanness })
    lift
        .  replyTo message
        $  "Set meanness to **"
        <> T.pack (show (unMeannessLevel meanness))
        <> "**"

setMeanness Nothing message = do
    dbRef    <- asks configDb
    meanness <- liftIO $ dbMeanness <$> readTVarIO dbRef
    lift
        .  replyTo message
        $  "Current meanness is **"
        <> T.pack (show (unMeannessLevel meanness))
        <> "**"

calc :: CalcExpr -> CommandFunc
calc expr message = do
    result <- liftIO $ eval expr
    lift . replyTo message $ case result of
        Left DivisionByZero -> "Error: Division by zero"
        Left (InvalidOperation err) -> "Error: " <> T.pack err
        Right value -> T.pack $ show value