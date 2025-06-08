{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
    ( goodbot
    ) where

import           CommandHandlers                ( CommandFunc
                                                , commandArgs
                                                , commandFunc
                                                , commandName
                                                , commandNames
                                                , commands
                                                , isFromCarl
                                                , mentionsMe
                                                , predicates
                                                , respond
                                                )
import           Commands                       ( handleCommandWithAliases )
import           Config                         ( Config(..)
                                                , UserConfig(..)
                                                , defaultDbFile
                                                , loadConfig
                                                )
import           Control.Concurrent.STM         ( TVar
                                                , newTVarIO
                                                , readTVarIO
                                                )
import           Control.Monad                  ( filterM
                                                , unless
                                                , when
                                                )
import           Control.Monad.Catch            ( catchAll )
import           Control.Monad.Reader           ( ReaderT
                                                , ask
                                                , asks
                                                , runReaderT
                                                , lift
                                                , liftIO
                                                )
import           Data.Maybe                     ( fromMaybe )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Data.Time                      ( defaultTimeLocale
                                                , formatTime
                                                , getCurrentTime
                                                , getCurrentTimeZone
                                                , utcToLocalTime
                                                )
import           Database                       ( Db(..)
                                                , loadDb
                                                )
import qualified Discord                       as D
import qualified Discord.Types                 as D
import           DiscordHelper                  ( createMessage
                                                , isFromSelf
                                                , messageContains
                                                , replyTo
                                                , updateStatus
                                                , writeLog
                                                , (|||)
                                                )
import           RateLimit                      ( RateLimiter
                                                , checkRateLimit
                                                , createRateLimiter
                                                , defaultRateLimitConfig
                                                )
import           System.Environment             ( getArgs )
import           System.Random                  ( randomIO )
import           Types                          ( meannessRatio
                                                , unMeannessLevel
                                                )

type App a = ReaderT Config D.DiscordHandler a

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
            []     -> "config.yaml"
            [path] -> path
            _      -> error "too many arguments provided: expected at most 1"
    configResult <- loadConfig configFile
    config@UserConfig {..} <- case configResult of
        Left err -> error err
        Right cfg -> pure cfg
    db <- loadDb (fromMaybe defaultDbFile userConfigDbFile)
    dbRef <- newTVarIO db
    rateLimiter <- createRateLimiter
    userFacingError <-
        D.runDiscord
                (D.def { D.discordToken   = userConfigDiscordToken
                       , D.discordOnStart = onStart config
                       , D.discordOnEvent = eventHandler config dbRef rateLimiter
                       , D.discordOnLog   = logText
                       }
                )
            `catchAll` \e -> pure $ T.pack $ show e
    T.putStrLn userFacingError

onStart :: UserConfig -> D.DiscordHandler ()
onStart config = do
    writeLog $ "bot started with config " <> T.pack (show config)

eventHandler :: UserConfig -> TVar Db -> RateLimiter -> D.Event -> D.DiscordHandler ()
eventHandler UserConfig {..} dbRef rateLimiter event =
    let config = Config
            { configDictKey       = userConfigDictKey
            , configUrbanKey      = userConfigUrbanKey
            , configCommandPrefix = fromMaybe "!" userConfigCommandPrefix
            , configDb            = dbRef
            , configDbFile        = fromMaybe defaultDbFile userConfigDbFile
            , configRateLimiter   = rateLimiter
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

messageCreate :: D.Message -> App ()
messageCreate message = do
    self <- lift $ isFromSelf message
    if self
        then pure ()
        else do
            prefix <- asks configCommandPrefix
            config <- ask
            
            -- Check if this looks like a command
            let messageText = D.messageContent message
            let looksLikeCommand = any (\cmd -> 
                    any (\name -> (prefix <> name) `T.isPrefixOf` messageText) 
                        (commandNames cmd)) commands
            
            -- Apply rate limiting for commands
            if looksLikeCommand
                then do
                    rateLimiter <- asks configRateLimiter
                    allowed <- liftIO $ checkRateLimit 
                        defaultRateLimitConfig 
                        rateLimiter 
                        (D.userId $ D.messageAuthor message)
                        messageText
                    
                    if allowed
                        then processCommand message
                        else lift $ replyTo message "You're sending commands too quickly! Please slow down."
                else processCommand message

processCommand :: D.Message -> App ()
processCommand message = do
    prefix <- asks configCommandPrefix
    config <- ask
    commandHandled <- lift $ handleCommandWithAliases
        prefix
        commandNames
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
    when (unMeannessLevel meanness > 0) $ do
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