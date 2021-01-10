{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Control.Monad (void, when)
import Control.Monad.Reader
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Discord as D
import qualified Discord.Requests as D
import qualified Discord.Types as D
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
messageCreate message = do
    when (not (fromBot message) && isRussianRoulette (D.messageText message)) $ do
        chamber <- liftIO $ (`mod` 6) <$> (randomIO :: IO Int)
        case (chamber, D.messageGuild message) of
            (0, Just gId) -> do
                createMessage (D.messageChannel message) response
                createGuildBan gId (D.userId $ D.messageAuthor message) response
              where
                response = "Bang!"
            _ -> createMessage (D.messageChannel message) "Click."

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

isRussianRoulette :: Text -> Bool
isRussianRoulette = ("!rr" `T.isPrefixOf`) . T.toLower
