{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Control.Monad (void, when)
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
eventHandler dis event = case event of
    D.MessageCreate message -> messageCreate dis message
    D.TypingStart typingInfo -> typingStart dis typingInfo
    _ -> pure ()

messageCreate :: D.DiscordHandle -> D.Message -> IO ()
messageCreate dis message =
    when (not (fromBot message) && isRussianRoulette (D.messageText message)) $ do
        chamber <- (`mod` 6) <$> (randomIO :: IO Int)
        case (chamber, D.messageGuild message) of
            (0, Just gId) -> do
                D.restCall dis $ D.CreateMessage (D.messageChannel message) "Bang!"
                void $
                    D.restCall dis $
                        D.CreateGuildBan
                            gId
                            (D.userId $ D.messageAuthor message)
                            (D.CreateGuildBanOpts Nothing (Just "Bang!"))
            _ -> void $ D.restCall dis $ D.CreateMessage (D.messageChannel message) "Click."

typingStart :: D.DiscordHandle -> D.TypingInfo -> IO ()
typingStart dis (D.TypingInfo userId channelId utcTime) = do
    shouldReply <- (== 0) . (`mod` 1000) <$> (randomIO :: IO Int)
    when shouldReply $
        void $ D.restCall dis $ D.CreateMessage channelId $ T.pack $ "shut up <@" <> show userId <> ">"

fromBot :: D.Message -> Bool
fromBot m = D.userIsBot (D.messageAuthor m)

isRussianRoulette :: Text -> Bool
isRussianRoulette = ("!rr" `T.isPrefixOf`) . T.toLower
