{-# LANGUAGE OverloadedStrings #-}

module Lib where

import           Control.Monad                  ( when
                                                , void
                                                )
import           Data.Text                      ( isPrefixOf
                                                , toLower
                                                , Text
                                                , pack
                                                )
import           System.Environment
import           System.Random
import qualified Data.Text.IO                  as TIO

import           Discord
import           Discord.Types
import qualified Discord.Requests              as R

bigbot :: IO ()
bigbot = do
  token           <- pack <$> getEnv "BIGBOT_TOKEN"
  userFacingError <- runDiscord
    $ def { discordToken = token, discordOnEvent = eventHandler }
  TIO.putStrLn userFacingError

eventHandler :: DiscordHandle -> Event -> IO ()
eventHandler dis event = case event of
  MessageCreate m ->
    when (not (fromBot m) && isRussianRoulette (messageText m)) $ do
      chamber <- (`mod` 6) <$> (randomIO :: IO Int)
      case (chamber, messageGuild m) of
        (0, Just gId) -> do
          void $ restCall dis $ R.CreateMessage (messageChannel m) "Bang!"
          void $ restCall dis $ R.CreateGuildBan
            gId
            (userId $ messageAuthor m)
            (R.CreateGuildBanOpts Nothing (Just "Bang!"))
        _ -> void $ restCall dis $ R.CreateMessage (messageChannel m) "Click."
  _ -> pure ()

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

isRussianRoulette :: Text -> Bool
isRussianRoulette = ("!rr" `isPrefixOf`) . toLower
