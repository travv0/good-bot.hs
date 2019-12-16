{-# LANGUAGE OverloadedStrings #-}

module Lib where

import           Control.Monad                  ( when )
import           Data.Text                      ( isPrefixOf
                                                , toLower
                                                , Text
                                                )
import           System.Random
import qualified Data.Text.IO                  as TIO

import           Discord
import           Discord.Types
import qualified Discord.Requests              as R

bigbot :: IO ()
bigbot = do
  userFacingError <- runDiscord $ def
    { discordToken   =
      "NjU1OTI5MjM4ODI5OTI0MzUz.XfbQHw.ZHvs3nCZhfXIjQJt_9_6vSfowqk"
    , discordOnEvent = eventHandler
    }
  TIO.putStrLn userFacingError

eventHandler :: DiscordHandle -> Event -> IO ()
eventHandler dis event = case event of
  MessageCreate m ->
    when (not (fromBot m) && isRussianRoulette (messageText m)) $ do
      chamber <- (`mod` 6) <$> (randomIO :: IO Int)
      case (chamber, messageGuild m) of
        (0, Just gId) -> do
          _ <- restCall dis (R.CreateMessage (messageChannel m) "Bang!")
          _ <- restCall
            dis
            (R.CreateGuildBan gId
                              (userId $ messageAuthor m)
                              (R.CreateGuildBanOpts Nothing (Just "Bang!"))
            )
          pure ()
        _ -> do
          _ <- restCall dis (R.CreateMessage (messageChannel m) "Click.")
          pure ()
  _ -> pure ()

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

isRussianRoulette :: Text -> Bool
isRussianRoulette = ("!rr" `isPrefixOf`) . toLower
