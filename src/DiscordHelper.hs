{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module DiscordHelper
    ( isCommand
    , writeError
    , writeLog
    , restCall
    , replyTo
    , createGuildBan
    , createMessage
    , isFromSelf
    , isFromUser
    , isFromBot
    , messageStartsWith
    , messageEquals
    , messageContains
    , updateStatus
    , Predicate
    , (|||)
    , (&&&)
    ) where

import           Control.Concurrent             ( writeChan )
import           Control.Monad.Reader           ( ask
                                                , liftIO
                                                )
import           Data.Aeson                     ( FromJSON )
import           Data.Foldable                  ( for_ )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Discord                       as D
import qualified Discord.Handle                as D
import qualified Discord.Internal.Rest         as D
import qualified Discord.Requests              as D

updateStatus :: D.ActivityType -> Maybe Text -> D.DiscordHandler ()
updateStatus activityType mactivity =
    D.sendCommand $ D.UpdateStatus $ D.UpdateStatusOpts
        { D.updateStatusOptsSince      = Nothing
        , D.updateStatusOptsActivities = case mactivity of
            Just activityName -> [D.Activity
                { D.activityName          = activityName
                , D.activityType          = activityType
                , D.activityUrl           = Nothing
                , D.activityCreatedAt     = 0
                , D.activityTimeStamps    = Nothing
                , D.activityApplicationId = Nothing
                , D.activityDetails       = Nothing
                , D.activityState         = Nothing
                , D.activityEmoji         = Nothing
                , D.activityParty         = Nothing
                , D.activityInstance      = Nothing
                , D.activityFlags         = Nothing
                , D.activityButtons       = Nothing
                }]
            Nothing -> []
        , D.updateStatusOptsNewStatus  = D.UpdateStatusOnline
        , D.updateStatusOptsAFK        = False
        }

type Predicate = D.Message -> D.DiscordHandler Bool

infixl 2 |||
(|||) :: Predicate -> Predicate -> Predicate
(pred1 ||| pred2) message = do
    p1 <- pred1 message
    p2 <- pred2 message
    pure $ p1 || p2

infixl 3 &&&
(&&&) :: Predicate -> Predicate -> Predicate
(pred1 &&& pred2) message = do
    p1 <- pred1 message
    p2 <- pred2 message
    pure $ p1 && p2

isFromSelf :: Predicate
isFromSelf message = do
    cache <- D.readCache
    pure $ D.userId (D.cacheCurrentUser cache) == D.userId
        (D.messageAuthor message)

isFromUser :: D.UserId -> Predicate
isFromUser userId message = pure $ D.userId (D.messageAuthor message) == userId

isFromBot :: Predicate
isFromBot m = pure $ D.userIsBot (D.messageAuthor m)

isCommand :: Text -> Text -> Predicate
isCommand prefix command message = do
    fromBot <- isFromBot message
    if fromBot
        then pure False
        else do
            (   messageEquals (prefix <> command)
                ||| messageStartsWith (prefix <> command <> " ")
                )
                message

messageStartsWith :: Text -> Predicate
messageStartsWith text =
    pure . (text `T.isPrefixOf`) . T.toLower . D.messageContent

messageEquals :: Text -> Predicate
messageEquals text = pure . (text ==) . T.toLower . D.messageContent

messageContains :: Text -> Predicate
messageContains text = pure . (text `T.isInfixOf`) . T.toLower . D.messageContent

writeLog :: Text -> D.DiscordHandler ()
writeLog l = do
    h <- ask
    liftIO $ writeChan (D.discordHandleLog h) l

writeError :: Text -> D.DiscordHandler ()
writeError e = do
    h <- ask
    liftIO $ writeChan (D.discordHandleLog h) $ "Error: " <> e

restCall :: (FromJSON a, D.Request (r a)) => r a -> D.DiscordHandler ()
restCall request = do
    r <- D.restCall request
    case r of
        Right _   -> pure ()
        Left  err -> writeError $ T.pack $ show err

replyTo :: D.Message -> Text -> D.DiscordHandler ()
replyTo replyingTo =
    createMessage (D.messageChannelId replyingTo) (Just $ D.messageId replyingTo)

createMessage
    :: D.ChannelId -> Maybe D.MessageId -> Text -> D.DiscordHandler ()
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

createGuildBan :: D.GuildId -> D.UserId -> Text -> D.DiscordHandler ()
createGuildBan guildId userId banMessage = restCall $ D.CreateGuildBan
    guildId
    userId
    (D.CreateGuildBanOpts Nothing (Just banMessage))
