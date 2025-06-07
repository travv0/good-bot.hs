{-# LANGUAGE OverloadedStrings #-}

module RateLimit
    ( RateLimiter
    , createRateLimiter
    , checkRateLimit
    , RateLimitConfig(..)
    , defaultRateLimitConfig
    ) where

import           Control.Concurrent.STM         ( TVar
                                                , atomically
                                                , modifyTVar'
                                                , newTVarIO
                                                , readTVar
                                                )
import           Control.Monad                  ( when )
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.Time                      ( UTCTime
                                                , diffUTCTime
                                                , getCurrentTime
                                                )
import qualified Discord                       as D
import           Data.Text                      ( Text )

data RateLimitConfig = RateLimitConfig
    { rateLimitMaxRequests :: Int      -- Max requests per window
    , rateLimitWindowSeconds :: Int    -- Window size in seconds
    }
    deriving (Show, Read)

defaultRateLimitConfig :: RateLimitConfig
defaultRateLimitConfig = RateLimitConfig
    { rateLimitMaxRequests = 5
    , rateLimitWindowSeconds = 60
    }

type UserRequests = Map D.Snowflake [(UTCTime, Text)]
type RateLimiter = TVar UserRequests

createRateLimiter :: IO RateLimiter
createRateLimiter = newTVarIO Map.empty

-- Returns True if the request is allowed, False if rate limited
checkRateLimit :: RateLimitConfig -> RateLimiter -> D.Snowflake -> Text -> IO Bool
checkRateLimit config limiter userId command = do
    now <- getCurrentTime
    atomically $ do
        requests <- readTVar limiter
        let userReqs = Map.findWithDefault [] userId requests
        let windowStart = fromIntegral (rateLimitWindowSeconds config)
        let recentReqs = filter (\(time, _) -> diffUTCTime now time < windowStart) userReqs
        
        if length recentReqs >= rateLimitMaxRequests config
            then pure False  -- Rate limited
            else do
                -- Add the new request
                let newReqs = (now, command) : recentReqs
                modifyTVar' limiter $ Map.insert userId newReqs
                pure True  -- Request allowed

-- Clean up old entries periodically to prevent memory leaks
cleanupOldEntries :: RateLimitConfig -> RateLimiter -> IO ()
cleanupOldEntries config limiter = do
    now <- getCurrentTime
    atomically $ do
        requests <- readTVar limiter
        let windowStart = fromIntegral (rateLimitWindowSeconds config * 2)
        let cleanedRequests = Map.map (filter (\(time, _) -> diffUTCTime now time < windowStart)) requests
        modifyTVar' limiter (const $ Map.filter (not . null) cleanedRequests)