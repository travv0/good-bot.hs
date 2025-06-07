{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types
    ( MeannessLevel
    , mkMeannessLevel
    , unMeannessLevel
    , meannessRatio
    ) where

newtype MeannessLevel = MeannessLevel Int
    deriving (Show, Read, Eq, Ord)

mkMeannessLevel :: Int -> MeannessLevel
mkMeannessLevel = MeannessLevel . min 11 . max 0

unMeannessLevel :: MeannessLevel -> Int
unMeannessLevel (MeannessLevel n) = n

meannessRatio :: MeannessLevel -> Int
meannessRatio (MeannessLevel 11) = 1
meannessRatio (MeannessLevel n)  = 2000 `div` n