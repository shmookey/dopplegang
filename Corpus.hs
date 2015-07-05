{-# LANGUAGE OverloadedStrings #-}

module Corpus where

import Data.Time
import Data.Aeson

import Data.Map

import Stats

data Message = Skipped
             | Msg { timestamp :: LocalTime
                   , user      :: String
                   , content   :: String
                   } deriving Show

type MarkovMap = Map String (CDF String)

data User = User { markovModel    :: MarkovMap
                 , lineLengths    :: CDF Int
                 , hourlyAverages :: [Int]
                 , daysActive     :: Int
                 } deriving Show


instance ToJSON User where
    toJSON x = 
        object [ "markovModel"    .= markovModel    x
               , "lineLengths"    .= lineLengths    x
               , "hourlyAverages" .= hourlyAverages x
               , "daysActive"     .= daysActive     x
               ]

