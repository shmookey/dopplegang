--{-# LANGUAGE OverloadedStrings #-}

module DB where

import Data.Map
import Data.Either
import qualified Data.Text.IO as TextIO
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B

import Data.Aeson

import Parser
import Markov
import Corpus

logFile :: FilePath
logFile = "input.log"

dbFile :: FilePath
dbFile = "db.json"

main :: IO ()
main = do
  infile <- TextIO.readFile logFile
  let (db, errs) = buildDB infile
  B.writeFile dbFile $ encode db
  putStrLn $ show errs

buildDB :: T.Text -> (Map String User, [String])
buildDB s = (corpus msgs, errs) 
            where (errs,msgs) = partitionEithers . Prelude.map parseLine $ T.lines s


