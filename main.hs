module Main where

import System.IO
import Data.Map
import Data.Either
import System.Random
import Control.Concurrent.Async
import qualified Data.Text.IO as TextIO
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B


import Data.Aeson

import Agent
import Parser
import Markov
import Corpus

logFile :: FilePath
logFile = "input.log"

dbFile :: FilePath
dbFile = "db.json"

main :: IO ()
main = do
    db <- gendb
    a1 <- async $ agent "shmookey" db
    a2 <- async $ agent "grug" db
    a3 <- async $ agent "Zanchey" db
    a4 <- async $ agent "trs80" db
    a5 <- async $ agent "cephei" db
    a6 <- async $ agent "Eskilla" db
    a7 <- async $ agent "JamesT" db
    a8 <- async $ agent "bob" db
    a9 <- async $ agent "sjy" db
    a10 <- async $ agent "nitre" db
    a11 <- async $ agent "tpg" db
    a12 <- async $ agent "The_Dan" db
    a13 <- async $ agent "froglet" db
    a14 <- async $ agent "Nick" db
    a15 <- async $ agent "Frenchie" db
    a16 <- async $ agent "Pommers" db
    wait a1
    return ()

gendb :: IO (Map String User)
gendb = do
    putStr "Generating database... " ; hFlush stdout
    messages <- parseFile logFile
    db <- return $ corpus messages
    B.writeFile dbFile . encode $ db
    putStrLn "done." ; hFlush stdout
    return db

