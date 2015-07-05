
module Parser where

import Data.Word
import Data.Time
import Data.Maybe
import Data.Attoparsec.Text as P
import Control.Applicative
import qualified Data.Text as T
import qualified Data.Text.IO as TextIO

import Corpus
import Markov

parseTimestamp :: Parser LocalTime
parseTimestamp = do
  year   <- count 4 digit ; char '-'
  month  <- count 2 digit ; char '-'
  day    <- count 2 digit ; char ' '
  hour   <- count 2 digit ; char ':'
  minute <- count 2 digit ; char ':'
  second <- count 2 digit
  return $ LocalTime { localDay       = fromGregorian (read year) (read month)  (read day)
                     , localTimeOfDay = TimeOfDay     (read hour) (read minute) (read second)
                     }

parseUsername :: Parser String
parseUsername = P.skipWhile  (inClass "@%+&") >> P.takeWhile1 (inClass "a-zA-Z0-9_") >>= return . T.unpack

parseContent :: Parser String
parseContent = P.takeText >>= return . T.unpack

parseMessage :: Parser Message
parseMessage = do
  timestamp <- parseTimestamp ; skipSpace
  username  <- parseUsername  ; skipSpace
  message   <- parseContent
  return $ Msg timestamp username message

parseUnknown :: Parser Message
parseUnknown = do line <- P.takeTill isEndOfLine
                  return Skipped

parserError :: Int -> String -> IO ()
parserError idx x = putStrLn $ "Error on line " ++ (show idx) ++ ": " ++ x

parseLine :: (Int, T.Text) -> IO Message
parseLine (idx, msg) =
    case result of Left err  -> parserError idx err >> return Skipped
                   Right msg -> return msg
    where result = parseOnly (parseMessage <|> parseUnknown) msg

parseFile :: FilePath -> IO [Message]
parseFile p = TextIO.readFile p >>= mapM parseLine . (zip [1..] . T.lines)

