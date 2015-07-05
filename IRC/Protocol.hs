{-# LANGUAGE OverloadedStrings #-}

module IRC.Protocol where

import Control.Applicative
import Text.Printf
import Data.Text
import Data.Maybe
import Data.Attoparsec.Text as P

data Command = 
    PRIVMSG String String
  | PING    String
  | PONG    String
  | NOTICE  String String
  | JOIN    String
  | NICK    String
  | USER    String String
  | PART    String String
  | Other   String String

data Host = 
    Server { hostname :: String }
  | User   { nick     :: String
           , username :: String
           , hostname :: String }

data Message = 
    ProtocolMessage Command
  | ServerMessage   Host Command
  | UserMessage     Host Command
  | ProtocolError   String

class ProtocolElement a where
    render :: a -> String

instance ProtocolElement Command where
    render (PRIVMSG a b) = printf "PRIVMSG %s :%s\r\n" a b
    render (PING a)      = printf "PING :%s\r\n" a
    render (PONG a)      = printf "PONG :%s\r\n" a
    render (NOTICE a b)  = printf "PRIVMSG %s :%s\r\n" a b
    render (PART a b)    = printf "PART %s :%s\r\n" a b
    render (NICK a)      = printf "NICK %s\r\n" a
    render (USER a b)    = printf "USER %s 0 * :%s\r\n" a b
    render (JOIN a)      = printf "JOIN %s\r\n" a
    render (Other a b)   = printf "%s %s\r\n" a b

instance ProtocolElement Host where
    render (Server name) = name
    render (User n u h)  = printf "%s!%s@%s" n u h

instance ProtocolElement Message where
    render (ProtocolMessage c) = render c
    render (ServerMessage h c) = printf ":%s %s" (render h) (render c)
    render (UserMessage h c)   = printf ":%s %s" (render h) (render c)
    render (ProtocolError e)   = printf "ERROR: %s" e

parseUser :: Parser Host
parseUser = do
    nick     <- parseNick     ; char '!'
    username <- parseUsername ; char '@'
    hostname <- parseHostname
    return $ User nick username hostname

parseServer :: Parser Host
parseServer = parseHostname >>= return . Server

parseHost :: Parser Host
parseHost = parseUser <|> parseServer

parseUsername :: Parser String
parseUsername = takeWhile1 (inClass "a-zA-Z0-9_.-") >>= (return . unpack)

parseNick :: Parser String
parseNick = takeWhile1 (inClass "a-zA-Z0-9_.-") >>= (return . unpack)

parseHostname :: Parser String
parseHostname = takeWhile1 (inClass "a-zA-Z0-9_.-") >>= (return . unpack)

parseChannel :: Parser String
parseChannel = do
    hashes <- many $ char '#'
    name   <- takeWhile1 (inClass "a-zA-Z0-9") >>= (return . unpack)
    return $ hashes ++ name

-- Command parsers

parsePRIVMSG :: Parser Command
parsePRIVMSG = do
    string "PRIVMSG " ; target  <- parseChannel <|> parseNick
    string " :"       ; message <- takeText >>= (return . unpack)
    return $ PRIVMSG target message

parseNOTICE :: Parser Command
parseNOTICE = do
    string "NOTICE " ; target  <- parseChannel <|> parseNick
    string " :"      ; message <- takeText >>= (return . unpack)
    return $ NOTICE target message

parsePART :: Parser Command
parsePART = do
    string "PART " ; target  <- parseChannel
    string " :"    ; message <- takeText >>= (return . unpack)
    return $ PART target message

parseJOIN :: Parser Command
parseJOIN = string "JOIN :" >> parseChannel >>= return . JOIN

parsePING :: Parser Command
parsePING = string "PING :" >> takeText >>= return . PING . unpack

parsePONG :: Parser Command
parsePONG = string "PING :" >> takeText >>= return . PONG . unpack

parseNICK :: Parser Command
parseNICK = string "NICK " >> parseNick >>= return . NICK

parseUSER :: Parser Command
parseUSER = do
    string "USER "  ; nick <- parseNick 
    string " 0 * :" ; desc <- takeText >>= return . unpack
    return $ USER nick desc

parseOther :: Parser Command
parseOther = do
    code      <- P.count 3 digit ; char ' '
    remainder <- takeText >>= return . unpack
    return $ Other code remainder

parseCommand :: Parser Command
parseCommand = do
     parsePRIVMSG
 <|> parsePING   
 <|> parsePONG   
 <|> parseNOTICE 
 <|> parseJOIN   
 <|> parseNICK   
 <|> parseUSER
 <|> parsePART 
 <|> parseOther

parseServerMessage :: Parser Message
parseServerMessage = do
    char ':'
    hostmask <- parseHost ; char ' '
    command  <- parseCommand
    return $ case hostmask of 
               Server _   -> ServerMessage hostmask command
               User _ _ _ -> UserMessage hostmask command

parseServerCommand :: Parser Message
parseServerCommand = parseCommand >>= \c -> return $ ProtocolMessage c

parseProtocol :: Parser Message
parseProtocol = parseServerMessage <|> parseServerCommand

parse :: String -> Message
parse x = either ProtocolError id $ parseOnly parseProtocol (pack x)

