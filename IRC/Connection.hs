module IRC.Connection (Config(Config), startIRC) where

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Exception
import Control.Monad.State
import Data.List
import Network
import System.IO
import Text.Printf

import IRC.Protocol hiding (nick)

data Config = 
    Config { server  :: String
           , port    :: Int
           , nick    :: String
           , channel :: String
           , onReady :: Chan Message -> Chan Command -> IO ()
           }

startIRC :: Config -> IO ()
startIRC conf = bracket (connect conf) hClose (run conf)

run :: Config -> Handle -> IO ()
run conf sock =
    do chOut <- newChan
       chIn  <- newChan
       forkIO $ listener sock chIn
       forkIO $ writer sock chOut
       writeChan chOut $ NICK (nick conf)
       writeChan chOut $ USER (nick conf) (nick conf)
       forever $ readChan chIn >>= (process conf chIn chOut)

process :: Config -> Chan Message -> Chan Command -> Message -> IO ()
process conf chIn chOut msg =
    case msg of
        ProtocolMessage (PING s) -> send $ PONG s
        ServerMessage _ (NOTICE "Auth" s) | "Welcome" `isPrefixOf` s -> doJoin
        _ -> return ()
    where doJoin = do inDup <- dupChan chIn
                      send   $ JOIN (channel conf)
                      forkIO $ (onReady conf) inDup chOut
                      return ()
          send = writeChan chOut

connect :: Config -> IO Handle
connect Config {server=ircServer, port=ircPort} = bracket_ started done $ do
    sock <- connectTo ircServer port
    _    <- hSetBuffering sock NoBuffering
    return sock
  where port     = PortNumber $ fromIntegral ircPort
        started  = printf "Connecting to %s..." ircServer >> hFlush stdout
        done     = putStrLn "done."

listener :: Handle -> Chan Message -> IO ()
listener sock ch = forever $ do
    line <- hGetLine sock
    printf "<<< %s\n" line
    msg  <- return $ parse line
    writeChan ch msg
    case msg of ProtocolError e -> printf "%s\n" e
                _ -> return ()
    hFlush stdout

writer :: Handle -> Chan Command -> IO ()
writer sock ch = forever $ do
    cmd  <- readChan ch
    msg  <- return $ render cmd
    hPrintf sock "%s\r\n" msg
    printf       ">>> %s"   msg
    hFlush stdout

