module Agent where

--import Control.Concurrent
import Control.Concurrent.Lifted
import Control.Exception
import Control.Monad.State
import Data.List
import Data.Map
import Network
import System.IO
import System.Random
import Text.Printf

import qualified IRC.Protocol as IRC
import IRC.Connection
import Corpus
import Markov

server  = "localhost" --"irc.ucc.asn.au"
port    = 6667
channel = "#Mucc"

data Agent = Agent { chIn  :: Chan IRC.Message
                   , chOut :: Chan IRC.Command
                   , gen   :: StdGen
                   }

type Context = StateT Agent IO

io = liftIO
forkState m s = fork $ runStateT m s >> return ()
putGen g a = a { gen = g }

agent :: String -> Map String User -> IO ()
agent nick db = startIRC $ Config server port ('M':nick) channel (run nick db)

run :: String -> Map String User -> Chan IRC.Message -> Chan IRC.Command -> IO ()
run nick db chIn chOut = do 
    g1        <- newStdGen ; g2 <- newStdGen
    let state  = Agent chIn chOut
    forkState (scheduler nick db) (state g1) 
    forkState (responder nick db) (state g2) 
    return ()

scheduler :: String -> Map String User -> Context ()
scheduler nick db = forever $ do
    g        <- gets gen
    (m, g')  <- return $ imitate nick db g
    (t, g'') <- return $ randomR (0,300) g'
    modify $ putGen g''
    say m
    io  $ threadDelay ((120+t) * 1000000)
    return ()

responder :: String -> Map String User -> Context ()
responder nick db = return ()

-- says something to the channel after a random delay up to 2 minutes
say :: String -> Context ()
say msg = do
    g           <- gets gen
    ch          <- gets chOut
    let (t, g')  = randomR (0,240) g
    modify $ putGen g'
    fork $ do threadDelay (t * 1000000)
              writeChan ch (IRC.PRIVMSG channel msg)
    return ()
