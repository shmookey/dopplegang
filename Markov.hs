module Markov where

import Data.Time
import System.Random
import Control.Monad.State
import Data.Map as M
import Data.List as L
import Data.Set as S

import Stats
import Corpus

type MarkovState  = ( [String]
                    , Int
                    , StdGen
                    )
type Activity     = ( [Int] -- Posts per hour
                    , Int   -- Total active days
                    )
type UserCounters = ( Map String (Counter String) -- word transitions
                    , Counter Int                 -- line lengths
                    , Map Int (Counter Day)       -- messages per hour
                    )

combine :: Ord a => Counter a -> Counter a -> Counter a
combine = M.unionWith (+)

-- count instances of each word in a sentence being successor to the last
transitions :: [String] -> Map String (Counter String)
transitions xs = L.foldl f M.empty $ zip ("^":xs) xs
                 where f acc (p,s) = M.insertWith combine p (count s) acc

-- merge two sets of stat counters
merge :: UserCounters -> UserCounters -> UserCounters
merge (m, l, t) (m', l', t') = ( M.unionWith combine m m'
                               , combine l l'
                               , M.unionWith combine t t'
                               )

-- process a message and update stat counters
accumulate :: Map String UserCounters -> Message -> Map String UserCounters
accumulate acc Skipped = acc
accumulate acc (Msg time user msg) =
    M.insertWith merge user (trans, lengths, times) acc
    where trans    = transitions msgWords
          lengths  = count $ length msgWords
          times    = M.insert (todHour $ localTimeOfDay time) (count $ localDay time) M.empty
          msgWords = words msg

activity :: Map Int (Counter Day) -> Activity
activity m = 
    (L.map getAvg [0..23], nDays)
    where crunch days = (M.foldl (+) 0 days) `div` (M.size days)
          getAvg hour = case hour `M.lookup` m of Just x  -> crunch x
                                                  Nothing -> 0
          nDays       =  S.size . S.unions $ L.map M.keysSet (M.elems m)

corpus :: [Message] -> Map String User
corpus msgs = M.map freqs userCounts
              where userCounts      = L.foldl accumulate M.empty msgs
                    freqs (m, l, t) = User (M.map toCDF m) (toCDF l) hourlies ndays
                                      where (hourlies, ndays) = activity t

sentence :: MarkovMap -> MarkovState -> (String, StdGen)
sentence mm (ws,   0, gen) = (L.intercalate " " . reverse $ init ws, gen)
sentence mm ([],   n, gen) = sentence mm (["^"], n, gen)
sentence mm (w:ws, n, gen) = 
    sentence mm (next:w:ws, n-1, gen')    
    where followers = w `M.lookup` mm
          starters  = mm ! "^"
          (p, gen') = random gen
          next      = case followers of Just cdf -> (choose cdf p)
                                        Nothing  -> (choose starters p)

imitate :: String -> Map String User -> StdGen -> (String, StdGen)
imitate name db gen =
    (msg, gen'')
    where n            = choose l p
          (p,gen')     = random gen
          User m l _ _ = db ! name
          (msg, gen'') = sentence m ([], n, gen')


