module Stats where

import System.Random
import Data.Maybe
import Data.Map.Strict as M
import Data.List as L

type Counter a = Map a Int
type CDF a     = [(a, Float)]

toCDF :: Counter a -> CDF a
toCDF xs = reverse . snd $ L.foldl (cumProb n) (0,[]) sorted
           where n       = sum . snd . unzip $ toList xs
                 cmp a b = (snd a) `compare` (snd b)
                 sorted  = sortBy cmp $ toList xs

cumProb :: Int -> (Float, CDF a) -> (a, Int) -> (Float, CDF a)
cumProb n = \(pCum, acc) (k, v) -> let p = pCum + (fromIntegral v) / (fromIntegral n) 
                                   in  (p, (k,p):acc)

count :: Ord a => a -> Counter a
count x = M.insert x 1 empty

pick :: CDF a -> StdGen -> (a, StdGen)
pick cdf g = (r,g')
             where match = find (\x -> p < snd x) cdf
                   r     = fst $ fromMaybe (last cdf) match
                   (p,g') = random g

choose :: CDF a -> Float -> a
choose cdf p = fst $ fromMaybe (last cdf) match
               where match = find (\x -> p < snd x) cdf

