module Main where

import qualified Data.IntMap as Map
import Data.List
import Data.Maybe
import System.Environment (getArgs)

readFile' :: String -> IO [Int]
readFile' f = do
  contents <- readFile f
  let ns = map read (lines contents)
   in return (sort (0 : (maximum ns + 3) : ns))

calcDiffs :: [Int] -> [Int]
calcDiffs [_] = []
calcDiffs ns@(a : b : _) = (b - a) : calcDiffs (tail ns)

main :: IO ()
main = do
  [file] <- getArgs
  ns <- readFile' file

  let ds = calcDiffs ns
      gs = group (sort ds)
   in print (length (head gs) * length (last gs))

  {- memoization in haskell is hard :/ -}
  let len = length ns - 1
      idxs = [0 .. len]
      {- precalculate everything thats possible -}
      next = map (\idx -> filter (\x -> (ns !! x - ns !! idx) > 0 && (ns !! x - ns !! idx) <= 3) idxs) idxs
      calcWays idx =
        let calc idx'
              | idx' == len = 1
              | otherwise = sum (map calcWays (next !! idx'))
         in map calc idxs !! idx -- saves the calculated value for after each indexation into the map
   in print (calcWays 0)