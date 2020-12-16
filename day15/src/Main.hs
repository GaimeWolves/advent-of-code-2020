module Main where

import Control.Lens (element, (.~))
import Data.HashMap as HM
import Data.List
import Data.List.Split
import Data.Maybe
import System.Environment (getArgs)

type Timeline = Map Int Int

readFile' :: IO (Timeline, Int, Int)
readFile' = do
  [file] <- getArgs
  cont <- readFile file

  let nums = Data.List.map read (splitOn "," cont)
      l = last nums
      a = length nums
      m = foldl' (\map x -> HM.insert x (size map + 1) map) HM.empty nums
   in return (m, l, a)

next :: (Timeline, Int, Int) -> (Timeline, Int, Int)
next (tl, n, age)
  | isJust maybeAge = (HM.insert n age tl, age - fromMaybe 0 maybeAge, age + 1)
  | otherwise = (HM.insert n age tl, 0, age + 1)
  where
    maybeAge = HM.lookup n tl

playUntil :: (Timeline, Int, Int) -> Int -> (Timeline, Int, Int)
playUntil state@(_, _, age) aMax
  | age == aMax = state
  | otherwise = playUntil (next state) aMax

main :: IO ()
main = do
  state <- readFile'

  let (_, p1, _) = playUntil state 2020
      (_, p2, _) = playUntil state 30000000
   in do
        print p1
        print p2