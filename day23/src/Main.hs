module Main where

import Data.Char (ord)
import Data.List as List (map)
import Data.Map as Map (Map, empty, insert, size, (!))
import Data.Maybe ()
import System.Environment (getArgs)

type Lookup = Map Int Int

readInput :: IO [Int]
readInput = do
  [input] <- getArgs
  return (List.map (\c -> ord c - ord '0') input)

toMap :: [Int] -> Lookup
toMap ns = recToMap Map.empty (last ns) ns
  where
    recToMap m p [] = Map.insert p (head ns) m
    recToMap m p (v : vs) = recToMap (Map.insert p v m) v vs

take3 :: Int -> Lookup -> (Lookup, (Int, Int, Int))
take3 v m = (m', (a, b, c))
  where
    a = m ! v
    b = m ! a
    c = m ! b
    n = m ! c
    m' = Map.insert v n m

findNext :: Int -> Int -> Int -> Int -> Int -> Int
findNext a b c size = recFindNext
  where
    recFindNext i
      | i == 0 = recFindNext size
      | i == a || i == b || i == c = recFindNext (i - 1)
      | otherwise = i

insertCups :: Lookup -> Int -> Int -> Int -> Lookup
insertCups m a c dest =
  let prev = dest
      next = m ! dest
      m' = Map.insert prev a $ Map.insert c next m
   in m'

step :: (Int, Lookup) -> (Int, Lookup)
step (v, m) =
  let (m', (a, b, c)) = take3 v m
      dest = findNext a b c (Map.size m) (v - 1)
      m'' = insertCups m' a c dest
      next = m'' ! v
   in (next, m'')

runFor :: (Int, Lookup) -> Int -> (Int, Lookup)
runFor s 0 = s
runFor s i = runFor (step s) (i - 1)

part1 :: (Int, Lookup) -> String
part1 (_, m) = recGetOrder 1
  where
    recGetOrder v
      | n == 1 = show v
      | v == 1 = recGetOrder n
      | otherwise = show v ++ recGetOrder n
      where
        n = m ! v

part2 :: (Int, Lookup) -> Int
part2 (_, m) = a * b
  where
    a = m ! 1
    b = m ! a

main :: IO ()
main = do
  ns@(v : _) <- readInput

  let s = (v, toMap ns)
      s' = runFor s 100
   in print (part1 s')

  let ns' = ns ++ [10 .. 1000000]
      s = (v, toMap ns')
      s' = runFor s 10000000
   in print (part2 s')