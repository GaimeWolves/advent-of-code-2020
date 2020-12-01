module Main where

import Data.List
import System.Environment

-- Part 1
sumCorrect :: (Int, Int) -> Bool
sumCorrect (a, b) = (a + b) == 2020

{- Explanation to myself:
(x:ys) <- tails l: put the head of the list in x and the tail in ys for every sublist
y <- ys: for every x, get every possible y in the sublist -}
pairs :: [a] -> [(a, a)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

mult2 :: (Int, Int) -> Int
mult2 (a, b) = a * b

rdFile :: String -> IO [Int]
rdFile file = do
  contents <- readFile file
  return (map read (lines contents))

-- Part 2
sumCorrect2 :: (Int, Int, Int) -> Bool
sumCorrect2 (a, b, c) = (a + b + c) == 2020

triplets :: [a] -> [(a, a, a)]
triplets l = [(x,y,z) | (x:ys) <- tails l, (y:zs) <- tails ys, z <- zs]

mult3 :: (Int, Int, Int) -> Int
mult3 (a, b, c) = a * b * c

main :: IO ()
main = do
  [file] <- getArgs
  ints <- rdFile file -- get list of numbers in file

  {- for every pair/triplet which sums to 2020, calculate the product and print the results -}
  print (map mult2 (filter sumCorrect (pairs ints))) -- part 1
  print (map mult3 (filter sumCorrect2 (triplets ints))) -- part 2