module Main where

import Data.List
import System.Environment (getArgs)

readFile' :: String -> IO [Integer]
readFile' f = do
  contents <- readFile f
  return (map read (lines contents))

getSubList :: Int -> Int -> [Integer] -> [Integer]
getSubList a b xs = take (b - a) (drop a xs)

getPairs :: Int -> Int -> [Integer] -> [Integer]
getPairs pSize idx dat = map (uncurry (+)) p
  where
    d = getSubList (idx - pSize) idx dat
    p = [(x, y) | (x : ys) <- tails d, y <- ys]

checkData :: Int -> Int -> [Integer] -> Integer
checkData pSize idx dat
  | idx >= length dat = -1
  | idx < pSize = error "Invalid index"
  | otherwise =
    let vs = getPairs pSize idx dat
     in if v `elem` vs
          then checkData pSize (idx + 1) dat
          else v
  where
    v = dat !! idx

findWeakList :: Int -> Int -> Integer -> [Integer] -> [Integer]
findWeakList a b w dat
  | a > b = findWeakList a (b + 1) w dat
  | b >= length dat = []
  | otherwise =
    let subList = getSubList a b dat
        v = sum subList
     in if v < w
          then findWeakList a (b + 1) w dat
          else
            if v > w
              then findWeakList (a + 1) b w dat
              else subList

main :: IO ()
main = do
  [file, pSize] <- getArgs
  dat <- readFile' file

  let wrongValue = checkData (read pSize) (read pSize) dat
      weakList = findWeakList 0 1 wrongValue dat
   in do
        print wrongValue
        print (minimum weakList + maximum weakList)