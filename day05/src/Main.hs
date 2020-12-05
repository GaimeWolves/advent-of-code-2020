module Main where

import System.Environment ( getArgs )
import Data.List.Split ( chunksOf )

binToDec :: [Bool] -> Int
binToDec = foldr (\x y -> fromEnum x + 2 * y) 0 . reverse

toBools :: [String] -> ([Bool], [Bool])
toBools [r, c] = (map (== 'B') r, map (== 'R') c)

readPass :: String -> (Int, Int)
readPass s = (binToDec r, binToDec c)
  where (r, c) = toBools (chunksOf 7 s)

readFile' :: String -> IO [(Int, Int)]
readFile' f = do
  contents <- readFile f
  return (map readPass (lines contents))

calcSeatID :: (Int, Int) -> Int
calcSeatID (r, c) = r * 8 + c

main :: IO ()
main = do
  [file] <- getArgs
  passes <- readFile' file

  print (maximum (map calcSeatID passes))

  let ids = map calcSeatID passes
      max' = maximum ids
      min' = minimum ids
    in print (head (filter (\x -> (x + 1) `notElem` ids && not (x == max' || x == min')) ids) + 1)