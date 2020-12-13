module Main where

import Data.List (elemIndex, find)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Math.NumberTheory.Moduli.Chinese (chineseRemainder)
import System.Environment (getArgs)

readFile' :: IO (Integer, [(Integer, Integer)])
readFile' = do
  [file] <- getArgs
  contents <- readFile file

  let [depstr, bstr] = lines contents
      bsstr = splitOn "," bstr
      filbsstr = filter (/= "x") bsstr
      bsv = map read filbsstr
      idx = map (\b -> fromIntegral (fromMaybe 0 (elemIndex b bsstr))) filbsstr
      bs = zip idx bsv
      dep = read depstr
   in return (dep, bs)

selectBus :: Integer -> [(Integer, Integer)] -> Integer
selectBus dep bs = id * (time - fromIntegral dep)
  where
    ts = map (\(_, id) -> fromMaybe 0 (find (>= fromIntegral dep) [0, id ..])) bs
    time = minimum ts
    idx = fromMaybe 0 (elemIndex time ts)
    id = snd (bs !! idx)

main :: IO ()
main = do
  (dep, bs) <- readFile'
  print (selectBus dep bs)
  print (chineseRemainder (map (\(idx, id) -> (- idx, id)) bs))