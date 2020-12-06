module Main where

import System.Environment ( getArgs )
import Data.List.Split ( splitOn )
import Data.Set ( empty, insert, intersection, size, union, Set )

readPerson :: String -> Set Char
readPerson = Prelude.foldr Data.Set.insert Data.Set.empty

readGroup :: [String] -> Set Char
readGroup ss = Prelude.foldr1 Data.Set.union (Prelude.map readPerson ss)

readFile' :: String -> IO [Set Char]
readFile' f = do
  contents <- readFile f
  return (Prelude.map (readGroup . lines) (splitOn "\n\n" contents))

readGroup' :: [String] -> Set Char
readGroup' ss = Prelude.foldr1 Data.Set.intersection (Prelude.map readPerson ss)

readFile'' :: String -> IO [Set Char]
readFile'' f = do
  contents <- readFile f
  return (Prelude.map (readGroup' . lines) (splitOn "\n\n" contents))

main :: IO ()
main = do
  [file] <- getArgs

  groups <- readFile' file
  print (Prelude.foldr (\s acc -> acc + Data.Set.size s) 0 groups)

  groups' <- readFile'' file
  print (Prelude.foldr (\s acc -> acc + Data.Set.size s) 0 groups')