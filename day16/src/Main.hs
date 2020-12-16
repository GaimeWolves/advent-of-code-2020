module Main where

import Data.IntSet (IntSet, delete, empty, insert, member, size, toList)
import Data.List (isPrefixOf, length, product)
import Data.List.Split (splitOn)
import System.Environment (getArgs)

type ValidRange = [(Int, Int)]

type Attribute = (String, ValidRange)

type Ticket = [Int]

readFile' :: IO ([Attribute], Ticket, [Ticket])
readFile' = do
  [file] <- getArgs
  cont <- readFile file

  let [specPart, myTicketPart, ticketsPart] = splitOn "\n\n" cont
      specStrs = map (splitOn ": ") (lines specPart)
      specRanges = map (map toRange . splitOn " or " . last) specStrs
      specNames = map head specStrs
      specs = zip specNames specRanges
      myTicket = map read (splitOn "," (last (lines myTicketPart)))
      tickets = map (map read . splitOn ",") (tail (lines ticketsPart))
   in return (specs, myTicket, tickets)
  where
    toRange rStr = (a, b)
      where
        [a, b] = map read (splitOn "-" rStr)

isFieldValidByAttrib :: Int -> Attribute -> Bool
isFieldValidByAttrib f (_, rs) = any (\(a, b) -> f >= a && f <= b) rs

isFieldValid :: [Attribute] -> Int -> Bool
isFieldValid v f = any (isFieldValidByAttrib f) v

isValid :: [Attribute] -> Ticket -> Bool
isValid v = all (isFieldValid v)

getScanningError :: [Attribute] -> Ticket -> Int
getScanningError v t = sum (filter (not . isFieldValid v) t)

getTotalScanningError :: [Attribute] -> [Ticket] -> Int
getTotalScanningError v ts = sum (map (getScanningError v) ts)

getPossibilities :: [Attribute] -> [Ticket] -> [IntSet]
getPossibilities v t = map (\fI -> foldr insert empty (filter (\aI -> all (\t -> isFieldValidByAttrib (t !! fI) (v !! aI)) t) fIdxs)) fIdxs
  where
    fIdxs = [0 .. length v - 1]

solveAttributes :: [Attribute] -> [IntSet] -> [Attribute]
solveAttributes v p = map ((!!) v . head . toList) (reducePossibilites 0 p)
  where
    reducePossibilites i p
      | all (\p -> size p == 1) p = p
      | skip = reducePossibilites ((i + 1) `mod` length p) p
      | otherwise = reducePossibilites ((i + 1) `mod` length p) p'
      where
        skip = not (any (\s -> member i s && size s == 1) p)
        p' = map (\s -> if size s > 1 then delete i s else s) p

getPartTwo :: [Attribute] -> Ticket -> Int
getPartTwo v t = product (map (t !!) (filter (\i -> "departure" `isPrefixOf` fst (v !! i)) idxs))
  where
    idxs = [0 .. length t - 1]

main :: IO ()
main = do
  (v, myT, ts) <- readFile'
  print (getTotalScanningError v ts)

  let poss = getPossibilities v (filter (isValid v) ts)
      ordAttr = solveAttributes v poss
   in do
        print (getPartTwo ordAttr myT)