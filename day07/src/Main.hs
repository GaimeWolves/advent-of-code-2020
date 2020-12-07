module Main where

import System.Environment ( getArgs )
import Data.List.Split ( splitOn )
import Text.ParserCombinators.ReadP
    ( ReadP, eof, many1, munch, readP_to_S, satisfy )
import Data.Char ( isDigit, isSpace )
import Data.Tree ( Tree, flatten, foldTree, unfoldTree )
import Data.Maybe ( fromMaybe, isJust )
import Data.List ( nub )

readBagP :: ReadP (String, Int)
readBagP = do
  cnt <- fmap read (many1 (satisfy isDigit))
  satisfy isSpace
  bag <- munch (const True)
  eof
  return (take (length bag - (if cnt == 1 then 4 else 5)) bag, cnt)

readBag :: String -> (String, Int)
readBag pw = fst (head (readP_to_S readBagP pw))

readLine :: [String] -> (String, [(String, Int)])
readLine [b, "no other bags"] = (b, [])
readLine [b, cs] = (b, map readBag (splitOn ", " cs))

readFile' :: String -> IO [(String, [(String, Int)])]
readFile' f = do
  contents <- readFile f
  let
    ls = map (splitOn " bags contain " . init) (lines contents)
    in
      return (map readLine ls)

toTree :: [(String, [(String, Int)])] -> Tree (String, Int)
toTree bags = unfoldTree f ("shiny gold", 1)
  where
    f (rb, rcnt) = ((rb, rcnt), leafes)
      where
        fbags = filter (\(_,cs) -> isJust (lookup rb cs)) bags
        leafes = map (\(b, cs) -> (b, fromMaybe 0 (lookup rb cs))) fbags

toTree2 :: [(String, [(String, Int)])] -> Tree (String, Int)
toTree2 bags = unfoldTree f ("shiny gold", 1)
  where
    f (rb, rcnt) = ((rb, rcnt), snd bag)
      where
        bag = head (filter (\(b,_) -> b == rb) bags)

countUnique :: [(String, Int)] -> Int
countUnique fTree = length (nub (map fst fTree))

main :: IO ()
main = do
  [file] <- getArgs
  bags <- readFile' file

  let
    tree = toTree bags
    in do
      -- putStrLn (drawTree $ fmap show tree)
      print (countUnique (flatten tree) - 1)

  let
    tree = toTree2 bags
    in do
      -- putStrLn (drawTree $ fmap show tree)
      print (snd (foldTree (\b bs -> (fst b, if null bs then snd b else snd b + snd b * sum (map snd bs))) tree) - 1)