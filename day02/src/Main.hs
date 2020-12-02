module Main where

import Text.ParserCombinators.ReadP
    ( ReadP, eof, many1, readP_to_S, satisfy, string )
import Data.Char ( isAlpha, isDigit, isSpace )
import System.Environment ( getArgs )

passwordP :: ReadP (Int, Int, Char, String)
passwordP = do
  low <- fmap read (many1 (satisfy isDigit))
  satisfy (== '-')
  high <- fmap read (many1 (satisfy isDigit))
  satisfy isSpace
  chr <- satisfy isAlpha
  string ": "
  password <- many1 (satisfy isAlpha)
  eof
  return (low, high, chr, password)

password :: String -> (Int, Int, Char, String)
password pw = fst (head (readP_to_S passwordP pw))

rdFile :: String -> IO [(Int, Int, Char, String)]
rdFile file = do
  contents <- readFile file
  return (map password (lines contents))

isValid :: (Int, Int, Char, String) -> Bool
isValid (low, high, chr, pw) = low <= n && n <= high
  where n = foldr (\c sum -> if c == chr then sum + 1 else sum) 0 pw

isValid2 :: (Int, Int, Char, String) -> Bool
isValid2 (i, j, chr, pw) = (i <= l && j <= l) && ((pw !! (i - 1)) == chr) /= ((pw !! (j - 1)) == chr)
  where l = length pw

main :: IO ()
main = do
  [file] <- getArgs
  pws <- rdFile file

  print (length (filter isValid pws))
  print (length (filter isValid2 pws))