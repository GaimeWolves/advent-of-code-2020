module Main where

import Data.List ()
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe, isJust)
import System.Environment (getArgs)
import Text.Regex.PCRE ((=~))

data Rule
  = Literal String
  | Recursive [Int] (Maybe [Int])
  | Reference Int
  deriving (Eq)

initRef :: Rule
initRef = Reference 0

parseLine :: [String] -> (Int, Rule)
parseLine [nStr, rStr]
  | '|' `elem` rStr =
    let [lRules, rRules] = map (map read . splitOn " ") (splitOn " | " rStr)
     in (n, Recursive lRules (Just rRules))
  | 'a' `elem` rStr = (n, Literal "a")
  | 'b' `elem` rStr = (n, Literal "b")
  | otherwise =
    let rules = map read $ splitOn " " rStr
     in (n, Recursive rules Nothing)
  where
    n = read nStr

toRegex :: [(Int, Rule)] -> String
toRegex rs = "^" ++ recToRegex 0 (fromMaybe initRef (lookup 0 rs)) ++ "$"
  where
    recToRegex n (Literal lit) = "(" ++ lit ++ ")"
    recToRegex n (Reference ref) = "(" ++ recToRegex ref (fromMaybe initRef (lookup ref rs)) ++ ")"
    recToRegex n (Recursive lr rr)
      | n == 8 && 8 `elem` rr' = "(" ++ recToRegex (head lr) (fromMaybe initRef (lookup (head lr) rs)) ++ "+)"
      | n == 11 && 11 `elem` rr' =
        let a = recToRegex (head lr) (fromMaybe initRef (lookup (head lr) rs))
            b = recToRegex (last lr) (fromMaybe initRef (lookup (last lr) rs))
         in "(?P<g>" ++ a ++ "(?P>g)?" ++ b ++ ")"
      | isJust rr = "(" ++ lStr ++ "|" ++ rStr ++ ")"
      | otherwise = "(" ++ lStr ++ ")"
      where
        rr' = fromMaybe [] rr
        lStr = foldr1 (++) (map (\i -> recToRegex i (fromMaybe initRef (lookup i rs))) lr)
        rStr = foldr1 (++) (maybe [] (map (\i -> recToRegex i (fromMaybe initRef (lookup i rs)))) rr)

readRegex :: [String] -> String
readRegex ls = toRegex $ map (parseLine . splitOn ": ") ls

readFile' :: IO ([String], [String])
readFile' = do
  [file] <- getArgs
  cont <- readFile file

  let [regStrs, inputs] = splitOn "\n\n" cont
   in return (lines regStrs, lines inputs)

main :: IO ()
main = do
  (rStr, iStr) <- readFile'

  let regex = readRegex rStr
   in do
        print regex
        print (length (filter (=~ regex) iStr))