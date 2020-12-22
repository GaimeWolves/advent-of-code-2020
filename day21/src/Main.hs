module Main where

import Data.Bifunctor (Bifunctor (second))
import Data.List as List
  ( delete,
    filter,
    foldr,
    intercalate,
    length,
    map,
    null,
    sortOn,
    sum,
    union,
  )
import Data.List.Split (splitOn)
import Data.Map as Map
  ( Map,
    empty,
    filter,
    foldr,
    insert,
    keys,
    map,
    null,
    toList,
    (!),
  )
import Data.Set as Set (empty, insert, toList, union)
import System.Environment (getArgs)

type Recipe = ([String], [String])

readLine :: String -> Recipe
readLine s =
  let parts = splitOn " (contains " (init s)
      ingredients = words $ head parts
      allergens = splitOn ", " $ last parts
   in (ingredients, allergens)

readFile' :: IO [Recipe]
readFile' = do
  [file] <- getArgs
  cont <- readFile file

  let ls = lines cont
      recipes = List.map readLine ls
   in return recipes

allAllergens :: [Recipe] -> [String]
allAllergens = Set.toList . List.foldr (\(_, as) s -> Set.union s (List.foldr Set.insert Set.empty as)) Set.empty

allIngredients :: [Recipe] -> [String]
allIngredients = Set.toList . List.foldr (\(is, _) s -> Set.union s (List.foldr Set.insert Set.empty is)) Set.empty

canContain :: [Recipe] -> String -> String -> Bool
canContain r i a = all (\(is, _) -> i `elem` is) pR
  where
    pR = List.filter (\(_, as) -> a `elem` as) r

reduce :: [Recipe] -> Map.Map String [String]
reduce r = List.foldr (\i m -> Map.insert i (List.filter (canContain r i) as) m) Map.empty is
  where
    is = allIngredients r
    as = allAllergens r

solve :: Map.Map String [String] -> Map.Map String [String]
solve m
  | isSolved = m
  | otherwise =
    let m' = List.foldr (\a m -> if toDelete a then Map.map (\as -> if List.length as > 1 then List.delete a as else as) m else m) m as
     in solve m'
  where
    isSolved = all (\k -> List.length (m ! k) < 2) (Map.keys m)
    as = Map.foldr List.union [] m
    toDelete a = not $ Map.null (Map.filter (\as -> a `elem` as && length as == 1) m)

sortAllergens :: Map.Map String [String] -> [(String, String)]
sortAllergens m =
  let fm = Map.filter (not . List.null) m
      is = List.map (second head) (Map.toList fm)
      sIs = List.sortOn snd is
   in sIs

part1 :: [Recipe] -> Map.Map String [String] -> Int
part1 r m = List.sum (List.map (\(is, _) -> length $ List.filter (`elem` allergenFreeIs) is) r)
  where
    allergenFreeIs = List.filter (\i -> List.null (m ! i)) (Map.keys m)

part2 :: [(String, String)] -> String
part2 is = List.intercalate "," (List.map fst is)

main :: IO ()
main = do
  r <- readFile'

  let p = reduce r
      is = sortAllergens $ solve p
   in do
        print (part1 r p)
        print (part2 is)