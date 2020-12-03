module Main where

import System.Environment ( getArgs )

readMap :: String -> [Bool]
readMap = map (== '#')

readFile' :: String -> IO [[Bool]]
readFile' f = do
  contents <- readFile f
  return (map readMap (lines contents))

countTrees :: [(Int, [Bool])] -> Int
countTrees s = length (filter (== True) (map (\ (s, l) -> l !! mod s (length l)) s))

countTrees1 :: [[Bool]] -> Int
countTrees1 m = countTrees (zip [0,3..] m)

{- Take every n-th element, where n is even -}
takeN :: Int -> [a] -> [a]
takeN n = map snd . filter ((== 1) . fst) . zip (cycle [1..n])

countTreesN :: [[Bool]] -> ([Int], Int) -> Int
countTreesN m (x, y) = countTrees (zip x (takeN y m))
args :: [([Int], Int)]
args = [
  ([0,1..], 1),
  ([0,3..], 1),
  ([0,5..], 1),
  ([0,7..], 1),
  ([0,1..], 2)
  ]

main :: IO ()
main = do
  [file] <- getArgs
  m <- readFile' file

  print (countTrees1 m)
  print (product (map (countTreesN m) args))