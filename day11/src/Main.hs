module Main where

import System.Environment (getArgs)

data Seat = Floor | Empty | Occupied deriving (Show, Eq)

readFile' :: IO [[Seat]]
readFile' = do
  [file] <- getArgs
  contents <- readFile file
  return (map (map (\c -> if c == 'L' then Empty else Floor) . filter (/= '\r')) (lines contents))

at :: [[Seat]] -> (Int, Int) -> Seat
at m (x, y)
  | (x < 0) || (x >= w) || (y < 0) || (y >= h) = Floor
  | otherwise = (m !! y) !! x
  where
    h = length m
    w = length (head m)

adj :: (Int, Int) -> [(Int, Int)]
adj (x, y) =
  [ (x - 1, y - 1),
    (x, y - 1),
    (x + 1, y - 1),
    (x - 1, y),
    (x + 1, y),
    (x - 1, y + 1),
    (x, y + 1),
    (x + 1, y + 1)
  ]

next :: Seat -> [Seat] -> Seat
next seat adjs
  | seat == Empty && cnt == 0 = Occupied
  | seat == Occupied && cnt >= 4 = Empty
  | otherwise = seat
  where
    cnt = length (filter (== Occupied) adjs)

step :: [[Seat]] -> [[Seat]]
step m = [[next (m `at` (x, y)) (map (at m) (adj (x, y))) | x <- [0 .. length (head m) - 1]] | y <- [0 .. length m - 1]]

adj' :: [[Seat]] -> (Int, Int) -> [Seat]
adj' m (x, y) =
  [ find 0 (-1),
    find 1 (-1),
    find 1 0,
    find 1 1,
    find 0 1,
    find (-1) 1,
    find (-1) 0,
    find (-1) (-1)
  ]
  where
    find xOff yOff
      | (x' < 0) || (x' >= w) || (y' < 0) || (y' >= h) = Floor
      | at m p' /= Floor = at m p'
      | otherwise = find (xOff + signum xOff) (yOff + signum yOff)
      where
        p'@(x', y') = (x + xOff, y + yOff)
        h = length m
        w = length (head m)

next' :: Seat -> [Seat] -> Seat
next' seat adjs
  | seat == Empty && cnt == 0 = Occupied
  | seat == Occupied && cnt >= 5 = Empty
  | otherwise = seat
  where
    cnt = length (filter (== Occupied) adjs)

step' :: [[Seat]] -> [[Seat]]
step' m = [[next' (m `at` (x, y)) (adj' m (x, y)) | x <- [0 .. length (head m) - 1]] | y <- [0 .. length m - 1]]

showBoard :: [[Seat]] -> String
showBoard m = unlines (map (map toChar) m)
  where
    toChar Floor = '.'
    toChar Empty = 'L'
    toChar Occupied = '#'

recurseUntilStable :: [[Seat]] -> ([[Seat]] -> [[Seat]]) -> Int
recurseUntilStable m fn
  | m == m' = sum (map (foldr (\x acc -> if x == Occupied then acc + 1 else acc) 0) m)
  | otherwise = recurseUntilStable m' fn
  where
    m' = fn m

main :: IO ()
main = do
  m <- readFile'
  print (recurseUntilStable m step)
  print (recurseUntilStable m step')
