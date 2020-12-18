module Main where

import Data.List
import System.Environment (getArgs)

type Cube = (Int, Int, Int)

data Dimension = Dimension
  { active :: [Cube],
    xBounds :: (Int, Int),
    yBounds :: (Int, Int),
    zBounds :: (Int, Int)
  }

toChar :: [Cube] -> (Int, Int, Int) -> Char
toChar a p = if active then '#' else '.'
  where
    active = p `elem` a

instance Show Dimension where
  show (Dimension a (xMin, xMax) (yMin, yMax) (zMin, zMax)) = unlines (map printZ [zMin .. zMax])
    where
      printZ z = "z=" ++ show z ++ "\n" ++ unlines [[toChar a (x, y, z) | x <- [xMin .. xMax]] | y <- [yMin .. yMax]] ++ "\n"

readFile' :: IO Dimension
readFile' = do
  [file] <- getArgs
  cont <- readFile file

  let ls = lines cont
      w = length (head ls)
      h = length ls
      wh = w `div` 2
      hh = h `div` 2
      xB = (if even w then - wh + 1 else - wh, wh)
      yB = (if even h then - hh + 1 else - hh, wh)
      zB = (0, 0)
      a = [(fst xB + x, fst yB + y, 0) | x <- [0 .. w - 1], y <- [0 .. h - 1], (ls !! y) !! x == '#']
   in return (Dimension a xB yB zB)

-- Fun hard coding time
neighbours :: Cube -> [Cube]
neighbours (x, y, z) =
  [ (x - 1, y - 1, z - 1),
    (x - 1, y - 1, z - 0),
    (x - 1, y - 1, z + 1),
    (x - 1, y - 0, z - 1),
    (x - 1, y - 0, z - 0),
    (x - 1, y - 0, z + 1),
    (x - 1, y + 1, z - 1),
    (x - 1, y + 1, z - 0),
    (x - 1, y + 1, z + 1),
    (x - 0, y - 1, z - 1),
    (x - 0, y - 1, z - 0),
    (x - 0, y - 1, z + 1),
    (x - 0, y - 0, z - 1),
    --(x - 0, y - 0, z - 0),
    (x - 0, y - 0, z + 1),
    (x - 0, y + 1, z - 1),
    (x - 0, y + 1, z - 0),
    (x - 0, y + 1, z + 1),
    (x + 1, y - 1, z - 1),
    (x + 1, y - 1, z - 0),
    (x + 1, y - 1, z + 1),
    (x + 1, y - 0, z - 1),
    (x + 1, y - 0, z - 0),
    (x + 1, y - 0, z + 1),
    (x + 1, y + 1, z - 1),
    (x + 1, y + 1, z - 0),
    (x + 1, y + 1, z + 1)
  ]

count :: [Cube] -> Cube -> Int
count a c = length (filter (`elem` a) (neighbours c))

next :: [Cube] -> Cube -> Bool -> Bool
next as c a
  | a = cnt == 2 || cnt == 3
  | otherwise = cnt == 3
  where
    cnt = count as c

step :: Dimension -> Dimension
step (Dimension a (xMin, xMax) (yMin, yMax) (zMin, zMax)) =
  let grid = [(x, y, z) | x <- [xMin - 1 .. xMax + 1], y <- [yMin - 1 .. yMax + 1], z <- [zMin - 1 .. zMax + 1]]
      a' = filter (\p -> next a p (p `elem` a)) grid
      (xMin', _, _) = minimumBy (\(x1, _, _) (x2, _, _) -> compare x1 x2) a'
      (xMax', _, _) = maximumBy (\(x1, _, _) (x2, _, _) -> compare x1 x2) a'
      (_, yMin', _) = minimumBy (\(_, y1, _) (_, y2, _) -> compare y1 y2) a'
      (_, yMax', _) = maximumBy (\(_, y1, _) (_, y2, _) -> compare y1 y2) a'
      (_, _, zMin') = minimumBy (\(_, _, z1) (_, _, z2) -> compare z1 z2) a'
      (_, _, zMax') = maximumBy (\(_, _, z1) (_, _, z2) -> compare z1 z2) a'
   in Dimension a' (xMin', xMax') (yMin', yMax') (zMin', zMax')

main :: IO ()
main = do
  dim <- readFile'
  let dim' = foldr (\t dim -> step dim) dim [0 .. 5]
   in do
        print (length (active dim'))
        print dim'