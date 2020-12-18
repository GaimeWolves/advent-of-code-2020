module Main where

import Data.List
import System.Environment (getArgs)

type Hypercube = (Int, Int, Int, Int)

data Dimension = Dimension
  { active :: [Hypercube],
    xBounds :: (Int, Int),
    yBounds :: (Int, Int),
    zBounds :: (Int, Int),
    wBounds :: (Int, Int)
  }

toChar :: [Hypercube] -> (Int, Int, Int, Int) -> Char
toChar a p = if active then '#' else '.'
  where
    active = p `elem` a

instance Show Dimension where
  show (Dimension a (xMin, xMax) (yMin, yMax) (zMin, zMax) (wMin, wMax)) = unlines (map printZW perms)
    where
      perms = [(z, w) | z <- [zMin .. zMax], w <- [wMin .. wMax]]
      printZW (z, w) = "z=" ++ show z ++ " w=" ++ show w ++ "\n" ++ unlines [[toChar a (x, y, z, w) | x <- [xMin .. xMax]] | y <- [yMin .. yMax]] ++ "\n"

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
      wB = (0, 0)
      a = [(fst xB + x, fst yB + y, 0, 0) | x <- [0 .. w - 1], y <- [0 .. h - 1], (ls !! y) !! x == '#']
   in return (Dimension a xB yB zB wB)

neighbours :: Hypercube -> [Hypercube]
neighbours c@(x, y, z, w) = [(x', y', z', w') | x' <- [x - 1 .. x + 1], y' <- [y - 1 .. y + 1], z' <- [z - 1 .. z + 1], w' <- [w - 1 .. w + 1], (x', y', z', w') /= c]

count :: [Hypercube] -> Hypercube -> Int
count a c = length (filter (`elem` a) (neighbours c))

next :: [Hypercube] -> Hypercube -> Bool -> Bool
next as c a
  | a = cnt == 2 || cnt == 3
  | otherwise = cnt == 3
  where
    cnt = count as c

step :: Dimension -> Dimension
step (Dimension a (xMin, xMax) (yMin, yMax) (zMin, zMax) (wMin, wMax)) =
  let grid = [(x, y, z, w) | x <- [xMin - 1 .. xMax + 1], y <- [yMin - 1 .. yMax + 1], z <- [zMin - 1 .. zMax + 1], w <- [wMin - 1 .. wMax + 1]]
      a' = filter (\p -> next a p (p `elem` a)) grid
      (xMin', _, _, _) = minimumBy (\(x1, _, _, _) (x2, _, _, _) -> compare x1 x2) a'
      (xMax', _, _, _) = maximumBy (\(x1, _, _, _) (x2, _, _, _) -> compare x1 x2) a'
      (_, yMin', _, _) = minimumBy (\(_, y1, _, _) (_, y2, _, _) -> compare y1 y2) a'
      (_, yMax', _, _) = maximumBy (\(_, y1, _, _) (_, y2, _, _) -> compare y1 y2) a'
      (_, _, zMin', _) = minimumBy (\(_, _, z1, _) (_, _, z2, _) -> compare z1 z2) a'
      (_, _, zMax', _) = maximumBy (\(_, _, z1, _) (_, _, z2, _) -> compare z1 z2) a'
      (_, _, _, wMin') = minimumBy (\(_, _, _, w1) (_, _, _, w2) -> compare w1 w2) a'
      (_, _, _, wMax') = maximumBy (\(_, _, _, w1) (_, _, _, w2) -> compare w1 w2) a'
   in Dimension a' (xMin', xMax') (yMin', yMax') (zMin', zMax') (wMin', wMax')

main :: IO ()
main = do
  dim <- readFile'
  let dim' = foldr (\t dim -> step dim) dim [0 .. 5]
   in do
        print dim'
        print (length (active dim'))