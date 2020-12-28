module Main where

import Data.List as List
import Data.Map as Map
import Data.Maybe
import System.Environment (getArgs)

data Direction = NE | E | SE | SW | W | NW deriving (Show)

type Point = (Int, Int)

toDirection :: String -> Maybe Direction
toDirection "ne" = Just NE
toDirection "e" = Just E
toDirection "se" = Just SE
toDirection "sw" = Just SW
toDirection "w" = Just W
toDirection "nw" = Just NW
toDirection s = Nothing

move :: Point -> Direction -> Point
move (q, r) NE = (q + 1, r - 1)
move (q, r) E = (q + 1, r)
move (q, r) SE = (q, r + 1)
move (q, r) NW = (q, r - 1)
move (q, r) W = (q - 1, r)
move (q, r) SW = (q - 1, r + 1)

readLine :: String -> [Direction]
readLine [] = []
readLine s =
  let d2 = toDirection (List.take 2 s)
      d1 = toDirection (List.take 1 s)
      d = if isJust d2 then fromMaybe E d2 else fromMaybe E d1
      s' = if isJust d2 then List.drop 2 s else List.drop 1 s
   in d : readLine s'

readFile' :: IO [[Direction]]
readFile' = do
  [file] <- getArgs
  cont <- readFile file

  return (List.map readLine $ lines cont)

calcPos :: [Direction] -> Point
calcPos = List.foldl' move (0, 0)

getFlippedTiles :: [[Direction]] -> [Point]
getFlippedTiles = List.foldl' insert []
  where
    insert ps ds
      | p `elem` ps = List.delete p ps
      | otherwise = p : ps
      where
        p = calcPos ds

getBounds :: [Point] -> (Point, Point)
getBounds ps = ((minQ - 1, maxQ + 1), (minR - 1, maxR + 1))
  where
    minQ = fst $ minimumBy (\(q1, _) (q2, _) -> compare q1 q2) ps
    maxQ = fst $ maximumBy (\(q1, _) (q2, _) -> compare q1 q2) ps
    minR = snd $ minimumBy (\(_, r1) (_, r2) -> compare r1 r2) ps
    maxR = snd $ maximumBy (\(_, r1) (_, r2) -> compare r1 r2) ps

neighbours :: Point -> [Point]
neighbours (q, r) =
  [ (q + 0, r - 1),
    (q - 1, r + 0),
    (q - 1, r + 1),
    (q + 0, r + 1),
    (q + 1, r + 0),
    (q + 1, r - 1)
  ]

isBlack :: [Point] -> Point -> Int -> Bool
isBlack ps p cnt
  | p `elem` ps = cnt == 1 || cnt == 2
  | otherwise = cnt == 2

step :: [Point] -> [Point]
step ps =
  let ((minQ, maxQ), (minR, maxR)) = getBounds ps
      cnts = fromList [((q, r), List.length $ List.filter (`elem` ps) $ neighbours (q, r)) | q <- [minQ .. maxQ], r <- [minR .. maxR]]
   in List.map fst $ Map.toList $ Map.filterWithKey (isBlack ps) cnts

stepFor :: [Point] -> Int -> [Point]
stepFor ps 0 = ps
stepFor ps i = stepFor (step ps) (i - 1)

main :: IO ()
main = do
  instr <- readFile'

  let s0 = getFlippedTiles instr
      s100 = stepFor s0 100
   in do
        print (length s0)
        print (length s100)