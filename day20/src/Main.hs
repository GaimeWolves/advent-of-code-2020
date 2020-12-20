module Main where

import Control.Lens
import Data.List
import Data.List.Split
import Data.Maybe
import System.Environment (getArgs)
import Text.Pretty.Simple

data Tile = Tile Int [[Bool]]

initTile = Tile 0 []

newtype Graph = Graph [[Maybe Tile]]

data Direction = North | South | West | East deriving (Eq)

data Constraints = Constraints
  { cTop :: Maybe Tile,
    cBottom :: Maybe Tile,
    cLeft :: Maybe Tile,
    cRight :: Maybe Tile
  }

instance Show Tile where
  show (Tile id dat) = "Tile " ++ show id ++ ":\n" ++ datStr
    where
      datStr = unlines (map (map (\b -> if b then '#' else '.')) dat)

readTile :: (String, [String]) -> Tile
readTile (idStr, tStr) = Tile id bs
  where
    id = fst $ head $ reads $ last $ splitOn " " idStr
    bs = map (map (== '#')) tStr

readFile' :: IO ([Tile], [[Bool]])
readFile' = do
  [inFile, smFile] <- getArgs
  cont <- readFile inFile
  smContent <- readFile smFile

  let tStrs = splitOn "\n\n" cont
      tlStrs = map (\t -> (head $ lines t, tail $ lines t)) tStrs
      sm = map (map (== '#')) (lines smContent)
   in return (map readTile tlStrs, sm)

toCentered :: Int -> (Int, Int) -> (Int, Int)
toCentered s (x, y) = (x - h, y - h)
  where
    h = s `div` 2

fromCentered :: Int -> (Int, Int) -> (Int, Int)
fromCentered s (x, y) = (x + h, y + h)
  where
    h = s `div` 2

rotate90CCW :: Int -> (Int, Int) -> (Int, Int)
rotate90CCW s (x, y) = (y, x')
  where
    x' = if even s then (- x) - 1 else - x

rotateTile90CW :: Tile -> Tile
rotateTile90CW (Tile id dat) =
  let s = length dat -- width and height
      h = s `div` 2
      cIdxs = [[toCentered s (x, y) | x <- [0 .. s - 1]] | y <- [0 .. s - 1]]
      rotCIdxs = map (map (rotate90CCW s)) cIdxs
      rotIdxs = map (map (fromCentered s)) rotCIdxs
      rotDat = map (map (\(x, y) -> dat !! y !! x)) rotIdxs
   in Tile id rotDat

flipTileX :: Tile -> Tile
flipTileX (Tile id dat) =
  let s = length dat
      idxs = [[(s - x - 1, y) | x <- [0 .. s - 1]] | y <- [0 .. s - 1]]
      flipDat = map (map (\(x, y) -> dat !! y !! x)) idxs
   in Tile id flipDat

transformations :: Tile -> [Tile]
transformations tile =
  [ tile,
    rotateTile90CW tile,
    rotateTile90CW $ rotateTile90CW tile,
    rotateTile90CW $ rotateTile90CW $ rotateTile90CW tile,
    flipTileX tile,
    rotateTile90CW $ flipTileX tile,
    rotateTile90CW $ rotateTile90CW $ flipTileX tile,
    rotateTile90CW $ rotateTile90CW $ rotateTile90CW $ flipTileX tile
  ]

checkSide :: Direction -> Tile -> Tile -> Bool
checkSide d (Tile _ a) (Tile _ b)
  | d == North = all (\i -> (a !! 0 !! i) == (b !! (s - 1) !! i)) idxs
  | d == South = all (\i -> (a !! (s - 1) !! i) == (b !! 0 !! i)) idxs
  | d == West = all (\i -> (a !! i !! 0) == (b !! i !! (s - 1))) idxs
  | d == East = all (\i -> (a !! i !! (s - 1)) == (b !! i !! 0)) idxs
  where
    s = length a
    idxs = [0 .. s - 1]

fitsConstraints :: Constraints -> Tile -> Bool
fitsConstraints (Constraints t b l r) tile = n && s && e && w
  where
    n = isNothing t || checkSide North tile (fromMaybe initTile t)
    s = isNothing b || checkSide South tile (fromMaybe initTile b)
    e = isNothing r || checkSide East tile (fromMaybe initTile r)
    w = isNothing l || checkSide West tile (fromMaybe initTile l)

-- DFS algorithm to summon demons
dfs :: Graph -> (Int, Int) -> [Tile] -> (Graph, [Tile], Bool)
dfs (Graph g) (x, y) ts
  | y == s = (Graph g, ts, True)
  | otherwise =
    let idx = [0 .. length ts - 1]
        pIdx = filter (any (fitsConstraints (Constraints t b l r)) . transformations . (!!) ts) idx -- all insertable tiles (by index)
        insTs = map (filter (fitsConstraints (Constraints t b l r)) . transformations . (!!) ts) pIdx -- all insertable transformations per tile
        remTs = map (\i -> map (ts !!) $ filter (/= i) idx) pIdx
        gs = map (map (\t -> Graph $ (element y .~ (element x ?~ t) (g !! y)) g)) insTs -- modified graphs
        fIdx = [0 .. length pIdx - 1]
        tIdxMax = if null gs then 0 else length (head gs)
        tIdx = [0 .. tIdxMax - 1]
        nextPos = if x == s - 1 then (0, y + 1) else (x + 1, y)
        res = concatMap (\i -> map (\j -> dfs (gs !! i !! j) nextPos (remTs !! i)) tIdx) fIdx
     in if null pIdx
          then (Graph g, ts, False)
          else fromMaybe (Graph g, ts, False) (find (\(_, _, solved) -> solved) res)
  where
    s = length g
    t = if y == 0 then Nothing else g !! (y - 1) !! x
    b = if y == length g - 1 then Nothing else g !! (y + 1) !! x
    l = if x == 0 then Nothing else g !! y !! (x - 1)
    r = if x == length g - 1 then Nothing else g !! y !! (x + 1)

cutBorders :: Tile -> Tile
cutBorders (Tile id dat) = Tile id dat'
  where
    dat' = [[dat !! y !! x | x <- [1 .. length dat - 2]] | y <- [1 .. length dat - 2]]

composeImage :: [[Tile]] -> Tile
composeImage ts = Tile 0 (foldr1 (++) (map forRow ts))
  where
    forRow :: [Tile] -> [[Bool]]
    forRow row = dat
      where
        (Tile _ dat) = foldr1 (\(Tile id t) (Tile _ acc) -> Tile 0 (zipWith (++) t acc)) row

checkForSeaMonster :: [[Bool]] -> [[Bool]] -> (Int, Int) -> Bool
checkForSeaMonster im sm (x, y) = all (\yOff -> all (\xOff -> isCorrect (im !! (y + yOff) !! (x + xOff)) (sm !! yOff !! xOff)) xIdxs) yIdxs
  where
    xIdxs = [0 .. length (head sm) - 1]
    yIdxs = [0 .. length sm - 1]
    isCorrect i m = (m && i) || not m

filterSeaMonster :: [[Bool]] -> [[Bool]] -> [[Bool]] -> (Int, Int) -> [[Bool]]
filterSeaMonster orig im sm p@(x, y)
  | toCut = map (map getPixel) idxs
  | otherwise = im
  where
    toCut = checkForSeaMonster orig sm p
    w = length (head sm)
    h = length sm
    s = length im
    idxs = [[(x, y) | x <- [0 .. s - 1]] | y <- [0 .. s - 1]]
    calcPixel i m = not m && i
    getPixel (x', y')
      | x' < x || x' > x + w - 1 || y' < y || y' > y + h - 1 = im !! y' !! x'
      | otherwise = calcPixel (im !! y' !! x') (sm !! (y' - y) !! (x' - x))

filterSeaMonsters :: [[Bool]] -> [[Bool]] -> [[Bool]] -> (Int, Int) -> [[Bool]]
filterSeaMonsters orig fil sm p@(x, y)
  | y > maxY = fil
  | otherwise = filterSeaMonsters orig (filterSeaMonster orig fil sm p) sm nextP
  where
    nextP = if x == maxX then (0, y + 1) else (x + 1, y)
    maxX = length orig - length (head sm)
    maxY = length orig - length sm

main :: IO ()
main = do
  (ts, sm) <- readFile'

  let s = round $ sqrt $ fromIntegral $ length ts
      initGraph = replicate s (replicate s Nothing)
      (Graph g, _, solved) = dfs (Graph initGraph) (0, 0) ts
      (Tile tl _) = fromMaybe (Tile 0 []) (head (head g))
      (Tile tr _) = fromMaybe (Tile 0 []) (head (last g))
      (Tile bl _) = fromMaybe (Tile 0 []) (last (head g))
      (Tile br _) = fromMaybe (Tile 0 []) (last (last g))
      splitImage = map (map (fromMaybe (Tile 0 []))) g
      image@(Tile _ dat) = composeImage (map (map cutBorders) splitImage)
      allImages = transformations image
      filteredImages = map (\(Tile _ dat) -> filterSeaMonsters dat dat sm (0, 0)) allImages
      roughness = map (foldr (\row acc -> acc + foldr (\pixel acc -> if pixel then acc + 1 else acc) 0 row) 0) filteredImages
   in do
        print (tl * tr * bl * br)
        print (minimum roughness)