module Main where

import System.Environment (getArgs)

data Ship = Ship
  { pos :: (Int, Int), -- Position
    dir :: (Int, Int) -- Direction
  }
  deriving (Show)

newtype Waypoint = Waypoint {wPos :: (Int, Int)} deriving (Show)

data State = State
  { ship :: Ship,
    waypoint :: Waypoint
  }
  deriving (Show)

initShip :: Ship
initShip =
  Ship
    { pos = (0, 0),
      dir = (1, 0)
    }

initWaypoint :: Waypoint
initWaypoint =
  Waypoint {wPos = (10, -1)}

initState :: State
initState =
  State
    { ship = initShip,
      waypoint = initWaypoint
    }

readFile' :: IO [(Char, Int)]
readFile' = do
  [file] <- getArgs
  contents <- readFile file
  return (map (\l -> (head l, read (tail l))) (lines contents))

rot :: (Int, Int) -> (Char, Int) -> (Int, Int)
rot d@(x, y) (c, v)
  | v == 360 = d
  | v == 180 = (- x, - y)
  | c == 'L' && v == 90 = rotLeft d
  | c == 'L' && v == 270 = rotRight d
  | c == 'R' && v == 90 = rotRight d
  | c == 'R' && v == 270 = rotLeft d
  where
    rotLeft (x, y) = (y, - x)
    rotRight (x, y) = (- y, x)

step :: State -> (Char, Int) -> State
step (State s wp) i@(c, v)
  | c == 'N' = State {ship = s {pos = (x, y - v)}, waypoint = wp}
  | c == 'S' = State {ship = s {pos = (x, y + v)}, waypoint = wp}
  | c == 'E' = State {ship = s {pos = (x + v, y)}, waypoint = wp}
  | c == 'W' = State {ship = s {pos = (x - v, y)}, waypoint = wp}
  | c == 'F' = State {ship = s {pos = (x + xD * v, y + yD * v)}, waypoint = wp}
  | c == 'L' || c == 'R' = State {ship = s {dir = rot d i}, waypoint = wp}
  where
    (x, y) = pos s
    d@(xD, yD) = dir s

step' :: State -> (Char, Int) -> State
step' (State s wp) i@(c, v)
  | c == 'N' = State {waypoint = wp {wPos = (xD, yD - v)}, ship = s}
  | c == 'S' = State {waypoint = wp {wPos = (xD, yD + v)}, ship = s}
  | c == 'E' = State {waypoint = wp {wPos = (xD + v, yD)}, ship = s}
  | c == 'W' = State {waypoint = wp {wPos = (xD - v, yD)}, ship = s}
  | c == 'F' = State {ship = s {pos = (x + xD * v, y + yD * v)}, waypoint = wp}
  | c == 'L' || c == 'R' = State {waypoint = wp {wPos = rot d i}, ship = s}
  where
    (x, y) = pos s
    d@(xD, yD) = wPos wp

run :: (State -> (Char, Int) -> State) -> [(Char, Int)] -> State -> State
run _ [] s = s
run fn is s = run fn (tail is) s'
  where
    s' = fn s (head is)

manhattan :: State -> Int
manhattan (State s _) = abs x + abs y
  where
    (x, y) = pos s

main :: IO ()
main = do
  is <- readFile'
  print (manhattan (run step is initState))
  print (manhattan (run step' is initState))
