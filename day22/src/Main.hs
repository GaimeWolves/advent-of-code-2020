module Main where

import Data.List.Split (splitOn)
import Data.Sequence as Seq
  ( Seq (Empty, (:<|)),
    foldrWithIndex,
    fromList,
    length,
    null,
    take,
    (|>),
  )
import System.Environment (getArgs)

type Deck = Seq.Seq Int

readFile' :: IO (Deck, Deck)
readFile' = do
  [file] <- getArgs
  cont <- readFile file

  let [p1, p2] = map (Seq.fromList . map read . tail . lines) (splitOn "\n\n" cont)
   in return (p1, p2)

step :: (Deck, Deck) -> (Deck, Deck)
step (c1 :<| cs1, c2 :<| cs2)
  | c1 > c2 = (cs1 |> c1 |> c2, cs2)
  | otherwise = (cs1, cs2 |> c2 |> c1)

play :: (Deck, Deck) -> (Deck, Deck)
play g@(p1, p2)
  | Seq.null p1 || Seq.null p2 = g
  | otherwise = play $ step g

recPlay :: (Deck, Deck) -> [(Deck, Deck)] -> (Deck, Deck, Bool)
recPlay (Empty, p2) _ = (Empty, p2, False) -- player 2 won
recPlay (p1, Empty) _ = (p1, Empty, True) -- player 1 won
recPlay g@(c1 :<| cs1, c2 :<| cs2) h
  | g `elem` h = (fst g, snd g, True) -- position already seen -> return immediatly
  | Seq.length cs1 >= c1 && Seq.length cs2 >= c2 -- sub game playable
    =
    let p1 = Seq.take c1 cs1
        p2 = Seq.take c2 cs2
        (_, _, p1Won) = recPlay (p1, p2) [] -- play sub game
        g' = if p1Won then (cs1 |> c1 |> c2, cs2) else (cs1, cs2 |> c2 |> c1) -- step accordingly
        h' = g : h
     in recPlay g' h'
  | otherwise -- step normally
    =
    let g' = step g
        h' = g : h
     in recPlay g' h'

part1 :: (Deck, Deck) -> Int
part1 g
  | Seq.null p2 = score p1
  | Seq.null p1 = score p2
  where
    (p1, p2) = play g
    score p = Seq.foldrWithIndex (\i c acc -> acc + (Seq.length p - i) * c) 0 p

part2 :: (Deck, Deck) -> Int
part2 g
  | p1Won = score p1
  | otherwise = score p2
  where
    (p1, p2, p1Won) = recPlay g []
    score p = Seq.foldrWithIndex (\i c acc -> acc + (Seq.length p - i) * c) 0 p

main :: IO ()
main = do
  game <- readFile'

  print (part1 game)
  print (part2 game)