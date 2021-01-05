module Main where

import Data.List
import Data.Maybe
import System.Environment (getArgs)

type Key = Integer

type LoopSize = Int

type Subject = Integer

modulo :: Integer
modulo = 20201227

baseSubject :: Integer
baseSubject = 7

step :: Integer -> Subject -> Integer
step v s = (v * s) `mod` modulo

genKey :: Subject -> LoopSize -> Key
genKey s l = iterate f 1 !! l
  where
    f x = step x s

findLoopSize :: Subject -> Key -> LoopSize
findLoopSize s k = fromMaybe 0 $ elemIndex k $ iterate f 1
  where
    f x = step x s

genEncKey :: Key -> Key -> Key
genEncKey cardKey doorKey = genKey cardKey loopSize
  where
    loopSize = findLoopSize baseSubject doorKey

readFile' :: IO (Key, Key)
readFile' = do
  [cardStr, doorStr] <- getArgs
  return (read cardStr, read doorStr)

main :: IO ()
main = do
  (cardKey, doorKey) <- readFile'

  print (genEncKey cardKey doorKey)