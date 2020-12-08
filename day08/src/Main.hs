module Main where

import Control.Lens (element, (.~))
import Data.IntSet (IntSet, empty, insert, member)
import System.Environment (getArgs)

data EmuState = EmuState
  { acc :: Int, -- Accumulator
    pc :: Int -- Program counter
  }
  deriving (Show)

initState :: EmuState
initState = EmuState {acc = 0, pc = 0}

readFile' :: String -> IO [(String, Int)]
readFile' f = do
  contents <- readFile f
  let ls = lines contents
      ws = map words ls
   in return (map (\[s, n] -> (s, read (if head n == '+' then tail n else n))) ws)

emulateInstr :: (String, Int) -> EmuState -> EmuState
emulateInstr ("nop", _) s = s {pc = pc s + 1}
emulateInstr ("acc", v) s = s {acc = acc s + v, pc = pc s + 1}
emulateInstr ("jmp", v) s = s {pc = pc s + v}
emulateInstr _ _ = error "Invalid instruction"

emulate :: IntSet -> [(String, Int)] -> EmuState -> (Int, Bool)
emulate set instrSet state =
  if pc state < length instrSet
    then
      if member (pc state) set
        then (acc state, False)
        else
          let set' = Data.IntSet.insert (pc state) set
              state' = emulateInstr instr state
           in emulate set' instrSet state'
    else (acc state, True)
  where
    instr = instrSet !! pc state

changeInstr :: [(String, Int)] -> Int -> ([(String, Int)], Bool)
changeInstr instrSet idx
  | idx >= length instrSet = (instrSet, True)
  | otherwise = ((element idx .~ fun instr) instrSet, False)
  where
    instr = instrSet !! idx
    fun i@(n, v)
      | n == "nop" = ("jmp", v)
      | n == "jmp" = ("nop", v)
      | otherwise = i

findFix :: [(String, Int)] -> Int -> Int
findFix instrSet idx
  | end = 0
  | terminated = acc'
  | otherwise = findFix instrSet (idx + 1)
  where
    state = initState
    (newInstrSet, end) = changeInstr instrSet idx
    (acc', terminated) = emulate Data.IntSet.empty newInstrSet state

main :: IO ()
main = do
  [file] <- getArgs
  instr <- readFile' file

  print (fst (emulate Data.IntSet.empty instr initState))
  print (findFix instr 0)