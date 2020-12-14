module Main where

import Control.Lens (element, (.~))
import Data.Bits (Bits (shiftR))
import Data.List (findIndex, foldl')
import Data.Maybe (fromMaybe, isJust)
import System.Environment (getArgs)

data Bit = Floating | Zero | One deriving (Show, Eq)

type Memory = [(Integer, [Bit])]

data Decoder = Decoder
  { memory :: Memory,
    bitmask :: [Bit]
  }

data Instruction = MemOp Integer Integer | MaskOp String deriving (Show)

instance Show Decoder where
  show (Decoder mem bitmask) = "Decoder {memory = " ++ show (map (\(i, b) -> "(" ++ show i ++ ", " ++ show (fromBits b) ++ ")") mem) ++ ", bitmask = " ++ showFromBits bitmask ++ "}"

initNum :: [Bit]
initNum = replicate 36 Zero

initDecoder :: Decoder
initDecoder =
  Decoder
    { memory = [],
      bitmask = initNum
    }

fromBits :: [Bit] -> Integer
fromBits [] = 0
fromBits (b : bs) = a * (2 ^ length bs) + fromBits bs
  where
    a = if b == Zero then 0 else 1

showFromBits :: [Bit] -> String
showFromBits = map mapper
  where
    mapper Floating = 'X'
    mapper Zero = '0'
    mapper One = '1'

toBits :: Integer -> [Bit]
toBits = genBits 0
  where
    genBits i v
      | i == 36 = []
      | v == 0 && i < 36 = genBits (i + 1) v ++ [Zero]
      | otherwise = genBits (i + 1) (v `shiftR` 1) ++ [b]
      where
        b = if even v then Zero else One

toBitMask :: String -> [Bit]
toBitMask = map mapper
  where
    mapper 'X' = Floating
    mapper '0' = Zero
    mapper '1' = One

memInsert :: Memory -> Integer -> [Bit] -> Memory
memInsert mem i x
  | isJust idx = (element (fromMaybe (-1) idx) .~ (i, x)) mem
  | otherwise = (i, x) : mem
  where
    idx = findIndex (\(idx, _) -> idx == i) mem

applyBitMask :: [Bit] -> [Bit] -> [Bit]
applyBitMask m x = zipWith apply m x
  where
    apply Floating x = x
    apply Zero _ = Zero
    apply One _ = One

runInstruction :: Decoder -> Instruction -> Decoder
runInstruction d (MemOp i x) = d {memory = memInsert (memory d) i (applyBitMask (bitmask d) (toBits x))}
runInstruction d (MaskOp m) = d {bitmask = toBitMask m}

applyBitMask' :: [Bit] -> [Bit] -> [Bit]
applyBitMask' m x = zipWith apply m x
  where
    apply Floating _ = Floating
    apply Zero x = x
    apply One _ = One

possibleAddresses :: [Bit] -> [[Bit]]
possibleAddresses = genAddress 0
  where
    genAddress i bs
      | i == 36 = [bs]
      | b == Floating = genAddress (i + 1) v1 ++ genAddress (i + 1) v2
      | otherwise = genAddress (i + 1) bs
      where
        b = bs !! i
        v1 = (element i .~ Zero) bs
        v2 = (element i .~ One) bs

memInsert' :: Memory -> [Bit] -> [Bit] -> [Bit] -> Memory
memInsert' mem m i x =
  let addrBits = possibleAddresses (applyBitMask' m i)
      addrs = map fromBits addrBits
   in foldr (\addr mem -> memInsert mem addr x) mem addrs

runInstruction' :: Decoder -> Instruction -> Decoder
runInstruction' d (MemOp i x) = d {memory = memInsert' (memory d) (bitmask d) (toBits i) (toBits x)}
runInstruction' d (MaskOp m) = d {bitmask = toBitMask m}

runProgram :: (Decoder -> Instruction -> Decoder) -> [Instruction] -> Decoder
runProgram fn = foldl' fn initDecoder

readFile' :: IO [Instruction]
readFile' = do
  [file] <- getArgs
  cont <- readFile file
  return (map readInstr (lines cont))
  where
    readInstr s
      | take 3 s == "mem" = MemOp (fst (head (reads (drop 4 s)))) (read (last (words s)))
      | otherwise = MaskOp (last (words s))

main :: IO ()
main = do
  prog <- readFile'

  let d = runProgram runInstruction prog
   in print (sum (map (\(_, b) -> fromBits b) (memory d)))

  let d = runProgram runInstruction' prog
   in print (sum (map (\(_, b) -> fromBits b) (memory d)))
