module Main where

import Data.List.Split
  ( dropBlanks,
    dropDelims,
    onSublist,
    oneOf,
    split,
  )
import Data.Maybe (fromMaybe, isJust)
import System.Environment (getArgs)
import Text.Pretty.Simple ()
import Text.Read (readMaybe)

data AST
  = Calculation AST AST -- Calculation
  | Epsilon -- End
  | Expression1 AST (Integer -> Integer -> Integer) (Maybe AST) -- Addition / Multiplication
  | Expression2 AST -- Parenthesis
  | Expression3 Integer -- Value

instance Eq AST where
  (==) Calculation {} Calculation {} = True
  (==) Epsilon Epsilon = True
  (==) Expression1 {} Expression1 {} = True
  (==) Expression2 {} Expression2 {} = True
  (==) Expression3 {} Expression3 {} = True
  (==) _ _ = False

instance Show AST where
  show Epsilon = "ε"
  show (Calculation e1 next) = "Calculation { e1=" ++ show e1 ++ ", next=" ++ show next ++ " }"
  show (Expression1 e2 op next)
    | isJust next = "Expression1 { e2=" ++ show e2 ++ ", op=" ++ opStr ++ ", next=" ++ show next ++ " }"
    | otherwise = "Expression1 { e2=" ++ show e2 ++ " }"
    where
      opStr = if op 3 3 == 6 then "+" else "*" -- Professional equality check
  show (Expression2 next) = "Expression2 { next=" ++ show next ++ " }"
  show (Expression3 v) = "Expression3 " ++ show v ++ " "

-- Parses the tokenization into an AST (this is so ugly)
readTokenization :: (AST, [String]) -> (AST, [String])
readTokenization (Calculation e1 next, []) -- EOF
  | e1 == Epsilon = error "Invalid AST"
  | otherwise = (Calculation e1 Epsilon, [])
readTokenization (_, []) = error "Invalid AST" -- EOF
readTokenization (Calculation {}, ss)
  | null ss' = readTokenization (Calculation exp Epsilon, ss')
  | head ss' == "\n" =
    let (next, ss'') = readTokenization (Calculation Epsilon Epsilon, tail ss')
     in (Calculation exp next, ss'')
  | otherwise = error "Invalid AST"
  where
    (exp, ss') = readTokenization (Expression1 Epsilon (+) Nothing, ss)

-- Expression 1
readTokenization (Expression1 {}, ss)
  | null ss' = (Expression1 exp2 (+) Nothing, ss')
  | otherwise =
    let (opStr : ss'') = ss'
     in if opStr == "+" || opStr == "*"
          then
            let op = if opStr == "+" then (+) else (*)
                (next, ss''') = readTokenization (Expression1 Epsilon (+) Nothing, ss'')
             in (Expression1 exp2 op (Just next), ss''')
          else (Expression1 exp2 (+) Nothing, ss')
  where
    (exp2, ss') = readTokenization (Expression2 Epsilon, ss)

-- Expression 2 (implicit Expression 3)
readTokenization (Expression2 {}, s : ss)
  | s == "(" =
    let (exp, s' : ss') = readTokenization (Expression1 Epsilon (+) Nothing, ss)
     in if s' == ")" then (Expression2 exp, ss') else error "Parenthesis mismatch"
  | isJust mNum = (Expression2 (Expression3 n), ss)
  | otherwise = error ("Expected number or '(' but got " ++ s ++ " Rem: " ++ show ss)
  where
    mNum = readMaybe s
    n = fromMaybe 0 mNum

-- Invalid
readTokenization _ = error "Invalid AST"

interpret :: AST -> [Integer]
interpret (Calculation e1 next) = interpret e1 ++ interpret next
interpret (Expression1 e2 op next)
  | isJust next = [head (interpret e2) `op` head (interpret (fromMaybe Epsilon next))]
  | otherwise = interpret e2
interpret (Expression2 next) = interpret next
interpret (Expression3 v) = [v]
interpret Epsilon = []

readFile' :: IO AST
readFile' = do
  [file] <- getArgs
  cont <- readFile file

  let switch ")" = "("
      switch "(" = ")"
      switch c = c
      ls = map reverse (filter (not . null) (split (oneOf "\n") cont))
      pre = map switch (filter (not . null) (concatMap (split (oneOf "()")) ls))
      tks = concatMap (split (dropBlanks $ dropDelims $ onSublist " ")) pre
   in return (fst (readTokenization (Calculation Epsilon Epsilon, tks)))

main :: IO ()
main = do
  ast <- readFile'
  --pPrint ast
  print (sum $ interpret ast)