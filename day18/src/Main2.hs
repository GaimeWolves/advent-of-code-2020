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
  | Expression1 AST (Maybe AST) -- Addition
  | Expression2 AST (Maybe AST) -- Multiplication
  | Expression3 AST -- Parenthesis
  | Expression4 Integer -- Value

instance Eq AST where
  (==) Calculation {} Calculation {} = True
  (==) Epsilon Epsilon = True
  (==) Expression1 {} Expression1 {} = True
  (==) Expression2 {} Expression2 {} = True
  (==) Expression3 {} Expression3 {} = True
  (==) Expression4 {} Expression4 {} = True
  (==) _ _ = False

instance Show AST where
  show Epsilon = "ε"
  show (Calculation e1 next) = "Calculation { e1=" ++ show e1 ++ ", next=" ++ show next ++ " }"
  show (Expression1 e2 next)
    | isJust next = "Expression1 { e2=" ++ show e2 ++ ", + , next=" ++ show next ++ " }"
    | otherwise = "Expression1 { e2=" ++ show e2 ++ " }"
  show (Expression2 e3 next)
    | isJust next = "Expression2 { e2=" ++ show e3 ++ ", * , next=" ++ show next ++ " }"
    | otherwise = "Expression2 { e2=" ++ show e3 ++ " }"
  show (Expression3 next) = "Expression3 { next=" ++ show next ++ " }"
  show (Expression4 v) = "Expression4 " ++ show v ++ " "

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
    (exp, ss') = readTokenization (Expression1 Epsilon Nothing, ss)

-- Expression 1
readTokenization (Expression1 {}, ss)
  | null ss' = (Expression1 exp2 Nothing, ss')
  | otherwise =
    let (opStr : ss'') = ss'
     in if opStr == "*"
          then
            let (next, ss''') = readTokenization (Expression1 Epsilon Nothing, ss'')
             in (Expression1 exp2 (Just next), ss''')
          else (Expression1 exp2 Nothing, ss')
  where
    (exp2, ss') = readTokenization (Expression2 Epsilon Nothing, ss)

-- Expression 2
readTokenization (Expression2 {}, ss)
  | null ss' = (Expression2 exp2 Nothing, ss')
  | otherwise =
    let (opStr : ss'') = ss'
     in if opStr == "+"
          then
            let (next, ss''') = readTokenization (Expression2 Epsilon Nothing, ss'')
             in (Expression2 exp2 (Just next), ss''')
          else (Expression2 exp2 Nothing, ss')
  where
    (exp2, ss') = readTokenization (Expression3 Epsilon, ss)

-- Expression 3 (implicit Expression 4)
readTokenization (Expression3 {}, s : ss)
  | s == "(" =
    let (exp, s' : ss') = readTokenization (Expression1 Epsilon Nothing, ss)
     in if s' == ")" then (Expression3 exp, ss') else error "Parenthesis mismatch"
  | isJust mNum = (Expression3 (Expression4 n), ss)
  | otherwise = error ("Expected number or '(' but got " ++ s ++ " Rem: " ++ show ss)
  where
    mNum = readMaybe s
    n = fromMaybe 0 mNum

-- Invalid
readTokenization _ = error "Invalid AST"

interpret :: AST -> [Integer]
interpret (Calculation e1 next) = interpret e1 ++ interpret next
interpret (Expression1 e2 next)
  | isJust next = [head (interpret e2) * head (interpret (fromMaybe Epsilon next))]
  | otherwise = interpret e2
interpret (Expression2 e3 next)
  | isJust next = [head (interpret e3) + head (interpret (fromMaybe Epsilon next))]
  | otherwise = interpret e3
interpret (Expression3 next) = interpret next
interpret (Expression4 v) = [v]
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