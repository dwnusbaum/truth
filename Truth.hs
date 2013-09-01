{-# OPTIONS_GHC -Wall #-}

module Truth
    ( Term(..)
    , varSplit
    , eval
    ) where

import Data.List
import Data.Maybe

data Term
   = Var String
   | And Term Term
   | Or  Term Term
   | Xor Term Term
   | Iff Term Term
   | Imp Term Term
   | Not Term
   deriving (Eq)

instance Show (Term) where
    showsPrec _ (Var   v) = showString v
    showsPrec p (And a b) = showParen (p>0) $ showsPrec p a . showChar '&' . showsPrec (p+1) b
    showsPrec p (Or  a b) = showParen (p>0) $ showsPrec p a . showChar '|' . showsPrec (p+1) b
    showsPrec p (Xor a b) = showParen (p>0) $ showsPrec p a . showChar '^' . showsPrec (p+1) b
    showsPrec p (Iff a b) = showParen (p>0) $ showsPrec p a . showString "<->" . showsPrec (p+1) b
    showsPrec p (Imp a b) = showParen (p>0) $ showsPrec p a . showString "->" . showsPrec (p+1) b
    showsPrec p (Not   t) = showChar '~' . showsPrec (p+1) t

varSplit :: [Term] -> ([String],[Term])
varSplit [] = ([],[])
varSplit (Var v : ts) = let (s, t) = varSplit ts in (v : s, t)
varSplit (    t : ts) = ([] , t : ts)

--Evaluator

eval :: [String] -> [Term] -> [([(String,Bool)],[Bool])]
eval vars rules = map (`evalRow` rules) env
  where truthValues = makeCases $ length vars
        env = map (zip vars) truthValues

makeCases :: Int -> [[Bool]]
makeCases n
  | n < 1 = error "There was less than 1 variable found"
  | n == 1 = [ [ True ], [ False ] ]
  | otherwise = transpose $ (replicate half True ++ replicate half False) : cases (n-1)
  where half = (2 ^ n) `div` 2
        cases 1 = [ concat (replicate half [True, False]) ]
        cases i = concat (replicate (2^(n-i)) (replicate (2^(i-1)) True ++ replicate (2^(i-1)) False)) : cases (i - 1)

evalRow :: [(String, Bool)] -> [Term] -> ([(String,Bool)],[Bool])
evalRow vars rules = (vars, map eval' rules)
  where eval' (Var   v) = unsafeLookup v vars
        eval' (And a b) = eval' a && eval' b
        eval' (Or  a b) = eval' a || eval' b
        eval' (Xor a b) = eval' a `xor` eval' b
        eval' (Iff a b) = eval' a == eval' b
        eval' (Imp a b) = if eval' a then eval' b else True
        eval' (Not   a) = not $ eval' a

xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _ = False

unsafeLookup :: Eq a => a -> [(a, b)] -> b
unsafeLookup = (fromJust .) . lookup

