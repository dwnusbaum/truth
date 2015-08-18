{-# OPTIONS_GHC -Wall #-}

module Truth
    ( Sym
    , Rule(..)
    , Env
    , truthTable
    ) where

import Data.Maybe

type Sym = String

data Rule
   = Var Sym
   | And Rule Rule
   | Or  Rule Rule
   | Xor Rule Rule
   | Iff Rule Rule
   | Imp Rule Rule
   | Not Rule
   deriving (Eq)

type Env = [(Sym, Bool)]

instance Show (Rule) where
    showsPrec _ (Var   v) = showString v
    showsPrec p (And a b) = showParen (p>0) $ showsPrec p a . showChar '&' . showsPrec (p+1) b
    showsPrec p (Or  a b) = showParen (p>0) $ showsPrec p a . showChar '|' . showsPrec (p+1) b
    showsPrec p (Xor a b) = showParen (p>0) $ showsPrec p a . showChar '^' . showsPrec (p+1) b
    showsPrec p (Iff a b) = showParen (p>0) $ showsPrec p a . showString "<->" . showsPrec (p+1) b
    showsPrec p (Imp a b) = showParen (p>0) $ showsPrec p a . showString "->" . showsPrec (p+1) b
    showsPrec p (Not   t) = showChar '~' . showsPrec (p+1) t

--Evaluator

truthTable :: [Sym] -> [Rule] -> [(Env, [Bool])]
truthTable vars rules = map (`evalRow` rules) $ getEnvs [] vars

getEnvs :: [(Sym, Bool)] -> [Sym] -> [Env]
getEnvs xs []     = [xs]
getEnvs xs (y:ys) = getEnvs ((y, True) : xs) ys ++ getEnvs ((y, False) : xs) ys

evalRow :: Env -> [Rule] -> (Env, [Bool])
evalRow env rules = (env, map eval' rules)
  where eval' (Var   v) = fromJust . lookup v $ env
        eval' (And a b) = eval' a && eval' b
        eval' (Or  a b) = eval' a || eval' b
        eval' (Xor a b) = eval' a `xor` eval' b
        eval' (Iff a b) = eval' a == eval' b
        eval' (Imp a b) = if eval' a then eval' b else True
        eval' (Not   a) = not $ eval' a

xor :: Bool -> Bool -> Bool
xor True  False = True
xor False True  = True
xor _     _     = False
