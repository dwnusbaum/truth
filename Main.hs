{-# OPTIONS_GHC -Wall #-}

module Main
    ( main
    ) where

import Data.List (intercalate)
import System.Environment (getArgs)

import Truth
import Parse

showResults :: [([(String,Bool)],[Bool])] -> String
showResults [] = "No values satisfied all rules"
showResults vals = "The following values satisfied all rules:\n" ++ intercalate "\nand\n" (map (unwords . map ppBool . fst) vals)
  where ppBool (s, True) = s
        ppBool (s,False) = '~' : s

main :: IO ()
main = getArgs >>= \args -> case args of
    [] -> putStrLn "No rules specified. Example usage: truth \"A;B;A|~B\""
    [x] -> case parse x of
        Left err -> print err
        Right rules -> putStrLn (showResults (filter (\(_,y) -> all (==True) y) evaled))
          where evaled = uncurry eval $ varSplit rules
    _ -> putStrLn "Invalid command line arguments"
