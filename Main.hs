{-# OPTIONS_GHC -Wall #-}

module Main
    ( main
    ) where

import Data.List (intercalate)
import System.Environment (getArgs)

import Truth
import Parse

parseAndEval :: String -> String -> IO ()
parseAndEval input filename = case parse input of
    Left err -> print err
    Right rules -> putStrLn (showResults (filter (\(_,y) -> all (==True) y) evaled)) >> writeFile filename (csv rules evaled) >> putStrLn ("Truth table written to " ++ filename)
      where evaled = uncurry eval $ varSplit rules

showResults :: [([(String,Bool)],[Bool])] -> String
showResults [] = "No values satisfied all rules"
showResults vals = "The following values satisfied all rules:\n" ++ intercalate "\nand\n" (map (unwords . map showVar . fst) vals)
  where showVar (s,True ) = s
        showVar (s,False) = '~' : s

csv :: [Term] -> [([(String,Bool)],[Bool])] -> String
csv rules vals = unlines $ intercalate "," (map show rules) : map (\(x,y) -> intercalate "," (map (showBool . snd) x ++ map showBool y)) vals
  where showBool True  = "T"
        showBool False = "F"

main :: IO ()
main = getArgs >>= \args -> case args of
    [] -> putStrLn "No rules specified. Example usage: truth \"A;B;A|~B\""
    [input]          -> parseAndEval input "truth.csv"
    [input,filename] -> parseAndEval input filename
    _ -> putStrLn "Invalid command line arguments"
