{-# OPTIONS_GHC -Wall #-}

module Main
    ( main
    ) where

import Data.List (intercalate)
import System.Environment (getArgs)

import Truth
import Parse

parseAndEval :: String -> String -> IO ()
parseAndEval varsInput rulesInput = case parseVars varsInput of
    Left err -> print err
    Right vars -> case parseRules rulesInput of
        Left err -> print err
        Right rules -> do
          putStrLn $ showResults $ filter (all id . snd) evaled
          putStrLn $ fullOutput vars rules evaled
          where evaled = eval vars rules

showResults :: [([(String,Bool)],[Bool])] -> String
showResults [] = "No values satisfied all rules"
showResults vals = "The following values satisfied all rules:\n" ++ intercalate "\nand\n" (map (unwords . map showVar . fst) vals)
  where showVar (s, True) = s
        showVar (s,False) = '~' : s

fullOutput :: [String] -> [Rule] -> [([(String,Bool)],[Bool])] -> String
fullOutput vars rules vals = unlines $ intercalate "," (vars ++ map show rules) : map (\(x,y) -> intercalate "," (map (showBool . snd) x ++ map showBool y)) vals
  where showBool True  = "T"
        showBool False = "F"

main :: IO ()
main = getArgs >>= \args -> case args of
    []      -> parseAndEval "A,B" "A|~B,A->B" --putStrLn "No rules specified. Example usage: truth \"A,B,A|~B,A->B\""
    [vars, rules] -> parseAndEval vars rules
    _ -> putStrLn "Invalid command line arguments"
