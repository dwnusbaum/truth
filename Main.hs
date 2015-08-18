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
          putStrLn ""
          putStrLn $ fullOutput rules evaled
          where evaled = truthTable vars rules

showResults :: [(Env,[Bool])] -> String
showResults [] = "No values satisfied all rules"
showResults vals = "The following values satisfied all rules:\n" ++ intercalate "\nand\n" (map (unwords . map showVar . fst) vals)
  where showVar (s, True) = s
        showVar (s,False) = '~' : s

fullOutput :: [Rule] -> [(Env,[Bool])] -> String
fullOutput rules results = unlines . map (intercalate "  ") $ header : spacer : map showRow results
  where header = map fst (fst $ head results) ++ map show rules
        spacer = map (flip replicate '-' . length) header
        showRow (env, vals) = varsStrings ++ rulesStrings
          where varsStrings  = pad (map (head . show . snd)  env) $ map fst env
                rulesStrings = pad (map (head . show      ) vals) $ map show rules
                pad chars guides = zipWith (\x y -> y ++ [x]) chars $ map (\x -> replicate (length x - 1) ' ') guides

main :: IO ()
main = getArgs >>= \args -> case args of
    [] -> putStrLn "No rules specified. Example usage: truth \"A,B,A|~B,A->B\""
    [vars, rules] -> parseAndEval vars rules
    _ -> putStrLn "Invalid command line arguments"
