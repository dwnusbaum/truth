import Control.Arrow (first)
import Control.Monad (liftM, liftM2, liftM3)
import Data.Functor.Identity(Identity)
import Data.List
import Data.Maybe
import System.Environment (getArgs)
import Text.Parsec hiding (parse)
import Text.Parsec.Expr
import Text.Parsec.Language(emptyDef)
import qualified Text.Parsec.Token as T

pprint :: Bool -> String
pprint True = "T"
pprint False = "F"

data Sym
   = Var String
   | And Sym Sym
   | Or  Sym Sym
   | Xor Sym Sym
   | Iff Sym Sym
   | Imp Sym Sym
   | Neg Sym
   deriving (Show, Eq)

isVar :: Sym -> Bool
isVar (Var v) = True
isVar _ = False

getVar :: Sym -> String
getVar (Var v) = v
getVar _ = undefined

--Evaluator

eval :: [String] -> [Sym] -> [([Bool],[Bool])]
eval vars rules = map (\x -> (map snd x, x `evalRow` rules)) env
  where truthValues = makeCases $ length vars
        env = map (zip vars) truthValues

makeCases :: Int -> [[Bool]]
makeCases n
  | n < 1 = error "There was less than 1 variable found"
  | n == 1 = [ [ True ], [ False ] ]
  | otherwise = transpose $ (replicate (half n 1) True ++ replicate (half n 1) False) : cases (n-1)
  where half x i = (2 ^ x) `div` (2 ^ (i - 1))
        cases 1 = [ concat (replicate (half n 1) [True, False]) ]
        cases i = concat (replicate (half n (i+1)) (replicate (half i 1) True ++ replicate (half i 1) False)) : cases (i - 1)

evalRow :: [(String, Bool)] -> [Sym] -> [Bool]
evalRow vars = map eval'
  where eval' (Var   v) = unsafeLookup v vars
        eval' (And a b) = eval' a && eval' b
        eval' (Or  a b) = eval' a || eval' b
        eval' (Xor a b) = eval' a `xor` eval' b
        eval' (Iff a b) = eval' a == eval' b
        eval' (Imp a b) = if eval' a then eval' b else True
        eval' (Neg   a) = not $ eval' a

xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _ = False

unsafeLookup :: Eq a => a -> [(a, b)] -> b
unsafeLookup = (fromJust .) . lookup


-- Parser

type Parser = Parsec String ()

parse :: String -> Either ParseError [Sym]
parse = runParser (expr `sepBy1` char ';') () "Truth"

expr :: Parser Sym
expr = buildExpressionParser opTable term

term =  liftM Var (many1 letter)
    <|> parens expr

lexer :: T.TokenParser ()
lexer = T.makeTokenParser style

style :: T.LanguageDef st
style = emptyDef
    { T.commentStart    = "{-"
    , T.commentEnd      = "-}"
    , T.commentLine     = "--"
    , T.nestedComments  = True
    , T.identStart      = letter
    , T.identLetter     = alphaNum <|> oneOf "_'"
    , T.opStart         = T.opLetter style
    , T.opLetter        = oneOf ""
    , T.reservedOpNames = [ "&", "|", "^", "<->", "->", "~", ";" ]
    , T.reservedNames   = []
    , T.caseSensitive   = True
    }

parens :: Parser a -> Parser a
parens = T.parens lexer

reservedOp :: String -> Parser ()
reservedOp = T.reservedOp lexer

opTable :: [[Operator String () Identity Sym]]
opTable = [ [ Infix  (reservedOp "<->" >> return Iff) AssocLeft
            , Infix  (reservedOp "->" >> return Imp) AssocLeft
            ]
          , [ Infix  (reservedOp "^" >> return Xor) AssocLeft
            , Infix  (reservedOp "&" >> return And) AssocLeft
            , Infix  (reservedOp "|" >> return Or) AssocLeft
            , Prefix (reservedOp "~" >> return Neg)
            ]
          ]

main = getArgs >>= \args -> case args of
    [] -> putStrLn "No rules specified"
    [x] -> case parse x of
        Left err -> print err
        Right val -> mapM_ (\(x,y) -> putStrLn (init (concatMap ((++ ",") . pprint) x) ++ "|" ++ init (concatMap ((++ ",") . pprint) y ))) $ uncurry eval vars
          where vars = first (map getVar) $ span isVar val
    _ -> putStrLn "Invalid command line arguments"
