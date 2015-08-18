{-# OPTIONS_GHC -Wall #-}

module Parse
    ( parseRules
    , parseVars
    ) where

import Control.Monad (liftM)
import Data.Functor.Identity(Identity)
import Text.Parsec hiding (parse)
import Text.Parsec.Expr
import Text.Parsec.Language(emptyDef)
import qualified Text.Parsec.Token as T

import Truth

type Parser = Parsec String ()

parseRules :: String -> Either ParseError [Rule]
parseRules = runParser (expr `sepBy` char ',') () "Rules"

parseVars :: String -> Either ParseError [String]
parseVars = runParser (identifier `sepBy1` char ',') () "Vars"

expr :: Parser Rule
expr = buildExpressionParser opTable term

term :: Parser Rule
term =  liftM Var identifier
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

identifier :: Parser String
identifier = T.identifier lexer

parens :: Parser a -> Parser a
parens = T.parens lexer

reservedOp :: String -> Parser ()
reservedOp = T.reservedOp lexer

opTable :: [[Operator String () Identity Rule]]
opTable = [ [ Prefix (reservedOp "~" >> return Not) ]
          , [ Infix  (reservedOp "<->" >> return Iff) AssocLeft
            , Infix  (reservedOp "->" >> return Imp) AssocLeft
            ]
          , [ Infix  (reservedOp "^" >> return Xor) AssocLeft
            , Infix  (reservedOp "&" >> return And) AssocLeft
            , Infix  (reservedOp "|" >> return Or) AssocLeft
            ]
          ]
