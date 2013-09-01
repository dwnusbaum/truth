{-# OPTIONS_GHC -Wall #-}

module Parse
    ( parse
    ) where

import Control.Monad (liftM)
import Data.Functor.Identity(Identity)
import Text.Parsec hiding (parse)
import Text.Parsec.Expr
import Text.Parsec.Language(emptyDef)
import qualified Text.Parsec.Token as T

import Truth

type Parser = Parsec String ()

parse :: String -> Either ParseError [Term]
parse = runParser (expr `sepBy1` char ';') () "Truth"

expr :: Parser Term
expr = buildExpressionParser opTable term

term :: Parser Term
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

opTable :: [[Operator String () Identity Term]]
opTable = [ [ Infix  (reservedOp "<->" >> return Iff) AssocLeft
            , Infix  (reservedOp "->" >> return Imp) AssocLeft
            ]
          , [ Infix  (reservedOp "^" >> return Xor) AssocLeft
            , Infix  (reservedOp "&" >> return And) AssocLeft
            , Infix  (reservedOp "|" >> return Or) AssocLeft
            , Prefix (reservedOp "~" >> return Not)
            ]
          ]
