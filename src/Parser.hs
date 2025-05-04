{-# LANGUAGE OverloadedStrings #-}
module Parser (parseLambda, topParser) where

import LambdaAST
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void

type Parser = Parsec Void String

-- Space consumer that ignores whitespace
consumeSpace :: Parser ()
consumeSpace = L.space space1 empty empty

-- Lexeme that consumes trailing whitespace
lexeme :: Parser a -> Parser a
lexeme = L.lexeme consumeSpace

-- Symbol parser that consumes trailing whitespace
symbol :: String -> Parser String
symbol = L.symbol consumeSpace

-- Parse a variable name (identifier)
identifier :: Parser String
identifier = lexeme ((:) <$> letterChar <*> many alphaNumChar)

-- Parse a variable reference
pVar :: Parser (Lambda String)
pVar = Var <$> identifier


pParens :: Parser (Lambda String)
pParens = between (symbol "(") (symbol ")") parseLambda

-- Parse a lambda abstraction
pLam :: Parser (Lambda String)
pLam = do 
  _ <- symbol "\\" <|> symbol "λ" -- parse \ eller λ
  param <- identifier -- parse så identifieren, som er en Streng
  _ <- symbol "." -- parse så ., for λ.x, for eksempel
  lam param <$> parseLambda -- 

-- Parse an application
pApp :: Parser (Lambda String)
pApp = do
  terms <- some pTerm
  return $ foldl1 App terms
  where
    pTerm = choice [pParens, pVar]

-- Parse any lambda calculus expression
parseLambda :: Parser (Lambda String)
parseLambda = try pLam <|> try pApp <|> try pParens <|> try pVar
 
-- Top-level parser that handles the whole input
topParser :: String -> Either (ParseErrorBundle String Void) (Lambda String)
topParser = parse (consumeSpace *> parseLambda <* eof) ""
