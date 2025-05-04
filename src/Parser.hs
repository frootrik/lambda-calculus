{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Parser
-- Description : Lambda calculus parser built using Megaparsec.
-- This module defines parsers for lambda expressions, including variables,
-- abstractions, and applications. The resulting expressions are represented
-- using the Lambda AST defined in 'LambdaAST'.

module Parser (parseLambda, topParser) where

import LambdaAST
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void

-- | The parser type specialized to 'String' input and 'Void' error type.
type Parser = Parsec Void String

-- | Consumes one or more whitespace characters (space, tab, etc.).
-- Ignores comments or other forms of input skipping.
consumeSpace :: Parser ()
consumeSpace = L.space space1 empty empty

-- | Wraps a parser to consume any trailing whitespace after it.
--
-- Useful to ensure whitespace is ignored between tokens.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme consumeSpace

-- | Parses a given string symbol and consumes any trailing whitespace.
--
-- Example: @symbol "."@ parses a literal dot and skips following spaces.
symbol :: String -> Parser String
symbol = L.symbol consumeSpace

-- | Parses an identifier — a variable name starting with a letter,
-- followed by any number of alphanumeric characters.
--
-- Example: "x", "var1", "z42"
identifier :: Parser String
identifier = lexeme ((:) <$> letterChar <*> many alphaNumChar)

-- | Parses a variable reference and wraps it as a 'Var' node in the AST.
--
-- Example: parsing "x" produces @Var "x"@.
pVar :: Parser (Lambda String)
pVar = Var <$> identifier

-- | Parses a parenthesized expression.
--
-- Example: @(λx.x)@
pParens :: Parser (Lambda String)
pParens = between (symbol "(") (symbol ")") parseLambda

-- | Parses a lambda abstraction using either '\' or 'λ' syntax.
--
-- Example: parses @\x.x@ or @λx.x@ into @LamAbs "x" (Var "x")@ (after scoping).
pLam :: Parser (Lambda String)
pLam = do 
  _ <- symbol "\\" <|> symbol "λ"    -- Accepts '\' or 'λ' for abstraction
  param <- identifier                -- Binds the parameter name
  _ <- symbol "."                    -- Parses the dot separating param and body
  lam param <$> parseLambda          -- Wraps with 'lam' for correct scoping

-- | Parses a left-associative function application from one or more terms.
--
-- Example: parses @f x y@ into @App (App f x) y@
pApp :: Parser (Lambda String)
pApp = do
  terms <- some pTerm
  return $ foldl1 App terms
  where
    pTerm = choice [pParens, pVar]

-- | Parses any lambda calculus expression, trying abstraction, application,
-- parenthesized expressions, or variables in order.
--
-- Returns a raw 'Lambda String' term (unscoped).
parseLambda :: Parser (Lambda String)
parseLambda = try pLam <|> try pApp <|> try pParens <|> try pVar

-- | Top-level parser that consumes the entire input string.
--
-- It parses a lambda expression and ensures no remaining input is left.
-- Returns either a parse error or the parsed 'Lambda String' expression.
topParser :: String -> Either (ParseErrorBundle String Void) (Lambda String)
topParser = parse (consumeSpace *> parseLambda <* eof) ""
