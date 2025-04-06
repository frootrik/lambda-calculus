{-# LANGUAGE OverloadedStrings #-}
module Parser (topParser) where

import LambdaAST
import Text.Megaparsec
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void

{-parsec vs parsect? trenger ikke side effekter->parsec

--type Parsec e s a = ParsecT e s Identity a

e: the type of custom component of error message (?)
s: the type of input stream 
m: inner monad of the parsect monad transformer 
a: monadic value, result of parsing (output?) m = identity = resultat av parse?
type Parser = Parsec Void String

Out-of-box for String, Text, ByteString og Stream


-}

type Parser = Parsec Void String

consumeSpace :: Parser ()
consumeSpace = L.space C.space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme consumeSpace 

symbol :: String -> Parser String
symbol = L.symbol consumeSpace 

varParser :: Parser Expr
varParser = Var <$> lexeme ((:) <$> C.letterChar <*> many C.alphaNumChar)

parens :: Parser Expr
parens = between (symbol "(") (symbol ")") expressionParser

abstractionParser :: Parser Expr
abstractionParser = do
    _ <- lexeme (C.char '\\' <|> C.char 'λ')
    toSub <- lexeme ((:) <$> C.letterChar <*> many C.alphaNumChar)
    _ <- lexeme (C.char '.')
    funcexpr <- expressionParser
    return (Abs toSub funcexpr)

termParser :: Parser Expr
termParser = parens <|> abstractionParser <|> varParser

expressionParser :: Parser Expr
expressionParser = do
    terms <- some termParser
    -- return $ foldl App terms
    return $ foldl1 App terms 

topParser :: String -> Either (ParseErrorBundle String Void) Expr
topParser = parse (consumeSpace *> expressionParser <* eof) ""
-- eof :: MonadParsec e s m => m () : "only"

-- parse :: Parsec e s a -> SourceName -> s -> Either (ParseErrorBundle s e) a
