module Clutch.Parser (parseString) where

import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.String -- input stream is of type ‘String’
import qualified Text.Megaparsec.Lexer as L

-- Syntax Tree
data Statement =
   TypeDeclaration String
   deriving (Show)

-- Lexer
sc :: Parser ()
sc = L.space (void spaceChar) lineComment blockComment
  where lineComment = L.skipLineComment "//"
        blockComment = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

semi :: Parser String
semi = symbol ";"

-- block :: Parser a -> Parser a
-- block = between (symbol "{") (symbol "}")

reservedWord :: String -> Parser()
reservedWord w = string w *> notFollowedBy alphaNumChar *> sc

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
    where
      p       = (:) <$> letterChar <*> many alphaNumChar
      check x = if elem x reservedWords
                  then fail $ "reserved word " ++ show x ++ " cannot be an identifier"
                  else return x
      reservedWords = ["type"]


-- Parser

clutchParser :: Parser Statement
clutchParser = sc *> statement <* eof

statement :: Parser Statement
statement = statement' <* semi

statement' :: Parser Statement
statement' = typeStatement

typeStatement :: Parser Statement
typeStatement = do
  reservedWord "type"
  name <- identifier
  return $ TypeDeclaration name


-- Runner

parseString :: String -> Statement
parseString source =
  case parse clutchParser "" source of
    Left e -> error $ show e
    Right r -> r


