module Clutch.Parser (parseString) where

import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.String -- input stream is of type ‘String’
import qualified Text.Megaparsec.Lexer as L

-- Syntax Tree
data CompilationUnit = CompilationUnit [Statement] deriving (Show)

data Statement =
  TypeDeclaration TypeLiteral [TypeStatement]
  deriving (Show)

data TypeStatement =
  Class String
  deriving (Show)

data TypeLiteral = 
   TypeLiteral String [TypeLiteral]
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

comma :: Parser String
comma = symbol ","

block :: Parser a -> Parser a
block = between (symbol "{") (symbol "}")

angle :: Parser a -> Parser a
angle = between (symbol "<") (symbol ">")

reservedWord :: String -> Parser()
reservedWord w = string w *> notFollowedBy alphaNumChar *> sc

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
    where
      p       = (:) <$> letterChar <*> many alphaNumChar
      check x = if elem x reservedWords
                  then fail $ "reserved word " ++ show x ++ " cannot be an identifier"
                  else return x
      reservedWords = ["type","class"]


-- Parser

clutchParser :: Parser CompilationUnit
clutchParser = do
  sc
  statements <- some statement
  eof
  return $ CompilationUnit statements

statement :: Parser Statement
statement = statement' <* semi

statement' :: Parser Statement
statement' = typeDeclaration

typeDeclaration :: Parser Statement
typeDeclaration = do
  reservedWord "type"
  typeId <- typeLiteral
  maybeStatements <- optional (block (many typeStatement)) 
  return $ TypeDeclaration typeId (maybe [] id maybeStatements)

typeStatement :: Parser TypeStatement
typeStatement = typeStatement' <* semi

typeStatement' :: Parser TypeStatement
typeStatement' = clazz

clazz :: Parser TypeStatement
clazz = reservedWord "class" >> identifier >>= \id -> return $ Class id

typeLiteral :: Parser TypeLiteral
typeLiteral = do
  name <- identifier
  maybeParams <- optional (angle (sepBy typeLiteral comma))
  return $ TypeLiteral name (maybe [] id maybeParams) 

-- Runner

parseString :: String -> CompilationUnit
parseString source =
  case parse clutchParser "" source of
    Left e -> error $ show e
    Right r -> r


