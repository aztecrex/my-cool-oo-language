module Clutch.Parser (parseString) where

import Data.Maybe (fromMaybe, isJust, fromJust)
import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.String -- input stream is of type ‘String’
import qualified Text.Megaparsec.Lexer as L

-- Syntax Tree
data CompilationUnit = CompilationUnit [Statement] deriving (Show)

data Statement =
    TypeDeclaration TypeLiteral [Statement] [Modifier]
  | Class String
  | InterfaceDeclaration TypeLiteral (Maybe TypeLiteral) [Statement]
  | Placeholder
  deriving (Show)

data TypeLiteral = 
   TypeLiteral String [TypeLiteral]
   deriving (Show)
  
data Modifier = Native deriving (Show)
 
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
      reservedWords = ["type", "class", "native", "placeholder"]


-- Parser

clutchParser :: Parser CompilationUnit
clutchParser = do
  sc
  statements <- statements topLevelStatement
  eof
  return $ CompilationUnit statements

statements :: Parser Statement -> Parser [Statement]
statements p = many (p <* semi) >>= \ss -> return  ss

topLevelStatement :: Parser Statement
topLevelStatement = typeDeclaration <|> interfaceDeclaration;

typeDeclaration :: Parser Statement
typeDeclaration = do
  maybeNative <- fmap (const Native) <$> optional (reservedWord "native")
  reservedWord "type"
  typeId <- typeLiteral
  maybeStatements <- optional (block (statements typeStatement)) 
  return $ TypeDeclaration typeId (maybe [] id maybeStatements)
          (fmap fromJust (filter isJust [maybeNative]))
            
typeStatement :: Parser Statement
typeStatement = clazz;

clazz :: Parser Statement
clazz = reservedWord "class" >> identifier >>= \id -> return $ Class id

typeLiteral :: Parser TypeLiteral
typeLiteral = do
  name <- identifier
  maybeParams <- optional (angle (sepBy typeLiteral comma))
  return $ TypeLiteral name (maybe [] id maybeParams) 

interfaceDeclaration :: Parser Statement
interfaceDeclaration = do
  reservedWord "interface"
  ifcId <- typeLiteral
  maybeBinding <- optional typeLiteral
  maybeStatements <- optional (block (statements interfaceStatement))
  return $ InterfaceDeclaration ifcId maybeBinding (fromMaybe [] maybeStatements)

interfaceStatement :: Parser Statement
interfaceStatement = reservedWord "placeholder" >>= \_ -> return Placeholder


-- Runner

parseString :: String -> CompilationUnit
parseString source =
  case parse clutchParser "" source of
    Left e -> error $ show e
    Right r -> r


