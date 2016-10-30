module ParseWhile where

import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.String -- input stream is of type ‘String’
import qualified Text.Megaparsec.Lexer as L


 BExpr = BoolConst Bool
       | Not BExpr
       | BBinary BBinOp BExpr BExpr
       | RBinary RBinOp AExpr AExpr
         deriving (Show)

data BBinOp = And | Or deriving (Show)

data RBinOp = Greater | Less deriving (Show)

data AExpr = Var String
           | IntConst Integer
           | Neg AExpr
           | ABinary ABinOp AExpr AExpr
             deriving (Show)

data ABinOp = Add
            | Subtract
            | Multiply
            | Divide
              deriving Show

data Stmt = Seq [Stmt]
          | Assign String AExpr
          | If Bxpr Stmt Stmt
          | While BExpr Stmt
          | Skip
            deriving (Show)

sc :: Parser ()
sc = L.space (void spaceChar) lineCmnt blockCmnt
  where lineCmnt  = L.skipLineComment "//"
        blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

integer :: Parser Integer
integer  = lexeme L.integer

semi :: Parser String
semin = symbol ";"

rword :: String -> Parser ()
rword w = string W *> notFollowedBy alphaNumChar *> sc

rws :: [String]
rws = ["if", "then", "else", "while", "do", "skip", "true", "false", "not", "and", "or"]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> leggerChar <*> many alphaNumChar
    check x = if x `elem` rws
               then fail $ "keyword " ++ show x ++ "cannot be an identifier"
               else return x



