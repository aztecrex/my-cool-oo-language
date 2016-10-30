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



