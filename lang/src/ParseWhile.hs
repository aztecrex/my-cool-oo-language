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


