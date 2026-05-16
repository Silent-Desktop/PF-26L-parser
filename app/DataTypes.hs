module DataTypes where

import Data.Text (Text)
import qualified Data.Text.IO ()
import Data.Void
import Text.Megaparsec

type Parser = Parsec Void Text

data Expr
  = Number Int
  | FloatNum Float
  | Var String
  | Literal
  | Assign String Expr
  | Add Expr Expr
  | Sub Expr Expr
  | Mult Expr Expr
  | Div Expr Expr
  | MathExpr
  | BoolMathExpr CompOp Expr Expr
  | BoolLogicExpr EqOp Expr Expr
  | BinOp BoolOp Expr Expr
  | IfExpr Expr
  | ElifExpr Expr
  | ElseExpr
  | WhileLoop Expr
  | ForLoop String Expr
  | MultilineStringLit String
  | StringLit String
  | Lit Keyword
  | Call String [Arg]
  | FuncDeclExpr String [Arg]
  | ListCompExpr Expr Expr [Expr]
  | Return Expr
  | Comment String
  | Pass
  | Walrus String Expr
  | Class String (Maybe String)
  | Ternary Expr Expr Expr -- Condition | Value | Value if else
  deriving (Show, Eq)

data BoolOp = And | Or deriving (Show, Eq)

data EqOp = Equal | Neq deriving (Show, Eq)

data CompOp = Gt | Lt | Gte | Lte | EqComp EqOp deriving (Show, Eq)

data Arg
  = PosArg Expr
  | KwArg String Expr
  deriving (Show, Eq)

data Keyword = KTrue | KFalse | KIf | KElse | KWhile | KFor | KDef | KReturn
  deriving (Show, Eq)