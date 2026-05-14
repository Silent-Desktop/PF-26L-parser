{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Parser where

import Data.Bool (bool)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO ()
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

data Expr
  = Number Int
  | Var String
  | Assign String Expr
  | Add Expr Expr
  | Sub Expr Expr
  | Mult Expr Expr
  | Div Expr Expr
  | MathExpr
  | BoolMathExpr CompOp Expr Expr
  | BoolLogicExpr EqOp Expr Expr
  | BinOp BoolOp Expr Expr
  | Conditional Expr
  | Lit Keyword
  deriving (Show, Eq)

data Keyword = KTrue | KFalse | KIf | KElse | KWhile | KFor | KDef | KReturn
  deriving (Show, Eq)

boolLit :: Parser Expr
boolLit =
  (keyword "True" >> return (Lit KTrue))
    <|> (keyword "False" >> return (Lit KFalse))

data BoolOp = And | Or deriving (Show, Eq)

data EqOp = Equal | Neq deriving (Show, Eq)

data CompOp = Gt | Lt | Gte | Lte | EqComp EqOp deriving (Show, Eq)

eqOp :: Parser EqOp
eqOp =
  try (symbol "==" >> return Equal)
    <|> try (symbol "!=" >> return Neq)

compOp :: Parser CompOp
compOp =
  try (symbol ">=" >> return Gte)
    <|> try (symbol "<=" >> return Lte)
    <|> ( EqComp
            <$> eqOp
        )
    <|> (symbol ">" >> return Gt)
    <|> (symbol "<" >> return Lt)

boolOp :: Parser BoolOp
boolOp = (keyword "and" >> return And) <|> (keyword "or" >> return Or)

boolOps :: Parser [BoolOp]
boolOps = many boolOp

pr :: Parser ()
pr = L.space space1 empty empty

helper :: Parser a -> Parser a
helper = L.lexeme pr

symbol :: Text -> Parser Text
symbol = L.symbol pr

number :: Parser Expr
number = Number <$> helper L.decimal

reserved :: [String]
reserved = ["if", "else", "while", "for", "def", "return", "True", "False"]

identifier :: Parser String
identifier = helper $ do
  name <- (:) <$> letterChar <*> many alphaNumChar
  if name `elem` reserved
    then fail $ name ++ " is a reserved keyword"
    else return name

variable :: Parser Expr
variable = Var <$> identifier

keyword :: Text -> Parser Text
keyword kw = helper (string kw <* notFollowedBy alphaNumChar)

mathExpr :: Parser Expr
mathExpr = do
  left <- variable <|> number
  rest left
  where
    rest left =
      do symbol "+"; right <- variable <|> number; rest (Add left right)
        <|> do symbol "-"; right <- variable <|> number; rest (Sub left right)
        <|> do symbol "*"; right <- variable <|> number; rest (Mult left right)
        <|> do symbol "/"; right <- variable <|> number; rest (Div left right)
        <|> return left

boolMathExpr :: Parser Expr
boolMathExpr = do
  left <- try mathExpr <|> variable <|> number
  operand <- compOp
  right <- try mathExpr <|> variable <|> number
  return $ BoolMathExpr operand left right

boolLogicExpr :: Parser Expr
boolLogicExpr = do
  left <- try boolLit <|> variable
  operand <- eqOp
  right <- try boolLit <|> variable
  return $ BoolLogicExpr operand left right

boolExpr :: Parser Expr
boolExpr = do
  left <- try boolLogicExpr <|> try boolMathExpr <|> boolLit <|> variable
  rest left
  where
    rest left =
      do
        op <- boolOp
        right <- try boolLogicExpr <|> try boolMathExpr <|> boolLit <|> variable
        rest (BinOp op left right)
        <|> return left

assign :: Parser Expr
assign = do
  name <- identifier
  symbol "="
  expr <- number <|> variable
  return $ Assign name expr

ifExpr :: Parser Expr
ifExpr = do
  keyword "if"
  content <- boolExpr
  symbol ":"
  return $ Conditional content

statement :: Parser Expr
statement = try assign <|> mathExpr