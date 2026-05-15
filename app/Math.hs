{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Math where

import qualified Data.Text.IO ()
import DataTypes
import Text.Megaparsec
import Utils
import Valuable

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

boolMathExpr :: Parser Expr
boolMathExpr = do
  left <- valuable
  operand <- compOp
  BoolMathExpr operand left <$> valuable

boolLogicExpr :: Parser Expr
boolLogicExpr = do
  left <- valuable
  operand <- eqOp
  BoolLogicExpr operand left <$> valuable

boolExpr :: Parser Expr
boolExpr = do
  left <- valuable
  rest left
  where
    rest left =
      do
        op <- boolOp
        right <- valuable
        rest (BinOp op left right)
        <|> return left

assign :: Parser Expr
assign = do
  name <- identifier
  symbol "="
  Assign name <$> valuable

ifExpr :: Parser Expr
ifExpr = do
  keyword "if"
  content <- boolExpr
  symbol ":"
  return $ Conditional content

whileExpr :: Parser Expr
whileExpr = do
  keyword "while"
  content <- boolExpr
  symbol ":"
  return $ WhileLoop content
