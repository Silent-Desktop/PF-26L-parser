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
    left <- try boolLogicExpr <|> try boolMathExpr <|> valuable
    rest left
  where
    rest left =
        do
            op <- boolOp
            right <-  try boolLogicExpr <|>try boolMathExpr <|> valuable
            rest (BinOp op left right)
        <|> return left

assign :: Parser Expr
assign = do
  name <- identifier
  symbol "="
  Assign name <$> valuable

ifExprInner :: Parser Expr
ifExprInner = do
  keyword "if"
  content <- boolExpr
  return $ IfExpr content

ifExpr :: Parser Expr
ifExpr = do
  expr <- ifExprInner
  symbol ":"
  return $ expr
elifExpr :: Parser Expr
elifExpr = do
  keyword "elif"
  content <- boolExpr
  symbol ":"
  return $ ElifExpr content

elseExpr :: Parser Expr
elseExpr = do
  keyword "else"
  content <- boolExpr
  symbol ":"
  return $ ElseExpr content

whileExpr :: Parser Expr
whileExpr = do
  keyword "while"
  content <- boolExpr
  symbol ":"
  return $ WhileLoop content

forExprInner :: Parser Expr
forExprInner = do
  keyword "for"
  iden <- identifier
  keyword "in"
  content <- valuable
  return $ ForLoop  iden content
forExpr :: Parser Expr
forExpr = do
  forLoop <- forExprInner
  symbol ":"
  return $ forLoop


compIfs :: Parser [Expr]
compIfs = do
  condition <- optional ifExprInner
  case condition of
    Nothing -> return []
    Just a -> rest [a]
  where
    rest acc =
      do
        next <- ifExprInner
        rest (acc ++ [next])
        <|> return acc
listCompOptIfExpr :: Parser Expr
listCompOptIfExpr = do
    symbol "["
    val <- valuable
    forLoop <- forExpr
    conditions <- compIfs
    symbol "]"
    return $ ListCompExpr val forLoop conditions

    

