{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Valuable where

import qualified Data.Text.IO ()
import DataTypes
import Literals
import Text.Megaparsec
import Utils


parens :: Parser Expr
parens = do
    symbol "("
    e <- valuable
    symbol ")"
    return e

atom :: Parser Expr
atom = try call <|> try parens <|> try boolLit <|> try stringLit <|> try walrus <|> try variable <|> number


mathAtom :: Parser Expr
mathAtom =  try listCompExpr <|> atom

mathExpr :: Parser Expr
mathExpr = do
    left <- mathAtom
    rest left
  where
    rest left =
            do symbol "+"; right <- mathAtom; rest (Add left right)
        <|> do symbol "-"; right <- mathAtom; rest (Sub left right)
        <|> do symbol "*"; right <- mathAtom; rest (Mult left right)
        <|> do symbol "/"; right <- mathAtom; rest (Div left right)
        <|> return left

baseExpr :: Parser Expr
baseExpr = try mathExpr <|> mathAtom

ternary :: Parser Expr
ternary = do
    value <- baseExpr
    condition <- ifExprInner
    keyword "else"
    fallback <- valuable
    return $ Ternary condition value fallback

valuable :: Parser Expr
valuable = try ternary <|> baseExpr


posArg :: Parser Arg
posArg = PosArg <$> valuable

kwArg :: Parser Arg
kwArg = do
  name <- identifier
  symbol "="
  KwArg name <$> valuable

arg :: Parser Arg
arg = try kwArg <|> posArg

args :: Parser [Arg]
args = do
  first <- optional arg
  case first of
    Nothing -> return []
    Just a -> rest [a]
  where
    rest acc =
      do
        symbol ","
        next <- case last acc of
          KwArg _ _ -> kwArg -- once we've seen a kwarg, only kwarg allowed
          PosArg _ -> arg -- still allow either
        rest (acc ++ [next])
        <|> return acc

call :: Parser Expr
call = do
  name <- identifier
  symbol "("
  arguments <- args
  symbol ")"
  return $ Call name arguments

functionDeclarationExpr :: Parser Expr
functionDeclarationExpr = do
  keyword "def"
  name <- identifier
  symbol "("
  arguments <- args
  symbol ")"
  symbol ":"
  return $ FuncDeclExpr name arguments


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

walrus :: Parser Expr
walrus = do
  name <- identifier
  symbol ":="
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



listCompExpr :: Parser Expr
listCompExpr = do
    symbol "["
    val <- try ternary <|> baseExpr
    forLoop <- forExpr
    conditions <- compIfs
    symbol "]"
    return $ ListCompExpr val forLoop conditions
