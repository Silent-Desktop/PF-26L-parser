{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Valuable where

import qualified Data.Text.IO ()
import DataTypes
import Debug.Trace
import Literals
import Text.Megaparsec
import Utils

atom :: Parser Expr
atom = try call <|> try parens <|> try boolLit <|> try stringLit <|> try walrus <|> try variable <|> number

mathAtom :: Parser Expr
mathAtom = try listCompExpr <|> atom

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

ternaryValue :: Parser Expr
ternaryValue = try boolLogicExpr <|> try boolMathExpr <|> baseExpr

ternary :: Parser Expr
ternary = do
  value <- ternaryValue
  condition <- ifExprInner
  keyword "else"
  Ternary condition value <$> valuable

valuable :: Parser Expr
valuable = try ternary <|> try boolExpr <|> try boolLogicExpr <|> try boolMathExpr <|> baseExpr

posArg :: Parser Arg
posArg = PosArg <$> valuable

kwArg :: Parser Arg
kwArg = do
  name <- identifier
  symbol "="
  KwArg name <$> valuable

arg :: Parser Arg
arg = try kwArg <|> posArg

listCompExpr :: Parser Expr
listCompExpr = do
  symbol "["
  prMultiline
  val <- try ternary <|> baseExpr
  prMultiline
  forLoop <- forExprInner
  prMultiline
  conditions <- compIfs
  prMultiline
  symbol "]"
  return $ ListCompExpr val forLoop conditions

parens :: Parser Expr
parens = do
  symbol "("
  prMultiline
  e <- valuable
  symbol ")" -- use regular symbol, not symbolM
  return e

args :: Parser [Arg]
args = do
  prMultiline
  first <- optional (try arg)
  case first of
    Nothing -> return []
    Just a -> rest [a]
  where
    rest acc =
      do
        try (symbol ",")
        prMultiline
        next <- optional (try arg)
        case next of
          Nothing -> return acc -- trailing comma, stop
          Just a -> rest (acc ++ [a])
        <|> return acc

call :: Parser Expr
call = do
  name <- identifier
  symbol "("
  prMultiline
  arguments <- args
  prMultiline
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
  left <- baseExpr
  operand <- compOp
  BoolMathExpr operand left <$> baseExpr

boolLogicExpr :: Parser Expr
boolLogicExpr = do
  left <- baseExpr
  operand <- eqOp
  BoolLogicExpr operand left <$> baseExpr

boolExpr :: Parser Expr
boolExpr = do
  left <- try boolLogicExpr <|> try boolMathExpr <|> baseExpr
  rest left
  where
    rest left =
      do
        op <- boolOp
        right <- try boolLogicExpr <|> try boolMathExpr <|> baseExpr
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
  Walrus name <$> (try walrus <|> try boolExpr <|> baseExpr)

ifExprInner :: Parser Expr
ifExprInner = do
  keyword "if"
  IfExpr <$> boolExpr

ifExpr :: Parser Expr
ifExpr = do
  expr <- ifExprInner
  symbol ":"
  return expr

elifExpr :: Parser Expr
elifExpr = do
  keyword "elif"
  content <- boolExpr
  symbol ":"
  return $ ElifExpr content

elseExpr :: Parser Expr
elseExpr = do
  keyword "else"
  symbol ":"
  return ElseExpr

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
  ForLoop iden <$> valuable

forExpr :: Parser Expr
forExpr = do
  forLoop <- forExprInner
  symbol ":"
  return forLoop

compIfs :: Parser [Expr]
compIfs = do
  condition <- optional (try ifExprInner)
  case condition of
    Nothing -> return []
    Just a -> rest [a]
  where
    rest acc =
      do
        next <- try (prMultiline >> ifExprInner)
        rest (acc ++ [next])
        <|> return acc

returnExpr :: Parser Expr
returnExpr = do
  keyword "return"
  Return <$> valuable

passExpr :: Parser Expr
passExpr = do
  keyword "pass"
  return Pass

commentExpr :: Parser Expr
commentExpr = do
  symbol "#"
  content <- many (satisfy (/= '\n'))
  return $ Comment content

classExpr :: Parser Expr
classExpr = do
  keyword "class"
  className <- identifier
  paren <- optional (symbol "(")
  case paren of
    Nothing -> do
      symbol ":"
      return $ Class className Nothing
    Just _ -> do
      inheritance <- identifier
      symbol ")"
      symbol ":"
      return $ Class className (Just inheritance)
