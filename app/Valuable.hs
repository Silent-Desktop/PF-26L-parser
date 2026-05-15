{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Valuable where

import qualified Data.Text.IO ()
import DataTypes
import Literals
import Text.Megaparsec
import Utils

mathExpr :: Parser Expr
mathExpr = do
  left <- valuable
  rest left
  where
    rest left =
      do symbol "+"; right <- valuable; rest (Add left right)
        <|> do symbol "-"; right <- valuable; rest (Sub left right)
        <|> do symbol "*"; right <- variable <|> number; rest (Mult left right)
        <|> do symbol "/"; right <- variable <|> number; rest (Div left right)
        <|> return left

valuable :: Parser Expr
valuable = try call <|> try mathExpr <|> boolLit <|> stringLit <|> variable <|> number

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
