{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Parser
  ( module Valuable,
    module DataTypes,
    Line (..),
    statement,
    statementWithLine,
    statementEnd,
    blankLine,
    strippedStatement,
    program,
  )
where

import Control.Monad (void)
import qualified Data.Text.IO ()
import DataTypes
import Text.Megaparsec
import Text.Megaparsec.Char (char, eol, hspace)
import Valuable

data Line = Line Expr (Maybe Expr) deriving (Show, Eq)

blankLine :: Parser ()
blankLine = hspace *> void eol

strippedStatement :: Parser Line
strippedStatement = do
  many (try blankLine)
  many (char ' ' <|> char '\t')
  statementWithLine

program :: Parser [Line]
program = many strippedStatement <* eof

statementEnd :: Parser (Maybe Expr)
statementEnd = do
  hspace
  comment <- optional commentExpr
  void eol <|> eof
  return comment

statement :: Parser Expr
statement = do
  expr <- try commentExpr <|> try ifExpr <|> try forExpr <|> try whileExpr <|> try assign <|> try functionDeclarationExpr <|> try boolLogicExpr <|> try classExpr <|> try boolMathExpr <|> try returnExpr <|> valuable
  _comment <- statementEnd
  return expr

statementWithLine :: Parser Line
statementWithLine = do
  expr <-
    try commentExpr
      <|> try assign
      <|> try ifExpr
      <|> try elifExpr
      <|> try elseExpr
      <|> try forExpr
      <|> try whileExpr
      <|> try functionDeclarationExpr
      <|> try returnExpr
      <|> try classExpr
      <|> try passExpr
      <|> valuable
  Line expr <$> statementEnd