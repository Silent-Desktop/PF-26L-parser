{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Parser
  ( module Valuable,
    module DataTypes,
    Line (..),
    statement,
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

data Line = Line Expr (Maybe Expr) Int SourcePos
  deriving (Show, Eq)

blankLine :: Parser ()
blankLine = hspace *> void eol

strippedStatement :: Parser Line
strippedStatement = do
  many (try blankLine)
  spaces <- many (char ' ' <|> char '\t')
  let indent = length spaces
  pos <- getSourcePos
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
  comment <- statementEnd
  return $ Line expr comment indent pos

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
  expr <- try commentExpr <|> try ifExpr <|> try forExpr <|> try whileExpr <|> try assign <|> try functionDeclarationExpr <|> try classExpr <|> try returnExpr <|> valuable
  _comment <- statementEnd
  return expr
