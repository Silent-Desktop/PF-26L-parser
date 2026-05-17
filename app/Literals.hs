{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Literals where

import qualified Data.Text.IO ()
import DataTypes
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Utils

integer :: Parser Expr
integer = Number <$> helper L.decimal

boolLit :: Parser Expr
boolLit =
  (keyword "True" >> return (Lit KTrue))
    <|> (keyword "False" >> return (Lit KFalse))

number :: Parser Expr
number = try float <|> integer

float :: Parser Expr
float = FloatNum <$> helper L.float

stringChar :: Char -> Parser Char
stringChar quote =
  (char '\\' >> escapedChar quote)
    <|> anySingleBut quote

escapedChar :: Char -> Parser Char
escapedChar quote =
  (char quote >> return quote) -- \" or \'
    <|> (char 'n' >> return '\n')
    <|> (char 't' >> return '\t')
    <|> (char '\\' >> return '\\')
    <|> (char 'r' >> return '\r')

tripleDouble :: Parser Expr
tripleDouble = do
  string "\"\"\""
  content <- manyTill L.charLiteral (string "\"\"\"")
  return $ MultilineStringLit content

tripleSingle :: Parser Expr
tripleSingle = do
  string "'''"
  content <- manyTill L.charLiteral (string "'''")
  return $ MultilineStringLit content

stringLit :: Parser Expr
stringLit =
  try tripleDouble
    <|> try tripleSingle
    <|> try singleQuoted
    <|> doubleQuoted
  where
    doubleQuoted = StringLit <$> (char '"' *> manyTill (stringChar '"') (char '"') <* notFollowedBy (char '"'))
    singleQuoted = StringLit <$> (char '\'' *> manyTill (stringChar '\'') (char '\'') <* notFollowedBy (char '\''))

noneLit :: Parser Expr
noneLit = keyword "None" >> return (Lit KNone)