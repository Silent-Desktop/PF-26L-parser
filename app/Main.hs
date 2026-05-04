{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Void
import qualified Data.Text as T
import Data.Text (Text)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Text.IO()

type Parser = Parsec Void Text

data Expr
  = Number Int
  | Var String
  | Assign String Expr
  deriving (Show)

pr :: Parser ()
pr = L.space space1 empty empty

helper :: Parser a -> Parser a
helper = L.lexeme pr

symbol :: Text -> Parser Text
symbol = L.symbol pr

number :: Parser Expr
number = Number <$> helper L.decimal

identifier :: Parser String
identifier = helper ((:) <$> letterChar <*> many alphaNumChar)
variable :: Parser Expr
variable = Var <$> identifier

assign :: Parser Expr
assign = do
  name <- identifier
  symbol "="
  expr <- number <|> variable
  return $ Assign name expr
main :: IO()
main = do
    let input = "x = 6"
    case parse assign "" (T.pack input) of
        Left err -> putStrLn (errorBundlePretty err)
        Right result -> print result