{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Utils where

import Data.Text (Text)
import qualified Data.Text.IO ()
import DataTypes
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

pr :: Parser ()
pr = L.space space1 empty empty

helper :: Parser a -> Parser a
helper = L.lexeme pr

reserved :: [String]
reserved = ["if", "else", "while", "for", "def", "return", "True", "False"]

identifier :: Parser String
identifier = helper $ do
    name <- (:) <$> (letterChar <|> char '_') <*> many (alphaNumChar <|> char '_')
    if name `elem` reserved
        then fail $ name ++ " is a reserved keyword"
        else return name

variable :: Parser Expr
variable = Var <$> identifier

keyword :: Text -> Parser Text
keyword kw = helper (string kw <* notFollowedBy alphaNumChar)

symbol :: Text -> Parser Text
symbol = L.symbol pr

