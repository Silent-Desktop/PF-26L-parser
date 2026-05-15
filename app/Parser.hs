{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Parser where

import qualified Data.Text.IO ()
import DataTypes
import Math
import Text.Megaparsec
import Valuable

statement :: Parser Expr
statement = try ifExpr <|> try functionDeclarationExpr <|> valuable