{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Parser where

import qualified Data.Text.IO ()
import DataTypes
import Text.Megaparsec
import Valuable
import Text.Megaparsec.Char (hspace, eol)
import Control.Monad (void)


statementEnd :: Parser ()
statementEnd = hspace *> (void eol <|> eof)

statement :: Parser Expr
statement = (try ifExpr<|> try forExpr <|> try whileExpr <|> try assign <|>try boolLogicExpr<|>try boolMathExpr <|> valuable)<* statementEnd