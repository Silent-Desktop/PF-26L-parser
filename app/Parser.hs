{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Parser where

import qualified Data.Text.IO ()
import DataTypes
import Text.Megaparsec
import Valuable
import Text.Megaparsec.Char (hspace, eol)
import Control.Monad (void)
data Line = Line Expr (Maybe Expr) deriving (Show, Eq)

statementEnd :: Parser (Maybe Expr)
statementEnd = do
    hspace
    comment <- optional commentExpr
    void eol <|> eof
    return comment

statement :: Parser Expr
statement = do
    expr <- try commentExpr <|> try ifExpr <|> try forExpr <|> try whileExpr <|> try assign <|> try functionDeclarationExpr<|>  try boolLogicExpr <|> try boolMathExpr <|> try returnExpr <|> valuable
    _comment <- statementEnd
    return $  expr 

statementWithLine :: Parser Line
statementWithLine = do
    expr <- try commentExpr <|> try ifExpr <|> try forExpr <|> try whileExpr <|> try functionDeclarationExpr <|> try assign <|> try boolLogicExpr <|> try boolMathExpr <|> try returnExpr <|> valuable
    comment <- statementEnd
    return $ Line expr comment