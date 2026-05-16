{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Main where

import Data.Text (Text)
import qualified Data.Text.IO ()
import qualified Data.Text.IO as TIO
import Data.Void
import Parser
import Text.Megaparsec

data ParseState = ParseState
  { currentIndent :: Int,
    indentStack :: [Int],
    currentBlock :: [Line],
    canHaveElse :: Bool,
    indentSize :: Int,
    requireIndent :: Bool
  }
  deriving (Show)

parseProgram :: FilePath -> IO (Either (ParseErrorBundle Text Void) [Line])
parseProgram path = do
  contents <- TIO.readFile path
  return $ parse program path contents

processLines :: [Line] -> ParseState -> IO ()
processLines [] _ = return ()
processLines (line@(Line expr comment indent sourcePos) : rest) state = do
  if requireIndent state && indent /= currentIndent state + indentSize state
    then
      fail $
        "invalid indentation at line "
          ++ show sourcePos
          ++ ", expected "
          ++ show (currentIndent state + indentSize state)
          ++ " got "
          ++ show indent
    else case expr of
      _ | isBlockStart expr -> do
        let newState = state {requireIndent = True, currentIndent = indent, canHaveElse = True}
        processLines rest newState
      ElseExpr -> do
        let newState = state {requireIndent = True, currentIndent = indent, canHaveElse = False}
        processLines rest newState
      Comment _ -> processLines rest state
      _ -> do
        let newState = state {requireIndent = False, currentIndent = indent, canHaveElse = False}
        processLines rest newState

isBlockStart :: Expr -> Bool
isBlockStart (IfExpr _) = True
isBlockStart (ElifExpr _) = True
isBlockStart (WhileLoop _) = True
isBlockStart (ForLoop _ _) = True
isBlockStart (FuncDeclExpr _ _) = True
isBlockStart (Class _ _) = True
isBlockStart _ = False

canEndIndent :: Expr -> Bool
canEndIndent (Assign _ _) = True
canEndIndent (ForLoop _ _) = True
canEndIndent expr = isValuable expr

isValuable :: Expr -> Bool
isValuable (Number _) = True
isValuable (FloatNum _) = True
isValuable (Lit _) = True
isValuable Literal = True
isValuable MathExpr = True
isValuable (Add _ _) = True
isValuable (Walrus _ _) = True
isValuable (Sub _ _) = True
isValuable (Div _ _) = True
isValuable (Mult _ _) = True
isValuable (BoolLogicExpr {}) = True
isValuable (BoolMathExpr {}) = True
isValuable (BinOp {}) = True
isValuable (ListCompExpr {}) = True
isValuable _ = False

processLine :: (Int, Line) -> IO ()
processLine (lineNum, Line expr comment indent sourcePos) = do
  putStrLn $
    "Line "
      ++ show lineNum
      ++ " (indent "
      ++ show indent
      ++ "): "
      ++ show expr
      ++ maybe "" (\c -> " | " ++ show c) comment

main :: IO ()
main = do
  result <- parseProgram "example2.py"
  let state = ParseState {currentIndent = 0, currentBlock = [], canHaveElse = False, indentSize = 4, requireIndent = False, indentStack = []}
  case result of
    Left err -> putStrLn $ errorBundlePretty err
    Right ls -> processLines ls state