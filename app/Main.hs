
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Main where

import qualified Data.Text.IO as TIO
import Parser
import qualified Data.Text.IO ()
import Text.Megaparsec

parseFile :: FilePath -> IO ()
parseFile path = do
    contents <- TIO.readFile path
    case parse program path contents of
        Left err      -> putStrLn $ errorBundlePretty err
        Right results -> mapM_ printLine (zip [1..] results)

printLine :: (Int, Line) -> IO ()
printLine (lineNum, Line expr Nothing) =
    putStrLn $ "Line " ++ show lineNum ++ ": " ++ show expr
printLine (lineNum, Line expr (Just comment)) =
    putStrLn $ "Line " ++ show lineNum ++ ": " ++ show expr ++ " | " ++ show comment

main :: IO ()
main = parseFile "example.py"