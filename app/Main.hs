module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Parser
import Text.Megaparsec (errorBundlePretty, parse)

indentLevel :: T.Text -> Int
indentLevel = T.length . T.takeWhile (== ' ')

parseLine :: Int -> T.Text -> IO ()
parseLine lineNum line
    | T.null (T.strip line) = return ()
    | otherwise = case parse statementWithLine (show lineNum) (T.stripStart line) of
        Left err              -> putStrLn $ "Line " ++ show lineNum ++ ": " ++ errorBundlePretty err
        Right (Line expr Nothing)      -> putStrLn $ "Line " ++ show lineNum ++ ": " ++ show expr
        Right (Line expr (Just comment)) -> putStrLn $ "Line " ++ show lineNum ++ ": " ++ show expr ++ " | " ++ show comment

parseFile :: FilePath -> IO ()
parseFile path = do
    contents <- TIO.readFile path
    let ls = zip [1..] (T.lines contents)
    mapM_ (uncurry parseLine) ls

main :: IO ()
main = parseFile "example.py"