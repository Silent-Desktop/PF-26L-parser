module Main where

import qualified Data.Text as T
import Parser
import Text.Megaparsec

main :: IO ()
main = do
  let input = "if x*10<10 and y>10 or z-5==0 and True!=False:"
  case parse ifExpr "" (T.pack input) of
    Left err -> putStrLn (errorBundlePretty err)
    Right result -> print result