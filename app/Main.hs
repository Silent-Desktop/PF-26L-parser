module Main where

import qualified Data.Text as T
import Parser
import Text.Megaparsec

main :: IO ()
main = do
  let input = "def hii(x,y,z=\"hi\"):"
  case parse statement "" (T.pack input) of
    Left err -> putStrLn (errorBundlePretty err)
    Right result -> print result