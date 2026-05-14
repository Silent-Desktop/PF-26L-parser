module Main where

import qualified Assignment
import Test.Hspec

main :: IO ()
main = hspec $ do
  Assignment.spec