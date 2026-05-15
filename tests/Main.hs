module Main where

import qualified Assignment
import qualified CallSpec
import qualified LoopSpec
import Test.Hspec

main :: IO ()
main = hspec $ do
  Assignment.spec
  CallSpec.spec
  LoopSpec.spec