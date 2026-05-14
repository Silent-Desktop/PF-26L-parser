{-# LANGUAGE OverloadedStrings #-}

module Assignment (spec) where

import Data.Either (isLeft)
import Parser
import Parser (mathExpr)
import Test.Hspec
import Text.Megaparsec (parse)

spec :: Spec
spec = do
  describe "assign" $ do
    it "parses a number assignment" $
      parse assign "" "x = 6" `shouldBe` Right (Assign "x" (Number 6))

    it "parses a variable assignment" $
      parse assign "" "x = y" `shouldBe` Right (Assign "x" (Var "y"))

    it "fails on missing rhs" $
      parse assign "" "x =" `shouldSatisfy` isLeft
  describe "add" $ do
    it "adds two numbers" $
      parse mathExpr "" "6+6" `shouldBe` Right (Add (Number 6) (Number 6))
    it "adds var and number" $
      parse mathExpr "" "x+6" `shouldBe` Right (Add (Var "x") (Number 6))
    it "adds number and var" $
      parse mathExpr "" "6+x" `shouldBe` Right (Add (Number 6) (Var "x"))
    it "adds two vars" $
      parse mathExpr "" "x+y" `shouldBe` Right (Add (Var "x") (Var "y"))
    it "adds three vars" $
      parse mathExpr "" "x+y+z" `shouldBe` Right (Add (Var "x") (Var "y"))