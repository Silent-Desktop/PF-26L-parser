{-# LANGUAGE OverloadedStrings #-}

module Assignment (spec) where

import Data.Either (isLeft)
import Parser
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
      parse mathExpr "" "x+y+z" `shouldBe` Right (Add (Add (Var "x") (Var "y")) (Var "z"))
    describe "simple comparisons" $ do
      it "parses greater than" $
        parse boolMathExpr "" "x > 6" `shouldBe` Right (BoolMathExpr (Var "x") Gt (Number 6))
      it "parses less than" $
        parse boolMathExpr "" "x < 6" `shouldBe` Right (BoolMathExpr (Var "x") Lt (Number 6))
      it "parses greater than or equal" $
        parse boolMathExpr "" "x >= 6" `shouldBe` Right (BoolMathExpr (Var "x") Gte (Number 6))
      it "parses less than or equal" $
        parse boolMathExpr "" "x <= 6" `shouldBe` Right (BoolMathExpr (Var "x") Lte (Number 6))
      it "parses equal" $
        parse boolMathExpr "" "x == 6" `shouldBe` Right (BoolMathExpr (Var "x") Equal (Number 6))
      it "parses not equal" $
        parse boolMathExpr "" "x != 6" `shouldBe` Right (BoolMathExpr (Var "x") Neq (Number 6))

    describe "math on either side" $ do
      it "parses math on the left" $
        parse boolMathExpr "" "x + 1 > 6" `shouldBe` Right (BoolMathExpr (Add (Var "x") (Number 1)) Gt (Number 6))
      it "parses math on the right" $
        parse boolMathExpr "" "x > y + 1" `shouldBe` Right (BoolMathExpr (Var "x") Gt (Add (Var "y") (Number 1)))
      it "parses math on both sides" $
        parse boolMathExpr "" "x + 1 > y - 2" `shouldBe` Right (BoolMathExpr (Add (Var "x") (Number 1)) Gt (Sub (Var "y") (Number 2)))

    describe "invalid input" $ do
      it "fails with no operator" $
        parse boolMathExpr "" "x y" `shouldSatisfy` isLeft
      it "fails with empty input" $
        parse boolMathExpr "" "" `shouldSatisfy` isLeft