{-# LANGUAGE OverloadedStrings #-}

module Assignment (spec) where

import Data.Either (isLeft)
import DataTypes
import Math
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
  describe "mathExpr" $ do
    it "adds two numbers" $
      parse statement "" "6+6" `shouldBe` Right (Add (Number 6) (Number 6))
    it "adds var and number" $
      parse statement "" "x+6" `shouldBe` Right (Add (Var "x") (Number 6))
    it "adds number and var" $
      parse statement "" "6+x" `shouldBe` Right (Add (Number 6) (Var "x"))
    it "adds two vars" $
      parse statement "" "x+y" `shouldBe` Right (Add (Var "x") (Var "y"))
    it "adds three vars" $
      parse statement "" "x+y+z" `shouldBe` Right (Add (Add (Var "x") (Var "y")) (Var "z"))
  describe "statement" $ do
    describe "simple comparisons" $ do
      it "parses greater than" $
        parse statement "" "x > 6" `shouldBe` Right (BoolMathExpr Gt (Var "x") (Number 6))
      it "parses less than" $
        parse statement "" "x < 6" `shouldBe` Right (BoolMathExpr Lt (Var "x") (Number 6))
      it "parses greater than or equal" $
        parse statement "" "x >= 6" `shouldBe` Right (BoolMathExpr Gte (Var "x") (Number 6))
      it "parses less than or equal" $
        parse statement "" "x <= 6" `shouldBe` Right (BoolMathExpr Lte (Var "x") (Number 6))
      it "parses equal" $
        parse statement "" "x == 6" `shouldBe` Right (BoolLogicExpr Equal (Var "x") (Number 6))
      it "parses not equal" $
        parse statement "" "x != 6" `shouldBe` Right (BoolLogicExpr Neq (Var "x") (Number 6))
    describe "math on either side" $ do
      it "parses math on the left" $
        parse statement "" "x + 1 > 6" `shouldBe` Right (BoolMathExpr Gt (Add (Var "x") (Number 1)) (Number 6))
      it "parses math on the right" $
        parse statement "" "x > y + 1" `shouldBe` Right (BoolMathExpr Gt (Var "x") (Add (Var "y") (Number 1)))
      it "parses math on both sides" $
        parse statement "" "x + 1 > y - 2" `shouldBe` Right (BoolMathExpr Gt (Add (Var "x") (Number 1)) (Sub (Var "y") (Number 2)))
    describe "invalid input" $ do
      it "fails with no operator" $
        parse statement "" "x y" `shouldSatisfy` isLeft
      it "fails with empty input" $
        parse statement "" "" `shouldSatisfy` isLeft
    describe "statement" $ do
      describe "equality" $ do
        it "parses True == True" $
          parse statement "" "True == True" `shouldBe` Right (BoolLogicExpr Equal (Lit KTrue) (Lit KTrue))
        it "parses True == False" $
          parse statement "" "True == False" `shouldBe` Right (BoolLogicExpr Equal (Lit KTrue) (Lit KFalse))
        it "parses False == False" $
          parse statement "" "False == False" `shouldBe` Right (BoolLogicExpr Equal (Lit KFalse) (Lit KFalse))
      describe "inequality" $ do
        it "parses True != False" $
          parse statement "" "True != False" `shouldBe` Right (BoolLogicExpr Neq (Lit KTrue) (Lit KFalse))
        it "parses False != True" $
          parse statement "" "False != True" `shouldBe` Right (BoolLogicExpr Neq (Lit KFalse) (Lit KTrue))
      describe "with variables" $ do
        it "parses var == True" $
          parse statement "" "x == True" `shouldBe` Right (BoolLogicExpr Equal (Var "x") (Lit KTrue))
        it "parses var == False" $
          parse statement "" "x == False" `shouldBe` Right (BoolLogicExpr Equal (Var "x") (Lit KFalse))
        it "parses var != True" $
          parse statement "" "x != True" `shouldBe` Right (BoolLogicExpr Neq (Var "x") (Lit KTrue))
        it "parses two vars" $
          parse statement "" "x == y" `shouldBe` Right (BoolLogicExpr Equal (Var "x") (Var "y"))
      describe "invalid input" $ do
        it "fails with no operator" $
          parse statement "" "True False" `shouldSatisfy` isLeft
        it "fails with empty input" $
          parse statement "" "" `shouldSatisfy` isLeft
        it "fails with math operator" $
          parse statement "" "True > False" `shouldBe` Right (BoolMathExpr Gt (Lit KTrue) (Lit KFalse))
      describe "statement" $ do
        describe "with bool literals" $ do
          it "parses if True:" $
            parse statement "" "if True:" `shouldBe` Right (IfExpr (Lit KTrue))
          it "parses if False:" $
            parse statement "" "if False:" `shouldBe` Right (IfExpr (Lit KFalse))
        describe "with comparisons" $ do
          it "parses if x > 6:" $
            parse statement "" "if x > 6: " `shouldBe` Right (IfExpr (BoolMathExpr Gt (Var "x") (Number 6)))
          it "parses if x == 6:" $
            parse statement "" "if x == 6: " `shouldBe` Right (IfExpr (BoolLogicExpr (Equal) (Var "x") (Number 6)))
          it "parses if x != y:" $
            parse statement "" "if x != y: " `shouldBe` Right (IfExpr (BoolLogicExpr (Neq) (Var "x") (Var "y")))
        describe "with bool logic" $ do
          it "parses if x == True:" $
            parse statement "" "if x == True: " `shouldBe` Right (IfExpr (BoolLogicExpr Equal (Var "x") (Lit KTrue)))
          it "parses if True == False:" $
            parse statement "" "if True == False: " `shouldBe` Right (IfExpr (BoolLogicExpr Equal (Lit KTrue) (Lit KFalse)))
        describe "with and/or" $ do
          it "parses if x > 6 and y < 2:" $
            parse statement "" "if x > 6.0 and y < 2: " `shouldBe` Right (IfExpr (BinOp And (BoolMathExpr Gt (Var "x") (FloatNum 6.0)) (BoolMathExpr Lt (Var "y") (Number 2))))
          it "parses if x > 6 or y < 2:" $
            parse statement "" "if x > 6 or y < 2: " `shouldBe` Right (IfExpr (BinOp Or (BoolMathExpr Gt (Var "x") (Number 6)) (BoolMathExpr Lt (Var "y") (Number 2))))
        describe "invalid input" $ do
          it "fails with no colon" $
            parse statement "" "if True" `shouldSatisfy` isLeft
          it "fails with no condition" $
            parse statement "" "if :" `shouldSatisfy` isLeft
          it "fails without if keyword" $
            parse statement "" "x > 6:" `shouldSatisfy` isLeft
          it "fails with empty input" $
            parse statement "" "" `shouldSatisfy` isLeft