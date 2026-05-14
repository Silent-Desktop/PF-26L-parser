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
  describe "mathExpr" $ do
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
  describe "boolMathExpr" $ do
    describe "simple comparisons" $ do
      it "parses greater than" $
        parse boolMathExpr "" "x > 6" `shouldBe` Right (BoolMathExpr Gt (Var "x") (Number 6))
      it "parses less than" $
        parse boolMathExpr "" "x < 6" `shouldBe` Right (BoolMathExpr Lt (Var "x") (Number 6))
      it "parses greater than or equal" $
        parse boolMathExpr "" "x >= 6" `shouldBe` Right (BoolMathExpr Gte (Var "x") (Number 6))
      it "parses less than or equal" $
        parse boolMathExpr "" "x <= 6" `shouldBe` Right (BoolMathExpr Lte (Var "x") (Number 6))
      it "parses equal" $
        parse boolMathExpr "" "x == 6" `shouldBe` Right (BoolMathExpr (EqComp Equal) (Var "x") (Number 6))
      it "parses not equal" $
        parse boolMathExpr "" "x != 6" `shouldBe` Right (BoolMathExpr (EqComp Neq) (Var "x") (Number 6))
    describe "math on either side" $ do
      it "parses math on the left" $
        parse boolMathExpr "" "x + 1 > 6" `shouldBe` Right (BoolMathExpr Gt (Add (Var "x") (Number 1)) (Number 6))
      it "parses math on the right" $
        parse boolMathExpr "" "x > y + 1" `shouldBe` Right (BoolMathExpr Gt (Var "x") (Add (Var "y") (Number 1)))
      it "parses math on both sides" $
        parse boolMathExpr "" "x + 1 > y - 2" `shouldBe` Right (BoolMathExpr Gt (Add (Var "x") (Number 1)) (Sub (Var "y") (Number 2)))
    describe "invalid input" $ do
      it "fails with no operator" $
        parse boolMathExpr "" "x y" `shouldSatisfy` isLeft
      it "fails with empty input" $
        parse boolMathExpr "" "" `shouldSatisfy` isLeft
    describe "boolLogicExpr" $ do
      describe "equality" $ do
        it "parses True == True" $
          parse boolLogicExpr "" "True == True" `shouldBe` Right (BoolLogicExpr Equal (Lit KTrue) (Lit KTrue))
        it "parses True == False" $
          parse boolLogicExpr "" "True == False" `shouldBe` Right (BoolLogicExpr Equal (Lit KTrue) (Lit KFalse))
        it "parses False == False" $
          parse boolLogicExpr "" "False == False" `shouldBe` Right (BoolLogicExpr Equal (Lit KFalse) (Lit KFalse))
      describe "inequality" $ do
        it "parses True != False" $
          parse boolLogicExpr "" "True != False" `shouldBe` Right (BoolLogicExpr Neq (Lit KTrue) (Lit KFalse))
        it "parses False != True" $
          parse boolLogicExpr "" "False != True" `shouldBe` Right (BoolLogicExpr Neq (Lit KFalse) (Lit KTrue))
      describe "with variables" $ do
        it "parses var == True" $
          parse boolLogicExpr "" "x == True" `shouldBe` Right (BoolLogicExpr Equal (Var "x") (Lit KTrue))
        it "parses var == False" $
          parse boolLogicExpr "" "x == False" `shouldBe` Right (BoolLogicExpr Equal (Var "x") (Lit KFalse))
        it "parses var != True" $
          parse boolLogicExpr "" "x != True" `shouldBe` Right (BoolLogicExpr Neq (Var "x") (Lit KTrue))
        it "parses two vars" $
          parse boolLogicExpr "" "x == y" `shouldBe` Right (BoolLogicExpr Equal (Var "x") (Var "y"))
      describe "invalid input" $ do
        it "fails with no operator" $
          parse boolLogicExpr "" "True False" `shouldSatisfy` isLeft
        it "fails with empty input" $
          parse boolLogicExpr "" "" `shouldSatisfy` isLeft
        it "fails with math operator" $
          parse boolLogicExpr "" "True > False" `shouldSatisfy` isLeft
      describe "ifExpr" $ do
        describe "with bool literals" $ do
          it "parses if True:" $
            parse ifExpr "" "if True:" `shouldBe` Right (Conditional (Lit KTrue))
          it "parses if False:" $
            parse ifExpr "" "if False:" `shouldBe` Right (Conditional (Lit KFalse))
        describe "with comparisons" $ do
          it "parses if x > 6:" $
            parse ifExpr "" "if x > 6:" `shouldBe` Right (Conditional (BoolMathExpr Gt (Var "x") (Number 6)))
          it "parses if x == 6:" $
            parse ifExpr "" "if x == 6:" `shouldBe` Right (Conditional (BoolMathExpr (EqComp Equal) (Var "x") (Number 6)))
          it "parses if x != y:" $
            parse ifExpr "" "if x != y:" `shouldBe` Right (Conditional (BoolLogicExpr (Neq) (Var "x") (Var "y")))
        describe "with bool logic" $ do
          it "parses if x == True:" $
            parse ifExpr "" "if x == True:" `shouldBe` Right (Conditional (BoolLogicExpr Equal (Var "x") (Lit KTrue)))
          it "parses if True == False:" $
            parse ifExpr "" "if True == False:" `shouldBe` Right (Conditional (BoolLogicExpr Equal (Lit KTrue) (Lit KFalse)))
        describe "with and/or" $ do
          it "parses if x > 6 and y < 2:" $
            parse ifExpr "" "if x > 6 and y < 2:" `shouldBe` Right (Conditional (BinOp And (BoolMathExpr Gt (Var "x") (Number 6)) (BoolMathExpr Lt (Var "y") (Number 2))))
          it "parses if x > 6 or y < 2:" $
            parse ifExpr "" "if x > 6 or y < 2:" `shouldBe` Right (Conditional (BinOp Or (BoolMathExpr Gt (Var "x") (Number 6)) (BoolMathExpr Lt (Var "y") (Number 2))))
        describe "invalid input" $ do
          it "fails with no colon" $
            parse ifExpr "" "if True" `shouldSatisfy` isLeft
          it "fails with no condition" $
            parse ifExpr "" "if :" `shouldSatisfy` isLeft
          it "fails without if keyword" $
            parse ifExpr "" "x > 6:" `shouldSatisfy` isLeft
          it "fails with empty input" $
            parse ifExpr "" "" `shouldSatisfy` isLeft