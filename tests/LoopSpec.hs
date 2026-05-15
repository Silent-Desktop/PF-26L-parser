{-# LANGUAGE OverloadedStrings #-}

module LoopSpec (spec) where

import Data.Either (isLeft)
import DataTypes
import Parser
import Test.Hspec
import Text.Megaparsec (parse)
import Math

spec :: Spec
spec = do
    describe "whileExpr" $ do
        describe "with bool literals" $ do
            it "parses while True:" $
                parse statement "" "while True:" `shouldBe` Right (WhileLoop (Lit KTrue))
            it "parses while False:" $
                parse statement "" "while False:" `shouldBe` Right (WhileLoop (Lit KFalse))
        describe "with comparisons" $ do
            it "parses while x > 6:" $
                parse statement "" "while x > 6:" `shouldBe` Right (WhileLoop (BoolMathExpr Gt (Var "x") (Number 6)))
            it "parses while x == 6:" $
                parse statement "" "while x == 6:" `shouldBe` Right (WhileLoop (BoolLogicExpr Equal (Var "x") (Number 6)))
            it "parses while x != y:" $
                parse statement "" "while x != y:" `shouldBe` Right (WhileLoop (BoolLogicExpr Neq (Var "x") (Var "y")))
        describe "with bool logic" $ do
            it "parses while x == True:" $
                parse statement "" "while x == True:" `shouldBe` Right (WhileLoop (BoolLogicExpr Equal (Var "x") (Lit KTrue)))
            it "parses while True == False:" $
                parse statement "" "while True == False:" `shouldBe` Right (WhileLoop (BoolLogicExpr Equal (Lit KTrue) (Lit KFalse)))
        describe "with and/or" $ do
            it "parses while x > 6 and y < 2:" $
                parse statement "" "while x > 6 and y < 2:" `shouldBe` Right (WhileLoop (BinOp And (BoolMathExpr Gt (Var "x") (Number 6)) (BoolMathExpr Lt (Var "y") (Number 2))))
            it "parses while x > 6 or y < 2:" $
                parse statement "" "while x > 6 or y < 2:" `shouldBe` Right (WhileLoop (BinOp Or (BoolMathExpr Gt (Var "x") (Number 6)) (BoolMathExpr Lt (Var "y") (Number 2))))
        describe "invalid input" $ do
            it "fails with no colon" $
                parse statement "" "while True" `shouldSatisfy` isLeft
            it "fails with no condition" $
                parse statement "" "while :" `shouldSatisfy` isLeft
            it "fails without while keyword" $
                parse statement "" "x > 6:" `shouldSatisfy` isLeft
            it "fails with empty input" $
                parse statement "" "" `shouldSatisfy` isLeft
    describe "statement" $ do
        describe "with variables" $ do
            it "parses for x in y:" $
                parse statement "" "for x in y:" `shouldBe` Right (ForLoop "x" (Var "y"))
            it "parses for item in items:" $
                parse statement "" "for item in items:" `shouldBe` Right (ForLoop "item" (Var "items"))
        describe "with function calls" $ do
            it "parses for x in range(10):" $
                parse statement "" "for x in range(10):" `shouldBe` Right (ForLoop "x" (Call "range" [PosArg (Number 10)]))
            it "parses for x in range(1, 10):" $
                parse statement "" "for x in range(1, 10):" `shouldBe` Right (ForLoop "x" (Call "range" [PosArg (Number 1), PosArg (Number 10)]))
            it "parses for x in enumerate(items):" $
                parse statement "" "for x in enumerate(items):" `shouldBe` Right (ForLoop "x" (Call "enumerate" [PosArg (Var "items")]))
        describe "invalid input" $ do
            it "fails with no colon" $
                parse statement "" "for x in y" `shouldSatisfy` isLeft
            it "fails with no iterable" $
                parse statement "" "for x in :" `shouldSatisfy` isLeft
            it "fails with no variable" $
                parse statement "" "for in y:" `shouldSatisfy` isLeft
            it "fails without for keyword" $
                parse statement "" "x in y:" `shouldSatisfy` isLeft
            it "fails with empty input" $
                parse statement "" "" `shouldSatisfy` isLeft
    describe "listCompExpr" $ do
        describe "basic" $ do
            it "parses simple list comp" $
                parse listCompOptIfExpr "" "[x for x in y:]" `shouldBe` Right (ListCompExpr (Var "x") (ForLoop "x" (Var "y")) [])
            it "parses list comp with expression" $
                parse listCompOptIfExpr "" "[x + 1 for x in y:]" `shouldBe` Right (ListCompExpr (Add (Var "x") (Number 1)) (ForLoop "x" (Var "y")) [])
            it "parses list comp with range" $
                parse listCompOptIfExpr "" "[x for x in range(10):]" `shouldBe` Right (ListCompExpr (Var "x") (ForLoop "x" (Call "range" [PosArg (Number 10)])) [])
            it "parses list comp with function call as value" $
                parse listCompOptIfExpr "" "[f(x) for x in y:]" `shouldBe` Right (ListCompExpr (Call "f" [PosArg (Var "x")]) (ForLoop "x" (Var "y")) [])
        describe "with conditions" $ do
            it "parses list comp with if" $
                parse listCompOptIfExpr "" "[x for x in y: if x > 0]" `shouldBe` Right (ListCompExpr (Var "x") (ForLoop "x" (Var "y")) [IfExpr (BoolMathExpr Gt (Var "x") (Number 0))])
            it "parses list comp with multiple ifs" $
                parse listCompOptIfExpr "" "[x for x in y: if x > 0 if x < 10]" `shouldBe` Right (ListCompExpr (Var "x") (ForLoop "x" (Var "y")) [IfExpr (BoolMathExpr Gt (Var "x") (Number 0)), IfExpr (BoolMathExpr Lt (Var "x") (Number 10))])
        describe "invalid input" $ do
            it "fails with no closing bracket" $
                parse listCompOptIfExpr "" "[x for x in y:" `shouldSatisfy` isLeft
            it "fails with no for" $
                parse listCompOptIfExpr "" "[x in y:]" `shouldSatisfy` isLeft
            it "fails with no opening bracket" $
                parse listCompOptIfExpr "" "x for x in y:" `shouldSatisfy` isLeft
            it "fails with empty input" $
                parse listCompOptIfExpr "" "" `shouldSatisfy` isLeft