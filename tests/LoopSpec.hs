{-# LANGUAGE OverloadedStrings #-}

module LoopSpec (spec) where

import Data.Either (isLeft,isRight)
import DataTypes
import Parser
import Test.Hspec
import Text.Megaparsec (parse)
import Valuable

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
    describe "forExpr" $ do
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
                parse listCompExpr "" "[x for x in y:]" `shouldBe` Right (ListCompExpr (Var "x") (ForLoop "x" (Var "y")) [])
            it "parses list comp with expression" $
                parse listCompExpr "" "[x + 1 for x in y:]" `shouldBe` Right (ListCompExpr (Add (Var "x") (Number 1)) (ForLoop "x" (Var "y")) [])
            it "parses list comp with range" $
                parse listCompExpr "" "[x for x in range(10):]" `shouldBe` Right (ListCompExpr (Var "x") (ForLoop "x" (Call "range" [PosArg (Number 10)])) [])
            it "parses list comp with function call as value" $
                parse listCompExpr "" "[f(x) for x in y:]" `shouldBe` Right (ListCompExpr (Call "f" [PosArg (Var "x")]) (ForLoop "x" (Var "y")) [])
        describe "with ternary value" $ do
            it "parses list comp with ternary value" $
                parse listCompExpr "" "[x if x > 0 else 0 for x in y:]" `shouldBe` Right (ListCompExpr (Ternary (IfExpr (BoolMathExpr Gt (Var "x") (Number 0))) (Var "x") (Number 0)) (ForLoop "x" (Var "y")) [])
            it "parses list comp with ternary and variable else" $
                parse listCompExpr "" "[x if x > 0 else z for x in y:]" `shouldBe` Right (ListCompExpr (Ternary (IfExpr (BoolMathExpr Gt (Var "x") (Number 0))) (Var "x") (Var "z")) (ForLoop "x" (Var "y")) [])
            it "parses list comp with call as ternary value" $
                parse listCompExpr "" "[f(x) if x > 0 else g(x) for x in y:]" `shouldBe` Right (ListCompExpr (Ternary (IfExpr (BoolMathExpr Gt (Var "x") (Number 0))) (Call "f" [PosArg (Var "x")]) (Call "g" [PosArg (Var "x")])) (ForLoop "x" (Var "y")) [])
            it "parses list comp with range and ternary" $
                parse listCompExpr "" "[x if x > 0 else 0 for x in range(10):]" `shouldBe` Right (ListCompExpr (Ternary (IfExpr (BoolMathExpr Gt (Var "x") (Number 0))) (Var "x") (Number 0)) (ForLoop "x" (Call "range" [PosArg (Number 10)])) [])
        describe "with conditions" $ do
            it "parses list comp with if" $
                parse listCompExpr "" "[x for x in y: if x > 0]" `shouldBe` Right (ListCompExpr (Var "x") (ForLoop "x" (Var "y")) [IfExpr (BoolMathExpr Gt (Var "x") (Number 0))])
            it "parses list comp with multiple ifs" $
                parse listCompExpr "" "[x for x in y: if x > 0 if x < 10]" `shouldBe` Right (ListCompExpr (Var "x") (ForLoop "x" (Var "y")) [IfExpr (BoolMathExpr Gt (Var "x") (Number 0)), IfExpr (BoolMathExpr Lt (Var "x") (Number 10))])
        describe "invalid input" $ do
            it "fails with no closing bracket" $
                parse listCompExpr "" "[x for x in y:" `shouldSatisfy` isLeft
            it "fails with no for" $
                parse listCompExpr "" "[x in y:]" `shouldSatisfy` isLeft
            it "fails with no opening bracket" $
                parse listCompExpr "" "x for x in y:" `shouldSatisfy` isLeft
            it "fails with no else in ternary" $
                parse listCompExpr "" "[x if x > 0 for x in y:]" `shouldSatisfy` isLeft
            it "fails with no for after ternary" $
                parse listCompExpr "" "[x if x > 0 else 0]" `shouldSatisfy` isLeft
            it "fails with empty input" $
                parse listCompExpr "" "" `shouldSatisfy` isLeft
    describe "nested expressions" $ do
        describe "nested ternary" $ do
            it "parses ternary in ternary fallback" $
                parse statement "" "x if x > 0 else y if y > 0 else 0" `shouldBe` Right (Ternary (IfExpr (BoolMathExpr Gt (Var "x") (Number 0))) (Var "x") (Ternary (IfExpr (BoolMathExpr Gt (Var "y") (Number 0))) (Var "y") (Number 0)))
            it "parses ternary as list comp value" $
                parse statement "" "[x if x > 0 else 0 for x in y:]" `shouldBe` Right (ListCompExpr (Ternary (IfExpr (BoolMathExpr Gt (Var "x") (Number 0))) (Var "x") (Number 0)) (ForLoop "x" (Var "y")) [])
            it "parses list comp as ternary fallback" $
                parse statement "" "x if x > 0 else [y for y in z:]" `shouldBe` Right (Ternary (IfExpr (BoolMathExpr Gt (Var "x") (Number 0))) (Var "x") (ListCompExpr (Var "y") (ForLoop "y" (Var "z")) []))
            it "parses list comp as ternary value" $
                parse statement "" "[x for x in y:] if True else z" `shouldBe` Right (Ternary (IfExpr (Lit KTrue)) (ListCompExpr (Var "x") (ForLoop "x" (Var "y")) []) (Var "z"))
        describe "nested list comprehensions" $ do
            it "parses list comp in list comp iterable" $
                parse statement "" "[x for x in [y for y in z:]:]" `shouldBe` Right (ListCompExpr (Var "x") (ForLoop "x" (ListCompExpr (Var "y") (ForLoop "y" (Var "z")) [])) [])
            it "parses list comp multiplied" $
                parse statement "" "[x for x in y:] * 2" `shouldBe` Right (Mult (ListCompExpr (Var "x") (ForLoop "x" (Var "y")) []) (Number 2))
            it "parses number multiplied by list comp" $
                parse statement "" "2 * [x for x in y:]" `shouldBe` Right (Mult (Number 2) (ListCompExpr (Var "x") (ForLoop "x" (Var "y")) []))
        describe "nested function calls" $ do
            it "parses call as arg to call" $
                parse statement "" "f(g(x))" `shouldBe` Right (Call "f" [PosArg (Call "g" [PosArg (Var "x")])])
            it "parses list comp as arg to call" $
                parse statement "" "f([x for x in y:])" `shouldBe` Right (Call "f" [PosArg (ListCompExpr (Var "x") (ForLoop "x" (Var "y")) [])])
            it "parses ternary as arg to call" $
                parse statement "" "f(x if x > 0 else 0)" `shouldBe` Right (Call "f" [PosArg (Ternary (IfExpr (BoolMathExpr Gt (Var "x") (Number 0))) (Var "x") (Number 0))])
            it "parses call in list comp iterable" $
                parse statement "" "[x for x in range(10):]" `shouldBe` Right (ListCompExpr (Var "x") (ForLoop "x" (Call "range" [PosArg (Number 10)])) [])
        describe "nested math" $ do
            it "parses ternary in math expression" $
                parse statement "" "(x if x > 0 else 0) + 1" `shouldSatisfy` isRight
            it "parses call result in math" $
                parse statement "" "f(x) + g(x)" `shouldBe` Right (Add (Call "f" [PosArg (Var "x")]) (Call "g" [PosArg (Var "x")]))
            it "parses list comp addition" $
                parse statement "" "[x for x in y:] + [z for z in w:]" `shouldBe` Right (Add (ListCompExpr (Var "x") (ForLoop "x" (Var "y")) []) (ListCompExpr (Var "z") (ForLoop "z" (Var "w")) []))
        describe "Everything" $ do
            it "parses complex nested expression" $
                parse statement "" "[f(x, y=z) if (x := g(True)) > 0 else [y for y in range(10): if y != False] for x in h(1, 2): if x == True if x <= 10]"
                `shouldBe` Right (ListCompExpr
                    (Ternary
                        (IfExpr (BoolMathExpr Gt (Assign "x" (Call "g" [PosArg (Lit KTrue)])) (Number 0)))
                        (Call "f" [PosArg (Var "x"), KwArg "y" (Var "z")])
                        (ListCompExpr
                            (Var "y")
                            (ForLoop "y" (Call "range" [PosArg (Number 10)]))
                            [IfExpr (BoolLogicExpr Neq (Var "y") (Lit KFalse))]))
                    (ForLoop "x" (Call "h" [PosArg (Number 1), PosArg (Number 2)]))
                    [IfExpr (BoolLogicExpr Equal (Var "x") (Lit KTrue)), IfExpr (BoolMathExpr Lte (Var "x") (Number 10))])