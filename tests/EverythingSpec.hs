{-# LANGUAGE OverloadedStrings #-}
module EverythingSpec (spec) where
import DataTypes
import Parser(statement)
import Test.Hspec
import Text.Megaparsec (parse)
spec :: Spec
spec = do
    describe "Everything" $ do
        it "parses complex nested expression" $
            parse statement "" "[f(x, y=z) if (x := g(True)) > 0 else [y for y in range(10) if y != False] for x in h(1, 2) if x == True if x <= 10]"
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