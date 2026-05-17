{-# LANGUAGE OverloadedStrings #-}

module Assignment (spec) where

import Data.Either (isLeft)
import DataTypes
import Parser (statement)
import Test.Hspec
import Text.Megaparsec (parse)
import Valuable

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
      describe "compOp extensions" $ do
        describe "in operator" $ do
          it "parses x in y" $
            parse statement "" "x in y" `shouldBe` Right (BoolMathExpr In (Var "x") (Var "y"))
          it "parses x in range(10)" $
            parse statement "" "x in range(10)" `shouldBe` Right (BoolMathExpr In (Var "x") (Call "range" [PosArg (Number 10)]))
          it "parses x in [1, 2, 3]" $
            parse statement "" "x in y" `shouldBe` Right (BoolMathExpr In (Var "x") (Var "y"))
          it "parses number in list comp" $
            parse statement "" "x in [y for y in z]" `shouldBe` Right (BoolMathExpr In (Var "x") (ListCompExpr (Var "y") (ForLoop "y" (Var "z")) []))
        describe "not in operator" $ do
          it "parses x not in y" $
            parse statement "" "x not in y" `shouldBe` Right (BoolMathExpr NotIn (Var "x") (Var "y"))
          it "parses x not in range(10)" $
            parse statement "" "x not in range(10)" `shouldBe` Right (BoolMathExpr NotIn (Var "x") (Call "range" [PosArg (Number 10)]))
        describe "is operator" $ do
          it "parses x is None" $
            parse statement "" "x is None" `shouldBe` Right (BoolMathExpr Is (Var "x") (Lit KNone))
          it "parses x is True" $
            parse statement "" "x is True" `shouldBe` Right (BoolMathExpr Is (Var "x") (Lit KTrue))
          it "parses x is False" $
            parse statement "" "x is False" `shouldBe` Right (BoolMathExpr Is (Var "x") (Lit KFalse))
          it "parses x is y" $
            parse statement "" "x is y" `shouldBe` Right (BoolMathExpr Is (Var "x") (Var "y"))
        describe "is not operator" $ do
          it "parses x is not None" $
            parse statement "" "x is not None" `shouldBe` Right (BoolMathExpr IsNot (Var "x") (Lit KNone))
          it "parses x is not True" $
            parse statement "" "x is not True" `shouldBe` Right (BoolMathExpr IsNot (Var "x") (Lit KTrue))
          it "parses x is not False" $
            parse statement "" "x is not False" `shouldBe` Right (BoolMathExpr IsNot (Var "x") (Lit KFalse))
          it "parses x is not y" $
            parse statement "" "x is not y" `shouldBe` Right (BoolMathExpr IsNot (Var "x") (Var "y"))
        describe "None literal" $ do
          it "parses None" $
            parse statement "" "x = None" `shouldBe` Right (Assign "x" (Lit KNone))
          it "parses None in comparison" $
            parse statement "" "x == None" `shouldBe` Right (BoolLogicExpr Equal (Var "x") (Lit KNone))
        describe "combined with bool operators" $ do
          it "parses x in y and z is None" $
            parse statement "" "x in y and z is None" `shouldBe` Right (BinOp And (BoolMathExpr In (Var "x") (Var "y")) (BoolMathExpr Is (Var "z") (Lit KNone)))
          it "parses x not in y or z is not None" $
            parse statement "" "x not in y or z is not None" `shouldBe` Right (BinOp Or (BoolMathExpr NotIn (Var "x") (Var "y")) (BoolMathExpr IsNot (Var "z") (Lit KNone)))
        describe "invalid input" $ do
          it "fails with just 'in'" $
            parse statement "" "in y" `shouldSatisfy` isLeft
          it "fails with just 'is'" $
            parse statement "" "is y" `shouldSatisfy` isLeft
          it "fails with 'not' alone" $
            parse statement "" "x not y" `shouldSatisfy` isLeft
      describe "listLit" $ do
        describe "basic" $ do
          it "parses empty list" $
            parse statement "" "[]" `shouldBe` Right (ListLit [])
          it "parses single number" $
            parse statement "" "[1]" `shouldBe` Right (ListLit [Number 1])
          it "parses single variable" $
            parse statement "" "[x]" `shouldBe` Right (ListLit [Var "x"])
          it "parses single string" $
            parse statement "" "[\"hello\"]" `shouldBe` Right (ListLit [StringLit "hello"])
          it "parses single bool" $
            parse statement "" "[True]" `shouldBe` Right (ListLit [Lit KTrue])
          it "parses multiple numbers" $
            parse statement "" "[1, 2, 3]" `shouldBe` Right (ListLit [Number 1, Number 2, Number 3])
          it "parses multiple variables" $
            parse statement "" "[x, y, z]" `shouldBe` Right (ListLit [Var "x", Var "y", Var "z"])
          it "parses mixed types" $
            parse statement "" "[1, x, \"hello\", True]" `shouldBe` Right (ListLit [Number 1, Var "x", StringLit "hello", Lit KTrue])
          it "parses trailing comma" $
            parse statement "" "[1, 2, 3,]" `shouldBe` Right (ListLit [Number 1, Number 2, Number 3])
        describe "with expressions" $ do
          it "parses math expression as element" $
            parse statement "" "[x + 1]" `shouldBe` Right (ListLit [Add (Var "x") (Number 1)])
          it "parses ternary as element" $
            parse statement "" "[x if x > 0 else 0]" `shouldBe` Right (ListLit [Ternary (IfExpr (BoolMathExpr Gt (Var "x") (Number 0))) (Var "x") (Number 0)])
          it "parses function call as element" $
            parse statement "" "[f(x)]" `shouldBe` Right (ListLit [Call "f" [PosArg (Var "x")]])
          it "parses list comp as element" $
            parse statement "" "[[x for x in y]]" `shouldBe` Right (ListLit [ListCompExpr (Var "x") (ForLoop "x" (Var "y")) []])
          it "parses nested list" $
            parse statement "" "[[1, 2], [3, 4]]" `shouldBe` Right (ListLit [ListLit [Number 1, Number 2], ListLit [Number 3, Number 4]])
        describe "multiline" $ do
          it "parses multiline list" $
            parse statement "" "[\n    1,\n    2,\n    3\n]" `shouldBe` Right (ListLit [Number 1, Number 2, Number 3])
          it "parses multiline with trailing comma" $
            parse statement "" "[\n    1,\n    2,\n    3,\n]" `shouldBe` Right (ListLit [Number 1, Number 2, Number 3])
        describe "invalid input" $ do
          it "fails with no closing bracket" $
            parse statement "" "[1, 2" `shouldSatisfy` isLeft
          it "fails with no opening bracket" $
            parse statement "" "1, 2]" `shouldSatisfy` isLeft
          it "fails with empty input" $
            parse statement "" "" `shouldSatisfy` isLeft
        describe "nested" $ do
          it "parses list as function argument" $
            parse statement "" "f([1, 2, 3])" `shouldBe` Right (Call "f" [PosArg (ListLit [Number 1, Number 2, Number 3])])
          it "parses list in ternary" $
            parse statement "" "[1, 2] if True else [3, 4]" `shouldBe` Right (Ternary (IfExpr (Lit KTrue)) (ListLit [Number 1, Number 2]) (ListLit [Number 3, Number 4]))
          it "parses list in list comp" $
            parse statement "" "[x for x in [1, 2, 3]]" `shouldBe` Right (ListCompExpr (Var "x") (ForLoop "x" (ListLit [Number 1, Number 2, Number 3])) [])
          it "parses list multiplication" $
            parse statement "" "[1, 2] * 3" `shouldBe` Right (Mult (ListLit [Number 1, Number 2]) (Number 3))
          it "parses list addition" $
            parse statement "" "[1, 2] + [3, 4]" `shouldBe` Right (Add (ListLit [Number 1, Number 2]) (ListLit [Number 3, Number 4]))
      describe "mathAssign" $ do
        describe "add assign" $ do
          it "parses x += 1" $
            parse statement "" "x += 1" `shouldBe` Right (AddAssign (Var "x") (Number 1))
          it "parses x += y" $
            parse statement "" "x += y" `shouldBe` Right (AddAssign (Var "x") (Var "y"))
          it "parses x += y + 1" $
            parse statement "" "x += y + 1" `shouldBe` Right (AddAssign (Var "x") (Add (Var "y") (Number 1)))
        describe "sub assign" $ do
          it "parses x -= 1" $
            parse statement "" "x -= 1" `shouldBe` Right (SubAssign (Var "x") (Number 1))
          it "parses x -= y" $
            parse statement "" "x -= y" `shouldBe` Right (SubAssign (Var "x") (Var "y"))
          it "parses x -= y - 1" $
            parse statement "" "x -= y - 1" `shouldBe` Right (SubAssign (Var "x") (Sub (Var "y") (Number 1)))
        describe "mult assign" $ do
          it "parses x *= 2" $
            parse statement "" "x *= 2" `shouldBe` Right (MultAssign (Var "x") (Number 2))
          it "parses x *= y" $
            parse statement "" "x *= y" `shouldBe` Right (MultAssign (Var "x") (Var "y"))
        describe "div assign" $ do
          it "parses x /= 2" $
            parse statement "" "x /= 2" `shouldBe` Right (DivAssign (Var "x") (Number 2))
          it "parses x /= y" $
            parse statement "" "x /= y" `shouldBe` Right (DivAssign (Var "x") (Var "y"))
        describe "with expressions" $ do
          it "parses x += f(y)" $
            parse statement "" "x += f(y)" `shouldBe` Right (AddAssign (Var "x") (Call "f" [PosArg (Var "y")]))
          it "parses x += [1, 2, 3]" $
            parse statement "" "x += [1, 2, 3]" `shouldBe` Right (AddAssign (Var "x") (ListLit [Number 1, Number 2, Number 3]))
        describe "invalid input" $ do
          it "fails with no right side" $
            parse statement "" "x +=" `shouldSatisfy` isLeft
          it "fails with no left side" $
            parse statement "" "+= 1" `shouldSatisfy` isLeft
          it "fails with empty input" $
            parse statement "" "" `shouldSatisfy` isLeft
        describe "nested" $ do
          it "parses x += y += 1" $
            parse statement "" "x += y + z" `shouldBe` Right (AddAssign (Var "x") (Add (Var "y") (Var "z")))
      describe "dot and index access" $ do
        describe "basic dot access" $ do
          it "parses simple dot access" $
            parse statement "" "x.y" `shouldBe` Right (DotAccess (Var "x") "y")
          it "parses chained dot access" $
            parse statement "" "x.y.z" `shouldBe` Right (DotAccess (DotAccess (Var "x") "y") "z")
          it "parses deeply chained dot access" $
            parse statement "" "a.b.c.d.e" `shouldBe` Right (DotAccess (DotAccess (DotAccess (DotAccess (Var "a") "b") "c") "d") "e")
        describe "basic index access" $ do
          it "parses simple index" $
            parse statement "" "x[0]" `shouldBe` Right (Index (Var "x") (Number 0))
          it "parses variable index" $
            parse statement "" "x[y]" `shouldBe` Right (Index (Var "x") (Var "y"))
          it "parses chained index" $
            parse statement "" "x[0][1]" `shouldBe` Right (Index (Index (Var "x") (Number 0)) (Number 1))
          it "parses deeply chained index" $
            parse statement "" "x[0][1][2][3]" `shouldBe` Right (Index (Index (Index (Index (Var "x") (Number 0)) (Number 1)) (Number 2)) (Number 3))
        describe "mixed dot and index" $ do
          it "parses dot then index" $
            parse statement "" "x.y[0]" `shouldBe` Right (Index (DotAccess (Var "x") "y") (Number 0))
          it "parses index then dot" $
            parse statement "" "x[0].y" `shouldBe` Right (DotAccess (Index (Var "x") (Number 0)) "y")
          it "parses alternating dot and index" $
            parse statement "" "x.y[0].z[1]" `shouldBe` Right (Index (DotAccess (Index (DotAccess (Var "x") "y") (Number 0)) "z") (Number 1))
          it "parses deeply mixed" $
            parse statement "" "a.b[0].c[1].d[2]" `shouldBe` Right (Index (DotAccess (Index (DotAccess (Index (DotAccess (Var "a") "b") (Number 0)) "c") (Number 1)) "d") (Number 2))
        describe "calls with dot and index" $ do
          it "parses call then dot" $
            parse statement "" "f().x" `shouldBe` Right (DotAccess (Call "f" []) "x")
          it "parses call then index" $
            parse statement "" "f()[0]" `shouldBe` Right (Index (Call "f" []) (Number 0))
          it "parses dot then call" $
            parse statement "" "x.f()" `shouldBe` Right (Call' (DotAccess (Var "x") "f") [])
          it "parses chained method calls" $
            parse statement "" "x.f().g()" `shouldBe` Right (Call' (DotAccess (Call' (DotAccess (Var "x") "f") []) "g") [])
          it "parses call with args then dot" $
            parse statement "" "f(x, y).z" `shouldBe` Right (DotAccess (Call "f" [PosArg (Var "x"), PosArg (Var "y")]) "z")
          it "parses call with args then index" $
            parse statement "" "f(x, y)[0]" `shouldBe` Right (Index (Call "f" [PosArg (Var "x"), PosArg (Var "y")]) (Number 0))
          it "parses method call with args then index" $
            parse statement "" "x.f(y)[0]" `shouldBe` Right (Index (Call' (DotAccess (Var "x") "f") [PosArg (Var "y")]) (Number 0))
          it "parses index then call" $
            parse statement "" "x[0]()" `shouldBe` Right (Call' (Index (Var "x") (Number 0)) [])
        describe "esoteric combinations" $ do
          it "parses list literal then index" $
            parse statement "" "[1, 2, 3][0]" `shouldBe` Right (Index (ListLit [Number 1, Number 2, Number 3]) (Number 0))
          it "parses string then index" $
            parse statement "" "\"hello\"[0]" `shouldBe` Right (Index (StringLit "hello") (Number 0))
          it "parses parens then dot" $
            parse statement "" "(x + y).real" `shouldBe` Right (DotAccess (Add (Var "x") (Var "y")) "real")
          it "parses parens then index" $
            parse statement "" "(x + y)[0]" `shouldBe` Right (Index (Add (Var "x") (Var "y")) (Number 0))
          it "parses list comp then index" $
            parse statement "" "[x for x in y][0]" `shouldBe` Right (Index (ListCompExpr (Var "x") (ForLoop "x" (Var "y")) []) (Number 0))
          it "parses nested index as index" $
            parse statement "" "x[y[0]]" `shouldBe` Right (Index (Var "x") (Index (Var "y") (Number 0)))
          it "parses call result as index" $
            parse statement "" "x[f(y)]" `shouldBe` Right (Index (Var "x") (Call "f" [PosArg (Var "y")]))
          it "parses ternary as index" $
            parse statement "" "x[y if y > 0 else 0]" `shouldBe` Right (Index (Var "x") (Ternary (IfExpr (BoolMathExpr Gt (Var "y") (Number 0))) (Var "y") (Number 0)))
          it "parses list comp result as index target" $
            parse statement "" "[f(x) for x in y][0].z"
              `shouldBe` Right
                (DotAccess (Index (ListCompExpr (Call "f" [PosArg (Var "x")]) (ForLoop "x" (Var "y")) []) (Number 0)) "z")
          it
            "parses deeply nested method chain"
            $ parse statement "" "a.b().c[0].d().e[1].f"
              `shouldBe` Right
                ( DotAccess
                    ( Index
                        ( DotAccess
                            ( Call'
                                ( DotAccess
                                    ( Index
                                        ( DotAccess
                                            ( Call'
                                                (DotAccess (Var "a") "b")
                                                []
                                            )
                                            "c"
                                        )
                                        (Number 0)
                                    )
                                    "d"
                                )
                                []
                            )
                            "e"
                        )
                        (Number 1)
                    )
                    "f"
                )
          it "parses call on string literal" $
            parse statement "" "\"hello\".upper()" `shouldBe` Right (Call' (DotAccess (StringLit "hello") "upper") [])
          it "parses multiline index" $
            parse statement "" "x[\n    0\n]" `shouldBe` Right (Index (Var "x") (Number 0))
          it "parses bool literal then method call" $
            parse statement "" "True.__class__()" `shouldBe` Right (Call' (DotAccess (Lit KTrue) "__class__") [])
        describe "invalid input" $ do
          it "fails with trailing dot" $
            parse statement "" "x." `shouldSatisfy` isLeft
          it "fails with empty index" $
            parse statement "" "x[]" `shouldSatisfy` isLeft
          it "fails with unclosed index" $
            parse statement "" "x[0" `shouldSatisfy` isLeft
          it "fails with double dot" $
            parse statement "" "x..y" `shouldSatisfy` isLeft