{-# LANGUAGE OverloadedStrings #-}

module CallSpec (spec) where

import Data.Either (isLeft)
import DataTypes
import Literals
import Parser(statement)
import Test.Hspec
import Text.Megaparsec (parse)

spec :: Spec
spec = do
  describe "call" $ do
    describe "no arguments" $ do
      it "parses empty call" $
        parse statement "" "f()" `shouldBe` Right (Call "f" [])
    describe "positional arguments" $ do
      it "parses single number arg" $
        parse statement "" "f(1)" `shouldBe` Right (Call "f" [PosArg (Number 1)])
      it "parses single variable arg" $
        parse statement "" "f(x)" `shouldBe` Right (Call "f" [PosArg (Var "x")])
      it "parses single string arg" $
        parse statement "" "f(\"hello\")" `shouldBe` Right (Call "f" [PosArg (StringLit "hello")])
      it "parses multiple positional args" $
        parse statement "" "f(x, 1, \"hello\")" `shouldBe` Right (Call "f" [PosArg (Var "x"), PosArg (Number 1), PosArg (StringLit "hello")])
      it "parses math expr as arg" $
        parse statement "" "f(x + 1)" `shouldBe` Right (Call "f" [PosArg (Add (Var "x") (Number 1))])
    describe "keyword arguments" $ do
      it "parses single kwarg" $
        parse statement "" "f(x=1)" `shouldBe` Right (Call "f" [KwArg "x" (Number 1)])
      it "parses single kwarg with string" $
        parse statement "" "f(x=\"hello\")" `shouldBe` Right (Call "f" [KwArg "x" (StringLit "hello")])
      it "parses multiple kwargs" $
        parse statement "" "f(x=1, y=2)" `shouldBe` Right (Call "f" [KwArg "x" (Number 1), KwArg "y" (Number 2)])
    describe "mixed arguments" $ do
      it "parses positional then keyword" $
        parse statement "" "f(1, x=2)" `shouldBe` Right (Call "f" [PosArg (Number 1), KwArg "x" (Number 2)])
      it "parses multiple positional then keyword" $
        parse statement "" "f(1, 2, x=3)" `shouldBe` Right (Call "f" [PosArg (Number 1), PosArg (Number 2), KwArg "x" (Number 3)])
      it "fails with positional after keyword" $
        parse statement "" "f(x=1, 2)" `shouldSatisfy` isLeft
    describe "invalid input" $ do
      it "fails with no closing paren" $
        parse statement "" "f(1" `shouldSatisfy` isLeft
      it "fails with empty input" $
        parse statement "" "" `shouldSatisfy` isLeft
  describe "stringLit" $ do
    describe "double quoted" $ do
      it "parses empty string" $
        parse stringLit "" "\"\"" `shouldBe` Right (StringLit "")
      it "parses simple string" $
        parse stringLit "" "\"hello\"" `shouldBe` Right (StringLit "hello")
      it "parses escaped quote" $
        parse stringLit "" "\"hello \\\"world\\\"\"" `shouldBe` Right (StringLit "hello \"world\"")
      it "parses escape sequences" $
        parse stringLit "" "\"hello\\nworld\"" `shouldBe` Right (StringLit "hello\nworld")
    describe "single quoted" $ do
      it "parses empty string" $
        parse stringLit "" "''" `shouldBe` Right (StringLit "")
      it "parses simple string" $
        parse stringLit "" "'hello'" `shouldBe` Right (StringLit "hello")
      it "parses escaped quote" $
        parse stringLit "" "'hello \\'world\\''" `shouldBe` Right (StringLit "hello 'world'")
    describe "multiline" $ do
      it "parses empty multiline" $
        parse stringLit "" "\"\"\"\"\"\"" `shouldBe` Right (MultilineStringLit "")
      it "parses multiline with newline" $
        parse stringLit "" "\"\"\"hello\nworld\"\"\"" `shouldBe` Right (MultilineStringLit "hello\nworld")
      it "parses multiline single quote" $
        parse stringLit "" "'''hello\nworld'''" `shouldBe` Right (MultilineStringLit "hello\nworld")
    describe "invalid input" $ do
      it "fails with unclosed double quote" $
        parse stringLit "" "\"hello" `shouldSatisfy` isLeft
      it "fails with unclosed single quote" $
        parse stringLit "" "'hello" `shouldSatisfy` isLeft
      it "fails with unclosed triple quote" $
        parse stringLit "" "\"\"\"hello" `shouldSatisfy` isLeft