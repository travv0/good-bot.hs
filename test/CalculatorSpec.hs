{-# LANGUAGE OverloadedStrings #-}

module CalculatorSpec (spec) where

import Test.Hspec
import Calculator
import Text.Megaparsec (parse)

spec :: Spec
spec = do
    describe "calcExpr parser" $ do
        it "parses simple numbers" $ do
            parse calcExpr "" "42" `shouldParse` CalcVal 42
            parse calcExpr "" "-42" `shouldParse` CalcVal (-42)
            parse calcExpr "" "3.14" `shouldParse` CalcVal 3.14
        
        it "parses basic arithmetic" $ do
            parse calcExpr "" "2 + 3" `shouldParse` 
                CalcBinary (CalcVal 2) Plus (CalcVal 3)
            parse calcExpr "" "10 - 5" `shouldParse` 
                CalcBinary (CalcVal 10) Minus (CalcVal 5)
            parse calcExpr "" "4 * 5" `shouldParse` 
                CalcBinary (CalcVal 4) Times (CalcVal 5)
            parse calcExpr "" "20 / 4" `shouldParse` 
                CalcBinary (CalcVal 20) Divide (CalcVal 4)
        
        it "parses functions" $ do
            parse calcExpr "" "sqrt 16" `shouldParse` 
                CalcPrefix Sqrt (CalcVal 16)
            parse calcExpr "" "sin 0" `shouldParse` 
                CalcPrefix Sin (CalcVal 0)
            parse calcExpr "" "abs -5" `shouldParse` 
                CalcPrefix Abs (CalcVal (-5))
        
        it "parses parentheses" $ do
            parse calcExpr "" "(2 + 3) * 4" `shouldParse`
                CalcBinary (CalcBinary (CalcVal 2) Plus (CalcVal 3)) Times (CalcVal 4)
    
    describe "eval" $ do
        it "evaluates simple arithmetic" $ do
            result <- eval (CalcBinary (CalcVal 2) Plus (CalcVal 3))
            result `shouldBe` Right 5
            
            result <- eval (CalcBinary (CalcVal 10) Minus (CalcVal 3))
            result `shouldBe` Right 7
            
            result <- eval (CalcBinary (CalcVal 4) Times (CalcVal 5))
            result `shouldBe` Right 20
        
        it "handles division by zero" $ do
            result <- eval (CalcBinary (CalcVal 10) Divide (CalcVal 0))
            result `shouldBe` Left DivisionByZero
        
        it "evaluates functions" $ do
            result <- eval (CalcPrefix Sqrt (CalcVal 16))
            result `shouldBe` Right 4
            
            result <- eval (CalcPrefix Abs (CalcVal (-5)))
            result `shouldBe` Right 5

shouldParse :: (Show a, Eq a) => Either e a -> a -> Expectation
shouldParse (Right actual) expected = actual `shouldBe` expected
shouldParse (Left _) _ = expectationFailure "Parse failed"