module TypesSpec (spec) where

import Test.Hspec
import Types

spec :: Spec
spec = do
    describe "MeannessLevel" $ do
        it "clamps values to valid range" $ do
            unMeannessLevel (mkMeannessLevel (-5)) `shouldBe` 0
            unMeannessLevel (mkMeannessLevel 0) `shouldBe` 0
            unMeannessLevel (mkMeannessLevel 5) `shouldBe` 5
            unMeannessLevel (mkMeannessLevel 11) `shouldBe` 11
            unMeannessLevel (mkMeannessLevel 20) `shouldBe` 11
        
        it "calculates correct meanness ratios" $ do
            meannessRatio (mkMeannessLevel 1) `shouldBe` 2000
            meannessRatio (mkMeannessLevel 2) `shouldBe` 1000
            meannessRatio (mkMeannessLevel 10) `shouldBe` 200
            meannessRatio (mkMeannessLevel 11) `shouldBe` 1