{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-compressor-rahul.chander
-- File description:
-- SpecDistance
-}

module DistanceSpec (spec) where

import Test.Hspec
import GetDistance

spec :: Spec
spec = describe "Test distance" $ do
    it "Zero Distance" $ do
        (distance (1,2,3) (1,2,3)) `shouldBe` 0.0

    it "Short distance of 1" $ do
        (distance (1,2,3) (2,2,3)) `shouldBe` 1.0

    it "Long distance of 5" $ do
        (distance (0,0,0) (0,0,5)) `shouldBe` 5.0

    it "Long distance of 13" $ do
        (distance (0,0,0) (4,8,10)) `shouldBe` 13.416408

    it "Long distance" $ do
        (distance (0,0,0) (56,123,6789)) `shouldBe` 6790.3447
