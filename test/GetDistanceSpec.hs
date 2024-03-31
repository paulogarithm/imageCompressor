{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-compressor-rahul.chander
-- File description:
-- GetDistanceSpec
-}

module GetDistanceSpec (spec) where

import Test.Hspec
import GetDistance

spec :: Spec
spec = describe "Test GetDistance" $ do
    it "Empty List" $ do
        getDistances [] (1,2,3) `shouldBe` []

    it "List with 1 element" $ do
        getDistances [(2,3,4)] (1,2,3) `shouldBe` [1.7320508]

    it "List with 5 element" $ do
        getDistances [
            (1,2,3),
            (0,0,0),
            (10,10,10),
            (22,34,45),
            (70,0,23)] (1,2,3) `shouldBe` [
                0,3.7416575,13.928389,56.82429,71.867935]
