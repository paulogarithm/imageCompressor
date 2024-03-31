{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-compressor-rahul.chander
-- File description:
-- ClosestIdxSpec
-}

module ClosestIdxSpec (spec) where

import Test.Hspec
import GetDistance

spec :: Spec
spec = describe "Test Closest idx" $ do
    it "One in list" $ do
        closestIdx [(1,2,3)] (10,10,10)`shouldBe` 0

    it "Short multiple in list" $ do
        closestIdx [(1,2,3),(2,3,4),(6,7,8),(19,0,12)] (2,6,9)`shouldBe` 2

    it "Multiple in list with big truples" $ do
        closestIdx [
            (123,3452,453),
            (876,754,482),
            (262,921,542),
            (1324,6613,4892),
            (237,567,7532)] (100,2356,162) `shouldBe` 0

    it "Empty List" $ do
        closestIdx [] (1,2,3) `shouldBe` 0
