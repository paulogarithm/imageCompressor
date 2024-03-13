{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-compressor-rahul.chander
-- File description:
-- KMeansAlgorithm
-}

module KMeansAlgorithm (
    manageAlgo
) where

import Data.List (elemIndex)
import KMeansData.TrupleData
import KMeansData.GetConfArgs

distance :: Truple -> Truple -> Float
distance (Truple r1 g1 b1) (Truple r2 g2 b2) =
    sqrt ((r * r) + (g * g) + (b * b))
    where   r = r1 - r2
            g = g1 - g2
            b = b1 - b2

position :: Eq a => a -> [a] -> Int
position x xs = case elemIndex x xs of
        Just n  -> n
        Nothing -> 0

-- a = Truple 1.1 2.2 3.3

getDistances :: [Truple] -> Truple -> [Float]
getDistances [] _ = []
getDistances (x:xs) what = (distance x what) : (getDistances xs what)

-- closest :: (Ord a) => (Num a) => [a] -> a -> a
-- closest xs x = xs !! position (minimum l) l where l = (getDistances xs x)

manageAlgo :: [Truple] -> Conf -> IO ()
manageAlgo t conf = return ()
