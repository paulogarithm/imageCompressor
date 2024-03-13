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


b :: [Truple]
b = [(Truple 1.1 2.2 3.3), (Truple 98.1 234.2 45.3), (Truple 1788.1 21231.2 3456.0), (Truple 5.0 4.0 3.0)]

a = [(Truple 1356.1 2234.2 3131.3), (Truple 1.1 2.2 3.3)]

getDistances :: [Truple] -> Truple -> [Float]
getDistances [] _ = []
getDistances (x:xs) what = (distance x what) : (getDistances xs what)

closest :: [Truple] -> Truple -> Truple
closest xs x = xs !! position (minimum l) l
    where
        l = (getDistances xs x)

assignCluster :: [Truple] -> [Truple] -> [(Truple, Int)]
assignCluster [] _ = []
assignCluster (x:xs) t = (x, (position (closest t x) t)) : (assignCluster xs t)

showMePos :: (Ord a) => (Num a) => [(a,Int)] -> [Int]
showMePos [] = []
showMePos ((_,x):xs) = x : (showMePos xs)

add :: (Num a) => a -> (a, b) -> (a, b)
add n (a, b) = (a + n, b)

-- _clusterNTotal :: (Integral a) => [(a,Int)] -> Int -> a -> (a,a) -- ???
-- _clusterNTotal [] cl nn = (0,nn)
-- _clusterNTotal ((x,c):xs) cl nn
--     | (c == cl) = add x (next (nn + 1))
--     | otherwise = next nn 
--         where
--             next n = _clusterNTotal xs cl n

-- clusterNTotal :: [(Truple, Int)] -> Int -> (Truple, a) -- ???
-- clusterNTotal a b = _clusterNTotal a b 0

-- clusterMeans :: [(Truple, Int)] -> Int -> [Truple]
-- clusterMeans _ 0 = []
-- clusterMeans t len = (clusterMeans t (len - 1)) ++ [div (fst foo) (snd foo)]
--     where foo = (clusterNTotal t (len - 1))

manageAlgo :: [Truple] -> Conf -> IO ()
manageAlgo t conf = return ()
