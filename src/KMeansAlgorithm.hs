{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-compressor-rahul.chander
-- File description:
-- KMeansAlgorithm
-}

module KMeansAlgorithm (
    manageAlgo
) where

import KMeansData.TrupleData
import KMeansData.GetConfArgs
import GetDistance

import GetCentroids

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

handleKMeans :: [Truple] -> [Truple] -> IO ()
handleKMeans list centroids =
    print (assignCluster list centroids)

manageAlgo :: [Truple] -> Conf -> IO ()
manageAlgo list (Conf nbCluster _ _) =
    (initCentroids list nbCluster) >>= (\centroids ->
        handleKMeans list centroids)
