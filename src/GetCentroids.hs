{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-compressor-rahul.chander
-- File description:
-- GetCentroids
-}

module GetCentroids (
    initCentroids
) where

import KMeansData.TrupleData
import GetDistance

import System.Random

getFirstCentroid :: [Truple] -> IO Truple
getFirstCentroid list = (randomRIO (1, (length list) - 1))
    >>= return . (\idx -> (list !! idx))

getCentroids :: Truple -> [Truple] -> Int -> [Truple]
getCentroids _ _ 0 = []
getCentroids centroid list nbCluster =
    centroid : (getCentroids (furthest newList centroid) newList newCluster)
    where
        newList = (filter (\x -> x /= centroid) list)
        newCluster = (nbCluster - 1)

initCentroids :: [Truple] -> Int -> IO [Truple]
initCentroids l n = (getFirstCentroid l) >>= return . (\c ->
    getCentroids c l (n))
