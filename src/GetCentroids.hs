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
import Parsing(Info)

getFirstCentroid :: [Info] -> IO Truple
getFirstCentroid list = (randomRIO (1, (length list) - 1))
    >>= return . (\idx -> (snd (list !! idx)))

getCentroids :: Truple -> [Info] -> Int -> [Truple]
getCentroids _ _ 0 = []
getCentroids centroid list nbCluster =
    centroid : (getCentroids (furthestInfo newList centroid) newList newCluster)
    where
        newList = (filter (\x -> (snd x) /= centroid) list)
        newCluster = (nbCluster - 1)

initCentroids :: [Info] -> Int -> IO [Truple]
initCentroids l n = (getFirstCentroid l) >>= return . (\c ->
    getCentroids c l (n))
