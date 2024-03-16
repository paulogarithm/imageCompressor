{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-compressor-rahul.chander
-- File description:
-- KMeansAlgorithm
-}

module KMeansAlgorithm (
    manageAlgo,
    getConvergence,
    getGroupAverage,
    Group,
) where

import KMeansData.TrupleData
import KMeansData.GetConfArgs
import GetDistance
import GetCentroids

type Group = [(Truple,[Truple])]

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

trupleAverage :: [Truple] -> Truple
trupleAverage t = ((tra total)/len, (tra total)/len, (trc total)/len)
    where   total = trupleTotal t
            len = fromIntegral $ length t

checkConvergenceLimit :: [Truple] -> [Truple] -> Float -> Bool
checkConvergenceLimit [] _ _ = True
checkConvergenceLimit _ [] _ = True
checkConvergenceLimit (newCentroid:list1) (centroid:list2) limit =
    (convergeDistance <= limit) && (checkConvergenceLimit list1 list2 limit)
    where
        convergeDistance = (distance newCentroid centroid)

getOverallDistance :: [Truple] -> [Truple] -> Float
getOverallDistance [] _ = 0
getOverallDistance _ [] = 0
getOverallDistance (a:as) (b:bs) = (distance a b) + (getOverallDistance as bs)

getConvergence :: Float -> Float -> Float
getConvergence a b = abs (a - b)

initGrouping :: [Truple] -> Group
initGrouping [] = []
initGrouping (x:xs) = (x,[]):(initGrouping xs)

pushInGroup :: Group -> Truple -> Truple -> Group
pushInGroup [] _ _ = []
pushInGroup ((pk,t):xs) k v | (k == pk) = ((pk,v:t):xs)
                            | otherwise = ((pk,t):(pushInGroup xs k v))

setGroup :: [(Truple,Int)] -> [Truple] -> Group
setGroup [] cen = initGrouping cen
setGroup ((what,index):xs) cen = pushInGroup object (cen!!index) what
    where object = setGroup xs cen

getGroupAverage :: Group -> [Truple]
getGroupAverage [] = []
getGroupAverage ((_,t):xs) = (trupleAverage t):(getGroupAverage xs)

-- Args: (valeurs) -> (centroids) -> (previousConvergence) => [Truple,[Truple]]
manageAlgo :: [Truple] -> [Truple] -> Float -> Maybe Group
manageAlgo list cen _ = Just (setGroup (assignCluster list cen) cen)
