{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-compressor-rahul.chander
-- File description:
-- KMeansAlgorithm
-}

module KMeansAlgorithm (executeKMeans) where

import GetDistance
import KMeansData.TrupleData
import DisplayOutput

type Group = [(Truple, [Truple])]

trupleAverage :: [Truple] -> Truple -> Truple
trupleAverage [] what = what
trupleAverage t _ =
    ((tra total) `div` len, (trb total) `div` len, (trc total) `div` len)
    where
        total = trupleTotal t
        len = length t

checkConvergenceLimit :: [Truple] -> [Truple] -> Float -> Bool
checkConvergenceLimit [] _ _ = True
checkConvergenceLimit _ [] _ = True
checkConvergenceLimit (newCentroid:list1) (centroid:list2) limit =
    (convergeDistance <= limit) && (checkConvergenceLimit list1 list2 limit)
    where
        convergeDistance = (distance newCentroid centroid)

initGroup :: [Truple] -> Group
initGroup [] = []
initGroup (x:xs) = (x,[]):(initGroup xs)

getNewCentroids :: Group -> [Truple]
getNewCentroids [] = []
getNewCentroids ((w, t):xs) = (trupleAverage t w):(getNewCentroids xs)

pushInGroup :: Group -> Int -> Truple -> Group
pushInGroup ((cen, truples):list) 0 t = ((cen, (t:truples)):list)
pushInGroup (centGrp:list) idx t =
    centGrp:(pushInGroup list (idx - 1) t)
pushInGroup _ _ _ = []

manageAlgo :: Group -> [Truple] -> [Truple] -> Group
manageAlgo grp [] _ = grp
manageAlgo grp (t:list) cen =
    manageAlgo (pushInGroup grp (closestIdx cen t) t) list cen

executeKMeans :: [Truple] -> [Truple] -> Float -> IO()
executeKMeans cen values l = return (result) >>= (\group ->
        newGeneration group)
    where
        result = manageAlgo (initGroup cen) values cen
        newGeneration group
            | (checkConvergenceLimit cen (newCen group) l) =
                (displayOutput group)
            | cen == (newCen group) = (displayOutput group)
            | otherwise = executeKMeans (newCen group) values l
        newCen group = getNewCentroids group
