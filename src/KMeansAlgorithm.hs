{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-compressor-rahul.chander
-- File description:
-- KMeansAlgorithm
-}

module KMeansAlgorithm (executeKMeans,Group) where

import GetDistance
import KMeansData.TrupleData
import DisplayOutput
import Parsing (Info)

type Group = [(Truple, [Info])]

trupleTotal :: [Info] -> Truple
trupleTotal [] = (0, 0, 0)
trupleTotal ((_,t):xs) = addTruple t (trupleTotal xs)

trupleAverage :: [Info] -> Truple -> Truple
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
getNewCentroids ((w,i):xs) = (trupleAverage i w):(getNewCentroids xs)

pushInGroup :: Group -> Int -> Info -> Group
pushInGroup ((cen,infos):list) 0 info = ((cen, (info:infos)):list)
pushInGroup (centGrp:list) idx info =
    centGrp:(pushInGroup list (idx - 1) info)
pushInGroup _ _ _ = []

manageAlgo :: Group -> [Info] -> [Truple] -> Group
manageAlgo grp [] _ = grp
manageAlgo grp ((p,t):list) cen =
    manageAlgo (pushInGroup grp (closestIdx cen t) (p,t)) list cen

executeKMeans :: [Truple] -> [Info] -> Float -> IO()
executeKMeans cen values l = return (result) >>=
    (\group -> newGeneration group)
    where   result = manageAlgo (initGroup cen) values cen
            newGeneration group
                | (checkConvergenceLimit cen (newCen group) l) =
                    (displayOutput group)
                | cen == (newCen group) = (displayOutput group)
                | otherwise = executeKMeans (newCen group) values l
            newCen group = getNewCentroids group
