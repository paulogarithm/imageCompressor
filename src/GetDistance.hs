{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-compressor-rahul.chander
-- File description:
-- GetDistance
-}

module GetDistance (
    getDistances,
    distance,
    position,
    closest,
    furthest
) where

import Data.List (elemIndex)
import KMeansData.TrupleData

a :: [Truple]
a = [(Truple 1356.1 2234.2 3131.3), (Truple 1.1 2.2 3.3)]

getDistances :: [Truple] -> Truple -> [Float]
getDistances [] _ = []
getDistances (x:xs) what = (distance x what) : (getDistances xs what)

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

closest :: [Truple] -> Truple -> Truple
closest xs x = xs !! position (minimum l) l
    where
        l = (getDistances xs x)

furthest :: [Truple] -> Truple -> Truple
furthest xs x = xs !! position (maximum l) l
    where
        l = (getDistances xs x)

