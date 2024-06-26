{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-compressor-rahul.chander
-- File description:
-- KMeansData
-}

{-- Here we can put the data with the Parser ! --}

module KMeansData.TrupleData (
    Truple,
    addTruple,
    tra,
    trb,
    trc
) where

addTruple :: Truple -> Truple -> Truple
addTruple (a,b,c) (d,e,f) = (a+d,b+e,c+f)

tra :: Truple -> Int
tra (a,_,_) = a

trb :: Truple -> Int
trb (_,b,_) = b

trc :: Truple -> Int
trc (_,_,c) = c

type Truple = (Int,Int,Int)
