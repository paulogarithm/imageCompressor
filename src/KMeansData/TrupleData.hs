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
    trupleTotal,
    trupleInt,
    tra,
    trb,
    trc
) where

addTruple :: Truple -> Truple -> Truple
addTruple (a,b,c) (d,e,f) = (a+d,b+e,c+f)

trupleTotal :: [Truple] -> Truple
trupleTotal [] = (0,0,0)
trupleTotal (tr:xs) = addTruple tr (trupleTotal xs)

trupleInt :: Truple -> (Int,Int,Int)
trupleInt (a,b,c) = (round a,round b,round c)

tra :: Truple -> Float
tra (a,_,_) = a

trb :: Truple -> Float
trb (_,b,_) = b

trc :: Truple -> Float
trc (_,_,c) = c

-- data Truple = Truple Float Float Float deriving (Show, Ord, Eq)
type Truple = (Float,Float,Float)
