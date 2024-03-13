{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-compressor-rahul.chander
-- File description:
-- Main
-}

module Main (main) where

-- import Compressor(compressor)

import System.Environment
import System.Exit
import System.Random
import Data.List (elemIndex)

import KMeansData.GetConfArgs
import KMeansData.TrupleData
import KMeansAlgorithm
import Options.Applicative

-- testData :: [Int]
-- testData = [1, 2, 5678, 990, 90, 1000, 67, 25, 12, 45]

-- position :: Int -> [Int] -> Int
-- position x xs =
--     case x `elemIndex` xs of
--         Just n  -> n
--         Nothing -> 0

-- myFurthest :: [Int] -> Int -> Int
-- myFurthest xs x = xs !! position (maximum l) l
--     where
--         l = map (\val -> (val - x) * ((val - x))) xs

-- getCentroids :: [Int] -> Int -> Int -> [Int]
-- getCentroids _ centroid 0 = centroid : []
-- getCentroids list centroid nbCluster =
--     centroid : (getCentroids newList (myFurthest newList (nbCluster - 1)) (nbCluster - 1))
--     where
--         newList = (filter (\x -> x /= centroid) list)

-- getFirstCentroid :: [Int] -> Int -> IO (Int)
-- getFirstCentroid list nbCluster =
--     (randomRIO (1, (length list) - 1)) >>= (\idx ->
--         return (list !! idx))

-- manageAlgo :: [Int] -> Int -> IO ()
-- manageAlgo list nbCluster = 

b :: [Truple]
b = [(Truple 1.1 2.2 3.3), (Truple 98.1 234.2 45.3)]

handleConf :: Conf -> IO ()
handleConf (Conf nbCluster convergeLimit fileName)
    | nbCluster <= 0 = exitWith (ExitFailure 84)
    | convergeLimit <= 0 = exitWith (ExitFailure 84)
    | otherwise = manageAlgo b (Conf nbCluster convergeLimit fileName)

main :: IO ()
main = do
    args <- getArgs
    case (execParserPure defaultPrefs confCommandParser args) of
        Success conf -> handleConf conf
        Failure _ -> exitWith (ExitFailure 84)
        CompletionInvoked _ -> exitWith (ExitFailure 84)

displayDouble :: Double -> IO()
displayDouble n = print n

displayConf :: Conf -> IO()
displayConf (Conf nbCluster convergeLimit fileName) =
    print nbCluster >>
        displayDouble convergeLimit >>
            putStrLn fileName
