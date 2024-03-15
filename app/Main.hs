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

b :: [Truple]
b = [
    (Truple 1.1 2.2 3.3),
    (Truple 9.1 2.2 45.3),
    (Truple 18.1 29.2 22.3),
    (Truple 38.1 0.2 34.3),
    (Truple 20.1 15.2 35.3),
    (Truple 12.1 20.2 15.0),
    (Truple 5.0 4.0 3.0)]

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
