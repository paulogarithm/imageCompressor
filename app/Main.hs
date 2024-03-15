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

import KMeansData.GetConfArgs
import KMeansData.TrupleData
import KMeansAlgorithm
import Options.Applicative
import GetCentroids

b :: [Truple]
b = [
    (Truple 1.1 2.2 3.3),
    (Truple 9.1 2.2 45.3),
    (Truple 18.1 29.2 22.3),
    (Truple 38.1 0.2 34.3),
    (Truple 20.1 15.2 35.3),
    (Truple 12.1 20.2 15.0),
    (Truple 5.0 4.0 3.0)]

resultAlgo :: [Truple] -> IO()
resultAlgo c = case manageAlgo b c 0 of
    Just _ -> exitSuccess
    Nothing -> exitWith (ExitFailure 84)

handleConf :: Conf -> IO ()
handleConf (Conf confClusters confConvlimit _)
    | confClusters <= 0 = exitWith (ExitFailure 84)
    | confConvlimit <= 0 = exitWith (ExitFailure 84)
    | otherwise = (initCentroids b confClusters) >>= resultAlgo

main :: IO ()
main = do
    args <- getArgs
    case (execParserPure defaultPrefs confCommandParser args) of
        Success conf -> handleConf conf
        Failure _ -> exitWith (ExitFailure 84)
        CompletionInvoked _ -> exitWith (ExitFailure 84)

