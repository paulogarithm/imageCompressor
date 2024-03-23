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
    (98,99,233),
    (88,77,211),
    (45,12,167),
    (33,16,94),
    (78,8,9),
    (20,27,67),
    (1,56,37),
    (66,20,26),
    (15,89,40)]

handleConf :: Conf -> IO ()
handleConf (Conf cClusters cConvLimit _)
    | cClusters <= 0 || cClusters >= length b = exitWith (ExitFailure 84)
    | cConvLimit <= 0 = exitWith (ExitFailure 84)
    | otherwise = (initCentroids b cClusters) >>=
        (\v -> executeKMeans v b cConvLimit)

main :: IO ()
main = do
    args <- getArgs
    case (execParserPure defaultPrefs confCommandParser args) of
        Success conf -> handleConf conf
        Failure _ -> exitWith (ExitFailure 84)
        CompletionInvoked _ -> exitWith (ExitFailure 84)

