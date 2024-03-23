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
import Parsing(getImageParsing,Info)

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

exitError :: IO()
exitError = exitWith (ExitFailure 84)

startKMeans :: Conf -> [Info] -> IO()
startKMeans (Conf cClusters cConvLimit _) infos =
    (initCentroids infos cClusters) >>= (\cen ->
        executeKMeans cen infos cConvLimit)

handleConf :: Conf -> IO ()
handleConf (Conf cClusters cConvLimit cFile)
    | cClusters <= 0 || cClusters >= length b = exitError
    | cConvLimit <= 0 = exitError
    | otherwise = getImageParsing cFile >>= (\pars -> case pars of
        Just res -> startKMeans (Conf cClusters cConvLimit cFile) res
        Nothing -> exitError)

main :: IO ()
main = do
    args <- getArgs
    case (execParserPure defaultPrefs confCommandParser args) of
        Success conf -> handleConf conf
        Failure _ -> exitError
        CompletionInvoked _ -> exitError

