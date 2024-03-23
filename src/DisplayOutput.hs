{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-compressor-rahul.chander
-- File description:
-- DisplayOutput
-}

module DisplayOutput (
    displayOutput
) where

import System.Exit
import KMeansData.TrupleData

_displayCentroids :: Truple -> IO ()
_displayCentroids truple =
    (putStrLn "--") >>
        (print truple)

_displayPoints :: [Truple] -> IO ()
_displayPoints [] = return ()
_displayPoints (t:list) =
        (print t) >>
            (_displayPoints list)

displayOutput :: [(Truple, [Truple])] -> IO ()
displayOutput [] = exitSuccess
displayOutput (x:xs) =
    _displayCentroids (fst x) >>
        (putStrLn "-") >>
            _displayPoints (snd x) >>
                displayOutput xs
