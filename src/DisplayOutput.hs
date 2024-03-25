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
import Parsing(Info)

_displayCentroids :: Truple -> IO ()
_displayCentroids truple =
    (putStrLn "--") >>
        (print truple)

_displayPoints :: [Info] -> IO ()
_displayPoints [] = return ()
_displayPoints ((p,t):list) = return ()
        >> putStr (show p)
        >> putStr " "
        >> putStrLn (show t)
        >> (_displayPoints list)

displayOutput :: [(Truple,[Info])] -> IO ()
displayOutput [] = exitSuccess
displayOutput (x:xs) =
    _displayCentroids (fst x) >>
        (putStrLn "-") >>
            _displayPoints (snd x) >>
                displayOutput xs
