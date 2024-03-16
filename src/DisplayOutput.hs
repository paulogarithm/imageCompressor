{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-compressor-rahul.chander
-- File description:
-- DisplayOutput
-}

module DisplayOutput (
    displayOutput
) where

import KMeansData.TrupleData

-- c :: [(Truple, [Truple])]
-- c = [
    -- ((Truple 1 2 3), [(Truple 2 3 4), (Truple 5 678 13), (Truple 88 8 3), (Truple 0 7 1)]),
    -- ((Truple 4 5 3), [(Truple 1 23 44), (Truple 45 1678 213), (Truple 883 812 33), (Truple 110 227 133)])]

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
displayOutput [] = return ()
displayOutput (x:xs) =
    _displayCentroids (fst x) >>
        (putStrLn "-") >>
            _displayPoints (snd x) >>
                displayOutput xs
