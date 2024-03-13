{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-compressor-rahul.chander
-- File description:
-- Compressor
-}

module Compressor(compressor) where

    import Bootstrap(closest, Truple)

    list :: [Truple]
    list = [(33,18,109),(33,17,109),(35,18,111),(35,21,109),(38,21,112)]

    compressor :: IO ()
    compressor = print (closest list (34,18,112))
