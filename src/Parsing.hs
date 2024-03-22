{-
-- EPITECH PROJECT, 2024
-- repo
-- File description:
-- Parsing
-}

module Parsing(parseFile) where

    import System.IO (readFile)
    
    parseFile :: String -> IO String
    parseFile filename = lines <$> readFile filename
