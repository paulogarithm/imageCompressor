{-
-- EPITECH PROJECT, 2024
-- repo
-- File description:
-- Parsing
-}

module Parsing(getImageParsing) where

    import System.IO()
    import Control.Exception(try)
    import KMeansData.TrupleData(Truple)
    import Data.Maybe(listToMaybe)

    type Information = ((Int,Int),Truple)

    parseFile :: String -> IO (Maybe [String])
    parseFile filename =
        (try (readFile filename) :: IO(Either IOError String)) >>=
            return . (\what -> case what of
                Left _ -> Nothing
                Right contents -> Just (lines contents))

    myReadTuple :: String -> Maybe (Int,Int)
    myReadTuple ('(':s) = case listToMaybe $ reads s of
            Just (a,',':xs) -> case listToMaybe $ reads xs of
                Just (b,')':_) -> Just (a,b)
                _ -> Nothing
            _ -> Nothing
    myReadTuple _ = Nothing

    myReadTruple :: String -> Maybe Truple
    myReadTruple ('(':as) = case listToMaybe $ reads as of
            Just (a,',':bs) -> case listToMaybe $ reads bs of
                Just (b,',':cs) -> case listToMaybe $ reads cs of
                    Just (c,')':_) -> Just (a,b,c)
                    _ -> Nothing
                _ -> Nothing
            _ -> Nothing
    myReadTruple _ = Nothing

    getInfo :: String -> Maybe Information
    getInfo [] = Nothing
    getInfo s   | (length w) == 2 = case myReadTuple (w!!0) of
                    Just a -> case myReadTruple (w!!1) of
                        Just b -> Just (a,b)
                        Nothing -> Nothing
                    Nothing -> Nothing
                | otherwise = Nothing
                where w = words s
    
    getInformations :: [String] -> Maybe [Information]
    getInformations [] = Just []
    getInformations (x:xs) = case getInfo x of
        Just info -> case getInformations xs of
            Just rest -> Just (info:rest)
            Nothing -> Nothing
        Nothing -> Nothing

    getImageParsing :: String -> IO (Maybe [Information])
    getImageParsing filename = parseFile filename >>=
        return . (\e -> case e of
            Just x -> getInformations x
            Nothing -> Nothing
        )
