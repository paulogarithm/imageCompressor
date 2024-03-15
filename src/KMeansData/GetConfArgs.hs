{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-compressor-rahul.chander
-- File description:
-- GetConfArgs
-}

module KMeansData.GetConfArgs (
    confCommandParser,
    Conf(..)
) where

import Options.Applicative

data Conf = Conf {  nbCluster :: Int,
                    convergeLimit :: Double,
                    fileName :: String
                    } deriving (Show)

clusterParser :: Parser Int
clusterParser = option auto (short 'n')

convergeParser :: Parser Double
convergeParser = option auto (short 'l')

fileNameParser :: Parser String
fileNameParser = strOption (short 'f')

confCommandParser :: ParserInfo Conf
confCommandParser = info
    (Conf <$> clusterParser <*> convergeParser <*> fileNameParser)
    fullDesc
