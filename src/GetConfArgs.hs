{-
-- EPITECH PROJECT, 2024
-- B-FUN-400-PAR-4-1-compressor-rahul.chander
-- File description:
-- GetConfArgs
-}

-- module GetConfArgs (
--     defaultConf,
--     getOpts,
--     Conf(..)
-- ) where

data Conf = Conf {  nbCluster :: Maybe Int,
                    convergeLimit :: Maybe Double,
                    fileName :: Maybe String
                    } deriving (Show)

isNbr :: [Char] -> Bool -> Bool
isNbr [] _ = True
isNbr ('-':list) True = isNbr list False
isNbr ('-':_) False = False
isNbr (x:list) _ | (x >= '0' && x <= '9') = True && isNbr list False
    | otherwise = False

readInt :: [Char] -> Maybe Int
readInt [] = Nothing
readInt list
    | (isNbr list True) == True = Just (read (list) :: Int)
    | otherwise = Nothing

defaultConf :: Conf
defaultConf = Conf {
                    nbCluster=Nothing,
                    convergeLimit=Nothing,
                    fileName=Nothing
                    }

checkValue :: Maybe Int -> Bool
checkValue Nothing = False
checkValue (Just value)
    | value < 0 = False
    | otherwise = True

getOpts :: Conf -> [String] -> Maybe Conf
getOpts conf [] = (Just conf)
getOpts (Conf Nothing convLimit file) ("-n":value:list)
    | (checkValue $ readInt value) /= False =
        getOpts (Conf (readInt value) convLimit file) list
    | otherwise = Nothing
getOpts (Conf nbCluster Nothing file) ("-l":value:list)
    | (checkValue $ readInt value) /= False =
        getOpts (Conf nbCluster (Just (read (value) :: Double)) file) list
    | otherwise = Nothing
getOpts (Conf nbCluster convLimit Nothing) ("-f":value:list) =
        getOpts (Conf nbCluster convLimit (Just value)) list
getOpts _ _ = Nothing
