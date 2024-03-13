module OneDimension where

    import Data.List (elemIndex)
    
    position :: Eq a => a -> [a] -> Int
    position x xs = case elemIndex x xs of
            Just n  -> n
            Nothing -> 0

    distance :: Num a => a -> a -> a
    distance a b = abs (a - b)

    getDistances :: Num a => [a] -> a -> [a]
    getDistances [] _ = []
    getDistances (x:xs) what = (distance x what) : (getDistances xs what)

    closest :: (Ord a) => (Num a) => [a] -> a -> a
    closest xs x = xs !! position (minimum l) l where l = (getDistances xs x)

    assignCluster :: (Ord a) => (Num a) => [a] -> [a] -> [(a,Int)]
    assignCluster [] _ = []
    assignCluster (x:xs) t = (x,(position (closest t x) t)):(assignCluster xs t)

    showMePos :: (Ord a) => (Num a) => [(a,Int)] -> [Int]
    showMePos [] = []
    showMePos ((_,x):xs) = x:(showMePos xs)

    add :: (Num a) => a -> (a,b) -> (a,b)
    add n (a,b) = (a + n,b)

    _clusterNTotal :: (Integral a) => [(a,Int)] -> Int -> a -> (a,a)
    _clusterNTotal [] cl nn = (0,nn)
    _clusterNTotal ((x,c):xs) cl nn | (c == cl) = add x (next (nn + 1))
                                    | otherwise = next nn 
                                    where next n = _clusterNTotal xs cl n

    clusterNTotal :: (Integral a) => [(a,Int)] -> Int -> (a,a)
    clusterNTotal a b = _clusterNTotal a b 0

    clusterMeans :: (Integral a) => [(a,Int)] -> Int -> [a]
    clusterMeans _ 0 = []
    clusterMeans t len = (clusterMeans t (len - 1)) ++ [div (fst foo) (snd foo)]
        where foo = (clusterNTotal t (len - 1))

    -- kmeans1D :: (Ord a) => (Num a) => [a] -> a -> [(a,Int)]
    -- kmeans1D _ _ = []
