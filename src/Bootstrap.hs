{-
-- EPITECH PROJECT, 2024
-- compressor
-- File description:
-- Bootstrap
-}

module Bootstrap() where

    -- import Data.List (elemIndex)
-- 
    -- position :: Float -> [Float] -> Int
    -- position x xs =
        -- case x `elemIndex` xs of
            -- Just n  -> n
            -- Nothing -> 0
-- 
    -- type Truple = (Float, Float, Float)
-- 
    -- distance :: Truple -> Truple -> Float
    -- distance (r1,g1,b1) (r2,g2,b2) = sqrt ((r * r) + (g * g) + (b * b))
        -- where   r = r1 - r2
                -- g = g1 - g2
                -- b = b1 - b2
-- 
    -- getDistances :: [Truple] -> Truple -> [Float]
    -- getDistances [] _ = []
    -- getDistances (x:xs) what = (distance x what) : (getDistances xs what)
-- 
    -- closest :: [Truple] -> Truple -> Truple
    -- closest xs x = xs !! position (minimum l) l where l = (getDistances xs x)
-- 