module Bowling where

data Frame = Open Int Int
           | Spare Int Int
           | Strike Int Int
           deriving (Eq, Show)

toFrames :: [Int] -> [Frame]
toFrames pins = go 1 pins
    where
        go 10 [x, y]    = [Open x y]
        go 10 [x, y, z]
            | x + y == 10 = [Spare x z]
        go n (x:y:z:ys)
            | x + y == 10 = Spare x z : go (n+1) (z:ys)
            | otherwise   = Open x y  : go (n+1) (z:ys)
