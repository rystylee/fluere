module Sound.Fluere.Rhythm where

import Data.List


-- Util
--
fillRestWith0 :: Num a => Int -> [a] -> Maybe ([a], [a])
fillRestWith0 n xs =
    if length xs == n
        then Just $ splitAt n xs
        else do
            let (xs', xs_) = splitAt n xs
            Just (xs' ++ replicate (n - length xs) 0, xs_)

sliceWithBeat :: Num a => Int -> [a] -> [[a]]
sliceWithBeat n =
    let phi [] = Nothing
        phi xs = fillRestWith0 n xs
    in unfoldr phi
--
--

-- These functions are used during the performance
newPlayerScore :: Int -> [Int] -> [[Int]]
newPlayerScore pbeat newscore =
    sliceWithBeat pbeat newscore
