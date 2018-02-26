module Sound.Pulse.PulseRhythm where

import Data.List


-- Util
--
sliceWithBeat :: Num a => Int -> [a] -> [[a]]
sliceWithBeat n = unfoldr phi
    where
        phi [] = Nothing
        phi xs = if length xs == n
                    then Just $ splitAt n xs
                    else do
                        let (xs', xs_) = splitAt n xs
                        Just (xs' ++ replicate (n - length xs) 0, xs_)
--
--

-- These functions are used during the performance
newPlayerScore :: Int -> [Int] -> [[Int]]
newPlayerScore pbeat newscore =
    sliceWithBeat pbeat newscore
