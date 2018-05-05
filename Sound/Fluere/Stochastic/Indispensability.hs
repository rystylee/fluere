module Sound.Fluere.Stochastic.Indispensability where

import Data.List as L
import Data.List.Split as S
import Data.Map as M

import Sound.Fluere.Stochastic.Util ( calcNumPulse
                                    , slideRight
                                    , slideLeft
                                    , sumUp
                                    , primeFactors
                                    , subdivisionLevels
                                    , multiply
                                    )


---------------------------------------------------------------------
-- References
-- Barlow, C. "Two essays on theory". Computer Music Journal, 11, 44-60, 1987
-- George Sioros, Carlos Guedes. "A Formal Approach for High-Level Automatic Rhythm Generation"
-- Github: mcartwright/pyrhythmicator
---------------------------------------------------------------------

---------------------------------------------------------------------
-- Indispensability Value
---------------------------------------------------------------------

-- indispensabilities (3, 4) 16 => [11,0,6,3,9,1,7,4,10,2,8,5]
indispensabilities :: (Int, Int) -> Int -> [Int]
indispensabilities (tsNum, tsDenom) subdivisionStep = indispensabilities' numPulse sls pfs
    where numPulse = calcNumPulse (tsNum, tsDenom) subdivisionStep
          pfs = primeFactors (tsNum, tsDenom) numPulse
          sls = subdivisionLevels pfs

indispensabilities' :: Int -> [Int] -> [Int] -> [Int]
indispensabilities' numPulse sls pfs = slideRight ids'
    where ids = Prelude.map (\sl -> indispensability numPulse sl pfs) sls
          ids' = sumUp ids

indispensability :: Int -> Int -> [Int] -> [Int]
indispensability numPulse sl pfs = b'''
    where b = basicIndispensability $ pfs !! (sl - 1)
          b' = slideLeft b
          b'' = fitWidth numPulse sl pfs $ extendList sl pfs b'
          b''' = multiply sl pfs b''

fitWidth :: Int -> Int -> [Int] -> [Int] -> [Int]
fitWidth numPulse sl pfs xs = if (length xs) /= numPulse then xs' else xs
    where n = numPulse `div` (length xs)
          xs' = concat $ Prelude.map (\x -> replicate n x) xs

extendList :: Int -> [Int] -> [Int] -> [Int]
extendList 0 _ _ = []
extendList 1 pfs xs = xs
extendList sl pfs xs = concat $ replicate n xs
    where n = L.foldl' (*) 1 (take (sl - 1) pfs)

basicIndispensability :: Int -> [Int]
basicIndispensability pf = xs
    where m = basicIndispensabilityMap
          Just xs = M.lookup pf m

basicIndispensabilityMap :: Map Int [Int]
basicIndispensabilityMap = tableMap
    where tableMap = M.fromList table
          table = [ (2, [1, 0])
                  , (3, [2, 0, 1])
                  , (5, [4, 0, 1, 3, 2])
                  , (7, [6, 0, 4, 1, 5, 2, 3])
                  , (11, [10, 0, 6, 3, 9, 1, 7, 2, 8, 4, 5])
                  , (13, [12, 0, 7, 3, 10, 1, 8, 4, 11, 2, 9, 5, 6])
                  , (17, [16, 0, 9, 4, 13, 2, 11, 6, 15, 1, 10, 5, 14, 3, 12, 7, 8])
                  , (19, [18, 0, 10, 3, 13, 6, 16, 1, 11, 4, 14, 7, 17, 2, 12, 5, 15, 8, 9])
                  , (23, [22, 0, 12, 6, 18, 3, 15, 9, 21, 1, 13, 7, 19, 2, 14, 8, 20, 4, 16, 5, 17, 10, 11])
                  , (29, [28, 0, 15, 7, 22, 4, 19, 11, 26, 1, 16, 8, 23, 5, 20, 12, 27, 2, 17, 9, 24, 3, 18, 10, 25, 6, 21, 13, 14])
                  , (31, [30, 0, 16, 5, 21, 10, 26, 3, 19, 8, 24, 13, 29, 1, 17, 6, 22, 11, 27, 2, 18, 7, 23, 12, 28, 4, 20, 9, 25, 14, 15])
                  ]
