module Sound.Fluere.Stochastic.Util where

import Data.List as L
import Data.List.Split as S
import Data.Map as M


---------------------------------------------------------------------
-- Indispensability
---------------------------------------------------------------------

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

primeFactors' :: Int -> [Int]
primeFactors' 1 = []
primeFactors' x = v:primeFactors' (x `div` v)
    where v = (factors x) !! 1

primeFactors :: (Int, Int) -> Int -> [Int]
primeFactors (tsNum, tsDenom) numPulse = (L.sort (primeFactors' tsNum)) ++ (L.sort (primeFactors' (numPulse `div` tsNum)))

subdivisionLevels :: [Int] -> [Int]
subdivisionLevels pfs = take (length pfs) [1..]

calcNumPulse :: (Int, Int) -> Int -> Int
calcNumPulse (tsNum, tsDenom) subdivisionStep = (subdivisionStep `div` tsDenom) * tsNum

multiply :: Int -> [Int] -> [Int] -> [Int]
multiply sl pfs xs = Prelude.map (* v) xs
    where pfs' = [1] ++ init pfs
          pfs'' = take sl pfs'
          v = L.foldl' (*) 1 pfs''

slideRight :: [a] -> [a]
slideRight xs = (last xs):[] ++ init xs

slideLeft :: [Int] -> [Int]
slideLeft xs = tail xs ++ (head xs):[]

sumUp :: [[Int]] -> [Int]
sumUp xss = Prelude.map sum xss'
    where xss' = L.transpose xss

-- ToDO
power x n
    | n == 0    = 1
    | even n    = power (x*x) (n `div` 2)
    | otherwise = x * power x (n - 1)

---------------------------------------------------------------------
-- Weight
---------------------------------------------------------------------

-- weightValues [(1, 0.6), (0.6, 0.36), (0.36, 0.216)] [3, 3, 6]
-- => [ [1.0,0.8666666666666667,0.7333333333333333]
--     ,[0.6,0.52,0.44]
--     ,[0.36,0.33599999999999997,0.312,0.288,0.264,0.24]
--    ]
weightValues :: [(Double, Double)] -> [Int] -> [[Double]]
weightValues ranges ls = Prelude.map (\x -> weightValues' x) $ zip ranges ls

weightValues' :: ((Double, Double), Int) -> [Double]
weightValues' ((max', min'), l) = [max'] ++ downSort deltas
    where l' = fromIntegral l
          diff = max' - min'
          delta = diff / l'
          deltas = Prelude.map (+ min') [x * delta | x <- [1..(l' - 1)]]

listToMap :: [[Int]] -> [[Double]] -> M.Map Int Double
listToMap xs ys = M.fromList ls
    where ls = concat $ zipWith zip xs ys

findElems :: M.Map Int Int -> Int -> M.Map Int Int
findElems m n = M.filter (== n) m

downSort :: Ord a => [a] -> [a]
downSort xs = L.sortBy (\x y -> compare y x ) xs
