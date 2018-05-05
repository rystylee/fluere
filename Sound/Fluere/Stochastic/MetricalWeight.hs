module Sound.Fluere.Stochastic.MetricalWeight where

import Data.Map as M
import Data.List as L
import Data.List.Split as S

import Sound.Fluere.Stochastic.Util ( calcNumPulse
                                    , primeFactors
                                    , subdivisionLevels
                                    , listToMap
                                    , weightValues
                                    , findElems
                                    , downSort
                                    , sumUp
                                    )
import Sound.Fluere.Stochastic.Indispensability (indispensabilities)


---------------------------------------------------------------------
-- References
-- Barlow, C. "Two essays on theory". Computer Music Journal, 11, 44-60, 1987
-- George Sioros, Carlos Guedes. "A Formal Approach for High-Level Automatic Rhythm Generation"
-- Github: mcartwright/pyrhythmicator
---------------------------------------------------------------------

weightList :: (Int, Int) -> Int -> Double -> [Double]
weightList (tsNum, tsDenom) subdivisionStep r = weightList' numPulse ids sls pfs r
    where ids = indispensabilities (tsNum, tsDenom) subdivisionStep
          numPulse = calcNumPulse (tsNum, tsDenom) subdivisionStep
          pfs = primeFactors (tsNum, tsDenom) numPulse
          sls = subdivisionLevels pfs


weightList' :: Int -> [Int] -> [Int] -> [Int] -> Double -> [Double]
weightList' numPulse ids sls pfs r = Prelude.map (\id -> calcWeight wmap id) ids
    where wmap = weightMap numPulse ids sls pfs r

calcWeight :: M.Map Int Double -> Int -> Double
calcWeight wmap id = w
    where Just w = M.lookup id wmap

-- dmw = [[0,4,8],[2,6,10],[1,3,5,7,9,11]]
-- wvs = [[1.0,0.8666666666666667,0.7333333333333333],[0.6,0.52,0.44],[0.36,0.33599999999999997,0.312,0.288,0.264,0.24]]
weightMap :: Int -> [Int] -> [Int] -> [Int] -> Double -> M.Map Int Double
weightMap numPulse ids sls pfs r = listToMap ids' wvs
    where dmw = dividedMetricalWeights numPulse sls pfs
          dmwLens = Prelude.map (\x -> length x) dmw
          ws = Prelude.map (\sl -> weight sl r) sls
          wvs = weightValues ws dmwLens
          ids' = S.splitPlaces dmwLens $ downSort ids

weight :: Int -> Double -> (Double, Double)
weight sl r = (max', min')
    where max' = r ^ (sl - 1)
          min' = r ^sl

--dividedMetricalWeights :: Int -> [Int] -> [Int] -> [[Int]]
dividedMetricalWeights numPulse sls pfs = Prelude.map (\m -> M.keys m) mwmaps
    where mwmaps = dividedMetricalWeightMap numPulse sls pfs

--dividedMetricalWeightMap :: Int -> [Int] -> [Int] -> [[Int]]
dividedMetricalWeightMap numPulse sls pfs = dmwmaps
    where mwmap = metricalWeightMap numPulse sls pfs
          dmwmaps = Prelude.map (\x -> findElems mwmap x) (downSort [1..(length pfs)])

metricalWeightMap :: Int -> [Int] -> [Int] -> M.Map Int Int
metricalWeightMap numPulse sls pfs = M.fromList $ zip [0..] mws
    where mws = metricalWeights numPulse sls pfs

metricalWeights :: Int -> [Int]-> [Int] -> [Int]
metricalWeights numPulse sls pfs = sumUp mws
    where mws = Prelude.map (\sl -> metricalWeight numPulse sl pfs) sls

metricalWeight :: Int -> Int -> [Int] -> [Int]
metricalWeight numPulse sl pfs = Prelude.map selectW [n..(n + numPulse - 1)]
    where pf = L.foldl' (*) 1 (take sl pfs)
          n = numPulse `div` pf
          selectW = (\x -> if x `mod` n == 0 then 1 else 0)
