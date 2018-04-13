module Sound.Rhythm.Util where


---------------------------------------------------------------------
-- For manipulating list utils
-- ex.) If [[1,2], [1,1,2], [1.5,0.5]], 4 (beat) is given as an argument,
-- it returns [[1,2,1], [1,1,2], [1.5, 0.5, 2]].
---------------------------------------------------------------------

--adjustList :: [[Double]] -> Double -> [[Double]]
--adjustList ls beat' = map f ls
--    where f = (flip adjustToBeat) beat'
--
--adjustToBeat :: [Double] -> Double -> [Double]
--adjustToBeat ls beat'
--    | beat' == sum ls    = ls
--    | beat' - sum ls < 0 = decreaseToBeat ls beat' $ sum ls
--    | beat' - sum ls > 0 = increaseToBeat ls beat' $ sum ls
--
--increaseToBeat :: [Double] -> Double -> Double -> [Double]
--increaseToBeat ls beat' sum'
--    | sum' == beat' = ls
--    | sum' < beat'  = ls ++ [beat' - sum']
--    | sum' > beat'  = decreaseToBeat ls beat' sum'
--
--decreaseToBeat :: [Double] -> Double -> Double -> [Double]
--decreaseToBeat ls beat' sum'
--    | sum' == beat' = ls
--    | sum' < beat'  = increaseToBeat ls beat' sum'
--    | sum' > beat'  = decreaseToBeat (init ls) beat' (sum' - last ls)
