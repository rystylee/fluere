module Sound.Rhythm.DensityMap where

import Data.List (sortBy, groupBy)
import Data.Map (Map, fromList)

import Sound.Fluere.MutableMap (MutableMap, fromListM)


---------------------------------------------------------------------
-- seconds = n / beatsPerCycle
---------------------------------------------------------------------

--type DensityMap = MutableMap Int [[Int]]
type DensityMap = Map Int [[Int]]


--newDensityMap :: Int -> IO DensityMap
newDensityMap :: Int -> Int -> DensityMap
newDensityMap min' n = fromList $ newDensityList min' n

newDensityList :: Int -> Int -> [(Int, [[Int]])]
newDensityList min' n = zip [0..(length xs)] xs
    where xs = groupByLength . sortByLength $ divide min' n


divide :: Int -> Int -> [[Int]]
divide min' n = filter (\x -> checkMin x min') xs
    where xs = divide' n

checkMin :: Ord a => [a] -> a -> Bool
checkMin xs n = minimum xs >= n


divide' :: Int -> [[Int]]
divide' n = partInt n n [] []
  where partInt 0 _ xs ys = (reverse xs):ys
        partInt 1 _ xs ys = (reverse (1:xs)):ys
        partInt n 1 xs ys = (reverse (replicate n 1 ++ xs)):ys
        partInt n k xs ys = if n - k >= 0 then partInt (n - k) k (k:xs) ys' else ys'
            where  ys' = partInt n (k - 1) xs ys

sortByLength :: [[a]] -> [[a]]
sortByLength xss = sortBy (\xs ys -> compare (length xs) (length ys)) xss

groupByLength :: [[a]] -> [[[a]]]
groupByLength xss = groupBy compareLength xss

compareLength :: [a] -> [a] -> Bool
compareLength xs ys = length xs == length ys
