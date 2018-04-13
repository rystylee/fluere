module Sound.Rhythm.Stream where

import Data.List (sort, reverse)


---------------------------------------------------------------------
--
---------------------------------------------------------------------

--data Intensity = High | Middle | Low
-- rapid <-> slow

-- 0 ~ 1
complexity :: Double
complexity = 1.0

localStream :: [Double] -> Int -> [Double]
localStream xs n = xs' ++ xs''
    where xs' = sort xs
          xs'' = reverse xs'

globalStream = undefined

swap = undefined 
