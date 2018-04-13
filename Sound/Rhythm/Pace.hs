module Sound.Rhythm.Pace where


---------------------------------------------------------------------
--
-- xs を渡すと、緩急をつけてくれる
-- [1,2,1,2,2,2,4] -> []
---------------------------------------------------------------------

data PaceLevel = High | Middle | Low

makePace :: [Double] -> Int -> [Double]
makePace xs n = undefined
