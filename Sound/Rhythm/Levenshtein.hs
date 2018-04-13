module Sound.Rhythm.Levenshtein where


---------------------------------------------------------------------
-- Levenshtein distance
-- ex.) levenshtein [1,2,1,4,2,2] [2,2]
-- ==> [0,1,2,1,1,2,2,1,1,3,2,2,4,3,3,5,4,3,6,5,4]
-- ==> 4
-- ex.) levenshteinNorm [1,2,1,4,2,2] [2,2]
-- ==> [0,1,2,1,1,2,2,1,1,3,2,2,4,3,3,5,4,3,6,5,4]
-- ==> 0.6666667
---------------------------------------------------------------------

levenshtein :: Eq a => [a] -> [a] -> Int
levenshtein ls1 ls2 = last disList
    where disList = map dist [(x, y) | x <- [0..m], y <- [0..n]]
          m = length ls1
          n = length ls2
          dist (0, 0) = 0
          dist (i, 0) = i
          dist (0, j) = j
          dist (i, j) = minimum [a, b, c]
              where a = disList !! (i * (n + 1) + (j - 1)) + 1
                    b = disList !! ((i - 1) * (n + 1) + j) + 1
                    c = disList !! ((i - 1) * (n + 1) + (j - 1)) + cost
                    cost = if ls1 !! (i - 1) == ls2 !! (j - 1) then 0 else 1

-- Normalization version
levenshteinNorm :: Eq a => [a] -> [a] -> Float
levenshteinNorm ls1 ls2 = norm levenshtein ls1 ls2

---------------------------------------------------------------------
-- Damerau-Levenshtein distance
-- This is Levenshtein distance that allowed transposition of elements.
---------------------------------------------------------------------

damerauLevenshtein :: Eq a => [a] -> [a] -> Int
damerauLevenshtein ls1 ls2 = undefined

-- Normalization version
damerauLevenshteinNorm :: Eq a => [a] -> [a] -> Float
damerauLevenshteinNorm ls1 ls2 = norm damerauLevenshtein ls1 ls2

---------------------------------------------------------------------
-- Util
---------------------------------------------------------------------

norm :: Eq a => ([a] -> [a] -> Int) -> [a] -> [a] -> Float
norm f ls1 ls2 = fromIntegral val / fromIntegral maxLen
    where val = f ls1 ls2
          maxLen = if m > n then m else n
          m = length ls1
          n = length ls2
