module Sound.Fluere.Bjorklund where

--import Data.List.Split


---------------------------------------------------------------------
-- The below is (c) Rohan Drape, taken from the hmt library and
-- distributed here under the terms of the GNU Public Licence.
---------------------------------------------------------------------

type STEP a = ((Int, Int), ([[a]], [[a]]))

left :: STEP a -> STEP a
left ((i, j), (xs, ys)) = ((j, i-j), (zipWith (++) xs' ys, xs''))
    where (xs', xs'') = splitAt j xs

right :: STEP a -> STEP a
right ((i, j), (xs, ys)) = ((i, j - i), (zipWith (++) xs ys', ys''))
    where (ys', ys'') = splitAt i ys

bjorklund' :: STEP a -> STEP a
bjorklund' (n, x) = if min i j <= 1 then (n, x) else bjorklund' (if i > j then left (n, x) else right (n, x))
    where (i, j) = n

--bjorklund :: (Int, Int) -> [Bool]
--bjorklund (i, j') = concat x' ++ concat y'
--    where j = j' - i
--          x = replicate i [True]
--          y = replicate j [False]
--          (_, (x', y')) = bjorklund' ((i, j), (x, y))

bjorklund :: (Int, Int) -> [Int]
bjorklund (i, j') = concat x' ++ concat y'
    where j = j' - i
          x = replicate i [1]
          y = replicate j [0]
          (_, (x', y')) = bjorklund' ((i, j), (x, y))

---- | The 'iseq' of a pattern is the distance between 'True' values.
----
---- > iseq (bjorklund (5,9)) == [2,2,2,2,1]
--iseq :: [Bool] -> [Int]
--iseq =
--    let f = split . keepDelimsL . whenElt
--    in tail . map length . f (== True)
--
---- | 'iseq' of pattern as compact string.
----
---- > iseq_str (bjorklund (5,9)) == "(22221)"
--iseq_str :: [Bool] -> String
--iseq_str = let f xs = "(" ++ concatMap show xs ++ ")"
--           in f . iseq
