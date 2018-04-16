module Sound.Rhythm.LSystem (generateTree) where

import Data.List.Split (splitOn)
import qualified Data.Map as M


---------------------------------------------------------------------
-- LSystem
-- ex.) generateTree ([1,2,1], [2,2]) "a:ab b:a" "a" 2
--      => [1.0,2.0,1.0,2.0,2.0,1.0,2.0,1.0]
---------------------------------------------------------------------

generateTree :: ([Double], [Double]) -> String -> String -> Int -> [Double]
generateTree symbol rule initial n = concat $ replace symbol k s
    where k = ((parse rule) !! 0 !! 0, (parse rule) !! 1 !! 0) 
          s = iterateLSystem initial n $ makeRuleMap rule

-- first argment xs means initial value of L-System
iterateLSystem :: String -> Int -> M.Map String String -> String
iterateLSystem xs n m
    | n <= 0 = xs
    | otherwise = iterateLSystem val (n - 1) m
    where val = applyLSystem xs m

applyLSystem :: String -> M.Map String String -> String
applyLSystem (x:xs) m
    | null xs = val
    | otherwise = val ++ applyLSystem xs m
    where Just val = applyRule [x] m

---------------------------------------------------------------------
-- Rule
---------------------------------------------------------------------

applyRule :: String -> M.Map String String -> Maybe String
applyRule k m = M.lookup k m

makeRuleMap :: String -> M.Map String String
makeRuleMap xs = M.fromList . map makeRule $ parse xs
    where makeRule xs = (xs !! 0, xs !! 1)

parse :: String -> [[String]]
parse xs = map (splitOn ":") $ words xs

---------------------------------------------------------------------
-- Util
---------------------------------------------------------------------

replace :: ([Double], [Double]) -> (String, String) -> String -> [[Double]]
replace (ls1, ls2) (str1:[], str2:[]) xs = map (\x -> if x == str1 then ls1 else ls2) xs
