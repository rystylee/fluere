module Sound.Fluere.Stochastic.Probability where


---------------------------------------------------------------------
-- Probability of event
---------------------------------------------------------------------

probabilityList :: Double -> [Double] -> Double -> [Double]
probabilityList mf wl d = Prelude.map (\w -> calcProb mf wl d w) wl

calcProb :: Double -> [Double] -> Double -> Double -> Double
calcProb mf wl d w = d * n * ((w + delta) ** mf)
    where n = calcN mf wl delta
          delta = 0.02

calcN :: Double -> [Double] -> Double -> Double
calcN mf wl delta = top / tp
    where tp = calcTotalProb mf wl delta
          top = calcOrgTotalProb wl

calcTotalProb :: Double -> [Double] -> Double -> Double
calcTotalProb mf wl delta = sum $ Prelude.map (\w -> (w + delta) ** mf) wl

calcOrgTotalProb :: [Double] -> Double
calcOrgTotalProb wl = sum wl
