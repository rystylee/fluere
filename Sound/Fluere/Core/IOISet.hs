module Sound.Fluere.Core.IOISet where

import Sound.Fluere.Core.MutableMap (MutableMap, fromListM, lookupM, insertM)
import Sound.Fluere.Core.BaseData
import Sound.Fluere.Core.Environment (getIOISetNames)

import Sound.Fluere.Stochastic.MetricalWeight (weightList)
import Sound.Fluere.Stochastic.Probability (probabilityList)


---------------------------------------------------------------------
-- Construction
---------------------------------------------------------------------

newIOISet :: String
          -> Int
          -> Double
          -> Double
          -> Double
          -> (Int, Int)
          -> Int
          -> [Double]
          -> [Double]
          -> Int
          -> IOISet
newIOISet n l mf d wf ts step wl pl c =
    IOISet { ioiSetName = n
           , ioiSetLength = l
           , ioiMetricalFactor = mf
           , ioiDensity = d
           , ioiWeightFactor = wf
           , ioiTimeSignature = ts
           , ioiSubdivisionStep = step
           , ioiWeightList = wl
           , ioiProbabilityList = pl
           , ioiCounter = c
           }

newIOISetMMap :: IOISet -> IO (MutableMap String IOISet)
newIOISetMMap ioi = fromListM [(ioiSetName ioi, ioi)]

addIOISet :: Environment -> IOISet -> IO ()
addIOISet e ioi = insertM (ioiSetName ioi) ioi $ ioiSetMMap e

---------------------------------------------------------------------
-- Swap
---------------------------------------------------------------------

swapIOISet :: Environment -> String -> (IOISet -> IOISet) -> IO ()
swapIOISet e n f = do
    let ioimmap = ioiSetMMap e
    Just ioi <- lookupM n ioimmap
    let newioi = f ioi
    insertM n newioi ioimmap

swapIOIMetricalFactor :: Environment -> String -> Double -> IO ()
swapIOIMetricalFactor e n newmf = do
    Just ioi <- lookupM n $ ioiSetMMap e
    let newwl = weightList (ioiTimeSignature ioi) (ioiSubdivisionStep ioi) $ ioiWeightFactor ioi
        newpl = probabilityList newmf (ioiDensity ioi) newwl
        swapmf ioi = ioi { ioiSetName = n
                         , ioiSetLength = length newpl
                         , ioiMetricalFactor = newmf
                         , ioiDensity = ioiDensity ioi
                         , ioiWeightFactor = ioiWeightFactor ioi
                         , ioiTimeSignature = ioiTimeSignature ioi
                         , ioiSubdivisionStep = ioiSubdivisionStep ioi
                         , ioiWeightList = newwl
                         , ioiProbabilityList = newpl
                         , ioiCounter = 0
                         }
    swapIOISet e n swapmf

swapIOIDensity :: Environment -> String -> Double -> IO ()
swapIOIDensity e n newd = do
    Just ioi <- lookupM n $ ioiSetMMap e
    let newwl = weightList (ioiTimeSignature ioi) (ioiSubdivisionStep ioi) $ ioiWeightFactor ioi
        newpl = probabilityList (ioiMetricalFactor ioi) newd newwl
        swapd ioi = ioi { ioiSetName = n
                        , ioiSetLength = length newpl
                        , ioiMetricalFactor = ioiMetricalFactor ioi
                        , ioiDensity = newd
                        , ioiWeightFactor = ioiWeightFactor ioi
                        , ioiTimeSignature = ioiTimeSignature ioi
                        , ioiSubdivisionStep = ioiSubdivisionStep ioi
                        , ioiWeightList = newwl
                        , ioiProbabilityList = newpl
                        , ioiCounter = 0
                        }
    swapIOISet e n swapd

swapIOIWeightFactor :: Environment -> String -> Double -> IO ()
swapIOIWeightFactor e n newwf = do
    Just ioi <- lookupM n $ ioiSetMMap e
    let newwl = weightList (ioiTimeSignature ioi) (ioiSubdivisionStep ioi) newwf
        newpl = probabilityList (ioiMetricalFactor ioi) (ioiDensity ioi) newwl
        swapwf ioi = ioi { ioiSetName = n
                         , ioiSetLength = length newpl
                         , ioiMetricalFactor = ioiMetricalFactor ioi
                         , ioiDensity = ioiDensity ioi
                         , ioiWeightFactor = newwf
                         , ioiTimeSignature = ioiTimeSignature ioi
                         , ioiSubdivisionStep = ioiSubdivisionStep ioi
                         , ioiWeightList = newwl
                         , ioiProbabilityList = newpl
                         , ioiCounter = 0
                         }
    swapIOISet e n swapwf

swapIOITimeSignature :: Environment -> String -> (Int, Int) -> IO ()
swapIOITimeSignature e n newts = do
    Just ioi <- lookupM n $ ioiSetMMap e
    let newwl = weightList newts (ioiSubdivisionStep ioi) (ioiWeightFactor ioi)
        newpl = probabilityList (ioiMetricalFactor ioi) (ioiDensity ioi) newwl
        swapts ioi = ioi { ioiSetName = n
                         , ioiSetLength = length newpl
                         , ioiMetricalFactor = ioiMetricalFactor ioi
                         , ioiDensity = ioiDensity ioi
                         , ioiWeightFactor = ioiWeightFactor ioi
                         , ioiTimeSignature = newts
                         , ioiSubdivisionStep = ioiSubdivisionStep ioi
                         , ioiWeightList = newwl
                         , ioiProbabilityList = newpl
                         , ioiCounter = 0
                         }
    swapIOISet e n swapts

swapIOISubdivisionStep :: Environment -> String -> Int -> IO ()
swapIOISubdivisionStep e n newstep = do
    Just ioi <- lookupM n $ ioiSetMMap e
    let newwl = weightList (ioiTimeSignature ioi) newstep (ioiWeightFactor ioi)
        newpl = probabilityList (ioiMetricalFactor ioi) (ioiDensity ioi) newwl
        swapstep ioi = ioi { ioiSetName = n
                           , ioiSetLength = length newpl
                           , ioiMetricalFactor = ioiMetricalFactor ioi
                           , ioiDensity = ioiDensity ioi
                           , ioiWeightFactor = ioiWeightFactor ioi
                           , ioiTimeSignature = ioiTimeSignature ioi
                           , ioiSubdivisionStep = newstep
                           , ioiWeightList = newwl
                           , ioiProbabilityList = newpl
                           , ioiCounter = 0
                           }
    swapIOISet e n swapstep

swapIOICounter :: Environment -> String -> Int -> IO ()
swapIOICounter e n newc = swapIOISet e n swapc
    where swapc ioi = ioi { ioiCounter = newc }


---------------------------------------------------------------------
-- batch manipulation

swapIOIMetricalFactors :: Environment -> [String] -> Double -> IO ()
swapIOIMetricalFactors e ns mf = sequence_ $ map (\n -> swapIOIWeightFactor e n mf) ns

swapAllIOIMetricalFactors :: Environment -> Double -> IO ()
swapAllIOIMetricalFactors e mf = do
    ioinames <- getIOISetNames e
    swapIOIMetricalFactors e ioinames mf

swapIOIDensities :: Environment -> [String] -> Double -> IO ()
swapIOIDensities e ns d = sequence_ $ map (\n -> swapIOIDensity e n d) ns

swapAllIOIDensities :: Environment -> Double -> IO ()
swapAllIOIDensities e d = do
    ioinames <- getIOISetNames e
    swapIOIDensities e ioinames d

swapIOIWeightFactors :: Environment -> [String] -> Double -> IO ()
swapIOIWeightFactors e ns wf = sequence_ $ map (\n -> swapIOIWeightFactor e n wf) ns

swapAllIOIWeightFactors :: Environment -> Double -> IO ()
swapAllIOIWeightFactors e wf = do
    ioinames <- getIOISetNames e
    swapIOIWeightFactors e ioinames wf

swapIOITimeSignatures :: Environment -> [String] -> (Int, Int) -> IO ()
swapIOITimeSignatures e ns ts = sequence_ $ map (\n -> swapIOITimeSignature e n ts) ns

swapAllIOITimeSignatures :: Environment -> (Int, Int) -> IO ()
swapAllIOITimeSignatures e ts = do
    ioinames <- getIOISetNames e
    swapIOITimeSignatures e ioinames ts

swapIOISubdivisionSteps :: Environment -> [String] -> Int -> IO ()
swapIOISubdivisionSteps e ns step = sequence_ $ map (\n -> swapIOISubdivisionStep e n step) ns

swapAllIOISubdivisionSteps :: Environment -> Int -> IO ()
swapAllIOISubdivisionSteps e step = do
    ioinames <- getIOISetNames e
    swapIOISubdivisionSteps e ioinames step

resetIOICounters :: Environment -> [String] -> IO ()
resetIOICounters e ns = sequence_ $ map (\n -> swapIOICounter e n 0) ns

resetAllIOICounters :: Environment -> IO ()
resetAllIOICounters e = do
    ioinames <- getIOISetNames e
    resetIOICounters e ioinames

---------------------------------------------------------------------
-- Get probability of next event
---------------------------------------------------------------------

nextProb :: Environment -> Player -> IO Double
nextProb e p = do
    Just ioi <- lookupM (playerIOISet p) (ioiSetMMap e)
    let pl = ioiProbabilityList ioi
        c = ioiCounter ioi
    if (c == (length pl - 1))
        then do
            swapIOICounter e (playerIOISet p) 0
        else do
            swapIOICounter e (playerIOISet p) (c + 1)
    return $ pl !! c
