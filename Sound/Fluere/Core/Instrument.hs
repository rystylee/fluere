module Sound.Fluere.Core.Instrument where

import Data.Map.Strict as M
import Sound.OSC.FD

import Sound.Fluere.Core.MutableMap (MutableMap, fromListM, insertM, lookupM)
import Sound.Fluere.Core.BaseData


---------------------------------------------------------------------
-- Construction
---------------------------------------------------------------------

newInstrument :: String -> InstrumentParameterMap -> Instrument
newInstrument n ipmap = Instrument { instrumentName = n
                                   , instrumentParameter = ipmap
                                   }

newInstrumentMMap :: Instrument -> IO (MutableMap String Instrument)
newInstrumentMMap i = fromListM [(instrumentName i, i)]

addInstrument :: Environment -> Instrument -> IO ()
addInstrument e i = insertM (instrumentName i) i $ instrumentMMap e

---------------------------------------------------------------------
-- Swap
---------------------------------------------------------------------

swapInstrument :: Environment -> String -> (Instrument -> Instrument) -> IO ()
swapInstrument e n f = do
    let immap = instrumentMMap e
    Just i <- lookupM n immap
    let newi = f i
    insertM n newi immap

swapInstrumentParameter :: Environment -> String -> InstrumentParameterMap -> IO ()
swapInstrumentParameter e n newipmap = swapInstrument e n swapip
    where swapip i = i { instrumentParameter = newipmap }

swapAmp :: Environment -> String -> Double -> IO ()
swapAmp e n newa = do
    Just i <- lookupM n $ instrumentMMap e
    let newipmap = M.insert "amp" [string "amp", float newa] $ instrumentParameter i
    swapInstrumentParameter e n newipmap

---------------------------------------------------------------------
-- Convert InstrumentParameter to OscScLang
---------------------------------------------------------------------

convertToOscScLang :: InstrumentParameterMap -> IO OscScLang
convertToOscScLang ipmap = return om
    where Just name = M.lookup "name" ipmap
          mapWithoutName = M.filterWithKey (\k _ -> k /= "name") ipmap
          es = Prelude.foldl (++) [] $ M.elems mapWithoutName
          om = OscScLang { scPath = "/s_new"
                         , scMessage = name ++ es
                         }

---------------------------------------------------------------------
-- Instruments
---------------------------------------------------------------------

kick :: Instrument
kick = newInstrument "kick" $ M.fromList [("name", [string "kick"]), ("freq", [string "freq", float 440]), ("amp", [string "amp", float 1.0]), ("pan", [string "pan", int32 0])]

snare :: Instrument
snare = newInstrument "snare" $ M.fromList [("name", [string "snare"]), ("freq", [string "freq", float 440]), ("amp", [string "amp", float 0.8]), ("pan", [string "pan", int32 0])]

hihat :: Instrument
hihat = newInstrument "hihat" $ M.fromList [("name", [string "hihat"]), ("freq", [string "freq", float 440]), ("amp", [string "amp", float 0.8]), ("pan", [string "pan", int32 0])]
