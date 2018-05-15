module Sound.Fluere.Core.SynthDef where

import Data.Map.Strict as M
import Sound.OSC.FD

import Sound.Fluere.Core.MutableMap (MutableMap, fromListM, insertM, lookupM)
import Sound.Fluere.Core.BaseData


---------------------------------------------------------------------
-- Construction
---------------------------------------------------------------------

newSynthDef :: String -> SynthDefParameterMap -> SynthDef
newSynthDef n sdmap = SynthDef { synthDefName = n
                               , synthDefParameter = sdmap
                               }

newSynthDefMMap :: SynthDef -> IO (MutableMap String SynthDef)
newSynthDefMMap s = fromListM [(synthDefName s, s)]

addSynthDef :: Environment -> SynthDef -> IO ()
addSynthDef e s = insertM (synthDefName s) s $ synthDefMMap e

---------------------------------------------------------------------
-- Swap
---------------------------------------------------------------------

swapSynthDef :: Environment -> String -> (SynthDef -> SynthDef) -> IO ()
swapSynthDef e n f = do
    let sdmap = synthDefMMap e
    Just s <- lookupM n sdmap
    let news = f s
    insertM n news sdmap

swapSynthDefParameter :: Environment -> String -> SynthDefParameterMap -> IO ()
swapSynthDefParameter e n newsdmap = swapSynthDef e n swapsp
    where swapsp i = i { synthDefParameter = newsdmap }

swapAmp :: Environment -> String -> Double -> IO ()
swapAmp e n newa = do
    Just s <- lookupM n $ synthDefMMap e
    let newsdmap = M.insert "amp" [string "amp", float newa] $ synthDefParameter s
    swapSynthDefParameter e n newsdmap

swapPan :: Environment -> String -> Int -> IO ()
swapPan e n newp = do
    Just s <- lookupM n $ synthDefMMap e
    let newsdmap = M.insert "pan" [string "pan", int32 newp] $ synthDefParameter s
    swapSynthDefParameter e n newsdmap

swapFreq :: Environment -> String -> Double -> IO ()
swapFreq e n newf = do
    Just s <- lookupM n $ synthDefMMap e
    let newsdmap = M.insert "freq" [string "freq", float newf] $ synthDefParameter s
    swapSynthDefParameter e n newsdmap

---------------------------------------------------------------------
-- Convert SynthDefParameter to OscScLang
---------------------------------------------------------------------

convertToOscScLang :: SynthDefParameterMap -> IO OscScLang
convertToOscScLang sdmap = return om
    where Just name = M.lookup "name" sdmap
          mapWithoutName = M.filterWithKey (\k _ -> k /= "name") sdmap
          es = Prelude.foldl (++) [] $ M.elems mapWithoutName
          om = OscScLang { scPath = "/s_new"
                         , scMessage = name ++ es
                         }

---------------------------------------------------------------------
-- SynthDefs
---------------------------------------------------------------------

kick :: SynthDef
kick = newSynthDef "kick" $ M.fromList [("name", [string "kick"]), ("freq", [string "freq", float 440]), ("amp", [string "amp", float 1.0]), ("pan", [string "pan", int32 0])]

snare :: SynthDef
snare = newSynthDef "snare" $ M.fromList [("name", [string "snare"]), ("freq", [string "freq", float 440]), ("amp", [string "amp", float 0.8]), ("pan", [string "pan", int32 0])]

closehihat :: SynthDef
closehihat = newSynthDef "closehihat" $ M.fromList [("name", [string "closehihat"]), ("freq", [string "freq", float 440]), ("amp", [string "amp", float 0.8]), ("pan", [string "pan", int32 0])]

clap :: SynthDef
clap = newSynthDef "clap" $ M.fromList [("name", [string "clap"]), ("freq", [string "freq", float 440]), ("amp", [string "amp", float 0.8]), ("pan", [string "pan", int32 0])]

cymbal :: SynthDef
cymbal = newSynthDef "cymbal" $ M.fromList [("name", [string "cymbal"]), ("freq", [string "freq", float 440]), ("amp", [string "amp", float 0.8]), ("pan", [string "pan", int32 0])]

claves :: SynthDef
claves = newSynthDef "claves" $ M.fromList [("name", [string "claves"]), ("freq", [string "freq", float 440]), ("amp", [string "amp", float 0.8]), ("pan", [string "pan", int32 0])]

cowbell :: SynthDef
cowbell = newSynthDef "cowbell" $ M.fromList [("name", [string "cowbell"]), ("freq", [string "freq", float 440]), ("amp", [string "amp", float 0.8]), ("pan", [string "pan", int32 0])]

lowconga :: SynthDef
lowconga = newSynthDef "lowconga" $ M.fromList [("name", [string "lowconga"]), ("freq", [string "freq", float 440]), ("amp", [string "amp", float 0.8]), ("pan", [string "pan", int32 0])]

middleconga :: SynthDef
middleconga = newSynthDef "middleconga" $ M.fromList [("name", [string "middleconga"]), ("freq", [string "freq", float 440]), ("amp", [string "amp", float 0.8]), ("pan", [string "pan", int32 0])]

highconga :: SynthDef
highconga = newSynthDef "highconga" $ M.fromList [("name", [string "highconga"]), ("freq", [string "freq", float 440]), ("amp", [string "amp", float 0.8]), ("pan", [string "pan", int32 0])]

maracas :: SynthDef
maracas = newSynthDef "maracas" $ M.fromList [("name", [string "maracas"]), ("freq", [string "freq", float 440]), ("amp", [string "amp", float 0.8]), ("pan", [string "pan", int32 0])]

rimshot :: SynthDef
rimshot = newSynthDef "rimshot" $ M.fromList [("name", [string "rimshot"]), ("freq", [string "freq", float 440]), ("amp", [string "amp", float 0.8]), ("pan", [string "pan", int32 0])]
