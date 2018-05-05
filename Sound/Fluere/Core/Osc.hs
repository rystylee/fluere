module Sound.Fluere.Core.Osc where

import Sound.OSC.FD

import Sound.Fluere.Core.BaseData


-- Used to send osc message to SuperCollider, for sound
sendToSC :: Double -> OscScLang -> IO ()
sendToSC lt slang = do
    client <- openUDP "127.0.0.1" 57110
    let m = createSCMessage $ scMessage slang
    sendOSC client $ Bundle (ut_to_ntpr lt) [Message (scPath slang) m]
    close client

-- Used to send osc message to SuperCollider, for sound
sendToOF :: Double -> OscOFLang -> IO ()
sendToOF lt oflang = do
    client <- openUDP "127.0.0.1" 8000
    let m = createOFMessage $ oFMessage oflang
    sendOSC client $ Bundle (ut_to_ntpr lt) [Message (oFPath oflang) m]
    close client

-- Util for creating osc message with SuperCollider
createSCMessage :: [Datum] -> [Datum]
createSCMessage sm = instrument ++ defaultArgs ++ scArgs
    where instrument = [head sm]
          defaultArgs = [int32 (-1), int32 0, int32 1]
          scArgs = tail sm

createOFMessage :: [Datum] -> [Datum]
createOFMessage om = om

convertToOscOFLang :: IOISet -> IO OscOFLang
convertToOscOFLang ioi = return om
    where l = ioiSetLength ioi
          cl = [int32 l]
          pl = ioiProbabilityList ioi
          dpl = map (\x -> float x) pl
          c = ioiCounter ioi
          dc = [int32 c]
          om = OscOFLang { oFPath = ioiSetName ioi
                         , oFMessage = cl ++ dc ++ dpl
                         }
