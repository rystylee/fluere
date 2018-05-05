module Sound.Fluere.Core.Osc (sendToSC) where

import Sound.OSC.FD

import Sound.Fluere.Core.BaseData


-- Used to send osc message to SuperCollider, for sound
sendToSC :: Double -> OscScLang -> IO ()
sendToSC lt slang = do
    client <- openUDP "127.0.0.1" 57110
    let m = createSCMessage $ scMessage slang
    sendOSC client $ Bundle (ut_to_ntpr lt) [Message (path slang) m]
    close client

-- Util for creating osc message with SuperCollider
createSCMessage :: [Datum] -> [Datum]
createSCMessage sm = instrument ++ defaultArgs ++ scArgs
    where instrument = [head sm]
          defaultArgs = [int32 (-1), int32 0, int32 1]
          scArgs = sm
