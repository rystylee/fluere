module Sound.Pulse.OSC where

import Sound.OSC.FD


-- Used to send osc message to SuperCollider
-- It is need to call this function to play a sound
sendToSC :: String -> [Datum] -> IO ()
sendToSC address oscMessage = do
    client <- openUDP "127.0.0.1" 57110
    let m = createSCMessage oscMessage
    sendOSC client $ Message address m

-- Util for creating osc message with SuperCollider
createSCMessage :: [Datum] -> [Datum]
createSCMessage oscMessage = 
    let instrument = [head oscMessage]
        defaultArgs = [int32 (-1), int32 0, int32 1]
        scArgs = tail oscMessage
     in instrument ++ defaultArgs ++ scArgs
