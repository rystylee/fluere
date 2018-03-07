module Sound.Fluere.OSC where

import Sound.OSC.FD


-- Used to send osc message to SuperCollider, for sound
sendToSC :: String -> [Datum] -> IO ()
sendToSC address oscMessage = do
    client <- openUDP "127.0.0.1" 57110
    let m = createSCMessage oscMessage
    sendOSC client $ Message address m
    close client

-- Used to send osc message to openFrameworks, for visual
sendToOF :: String -> [Datum] -> IO ()
sendToOF address oscMessage = do
    client <- openUDP "127.0.0.1" 8000
    let m = createSCMessage oscMessage
    sendOSC client $ Message address m
    close client

-- Util for creating osc message with SuperCollider
createSCMessage :: [Datum] -> [Datum]
createSCMessage oscMessage = 
    let instrument = [head oscMessage]
        defaultArgs = [int32 (-1), int32 0, int32 1]
        scArgs = tail oscMessage
     in instrument ++ defaultArgs ++ scArgs
