module Sound.Fluere.Data where

import Sound.OSC.FD (Datum)

import Sound.Fluere.MutableMap (MutableMap)


data DataBase = DataBase { tempoClockMMap :: MutableMap String TempoClock
                         , playerMMap :: MutableMap String Player
                         , actionMMap :: MutableMap String Action
                         , patternMMap :: MutableMap String Pattern
                         }

data TempoClock = TempoClock { tempoClockName :: String
                             , cps :: Double
                             , beatsPerCycle :: Double
                             , startTime :: Double
                             , startBar :: Double
                             , startBeat :: Double
                             , clockLatency :: Double
                             } deriving (Show)

data Player = Player { playerName :: String
                     , playerAction :: String
                     , playerPattern :: String
                     } deriving (Show)

data OscScLang = OscScLang { path :: String
                           , scMessage :: [Datum]
                           } deriving (Show)

data Action = PlaySound { actionName :: String
                        , oscMessage :: OscScLang
                        }
            | DisplayMessage { actionName :: String
                             , displayMessage :: String
                             }
            deriving (Show)

data Pattern = Pattern { patternName :: String
                       , durations :: [Int]
                       , index :: Int
                       } deriving (Show)
