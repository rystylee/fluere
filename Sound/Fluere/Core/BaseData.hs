module Sound.Fluere.Core.BaseData where

import Data.Map as M
import Sound.OSC.FD

import Sound.Fluere.Core.MutableMap (MutableMap)


---------------------------------------------------------------------
-- Environment
---------------------------------------------------------------------

data Environment = Environment { environmentName :: String
                               , tempoClockMMap :: MutableMap String TempoClock
                               , playerMMap :: MutableMap String Player
                               , actionMMap :: MutableMap String Action
                               , ioiSetMMap :: MutableMap String IOISet
                               , instrumentMMap :: MutableMap String Instrument
                               }

---------------------------------------------------------------------
-- TempoClock
---------------------------------------------------------------------

data TempoClock = TempoClock { tempoClockName :: String
                             , cps :: Double
                             , bpc :: Double
                             , startTime :: Double
                             , startBar :: Double
                             , startBeat :: Double
                             , clockLatency :: Double
                             } deriving (Show)

---------------------------------------------------------------------
-- Player
---------------------------------------------------------------------

data Player = Player { playerName :: String
                     , playerAction :: String
                     , playerIOISet :: String
                     , playerStatus :: Status
                     } deriving (Show)

data Status = Playing | Stopping
            deriving (Eq, Show)

---------------------------------------------------------------------
-- Action
---------------------------------------------------------------------

data Action = ConductPlayers { actionName :: String
                             , handlePlayers :: [String]
                             }
            | PlaySound { actionName :: String
                        , handleInstrument :: String
                        }
            deriving (Show)

---------------------------------------------------------------------
-- IOISet
---------------------------------------------------------------------

data IOISet = IOISet { ioiSetName :: String
                     , ioiSetLength :: Int
                     , ioiMetricalFactor :: Double
                     , ioiDensity :: Double
                     , ioiWeightFactor :: Double
                     , ioiTimeSignature :: (Int, Int)
                     , ioiSubdivisionStep :: Int
                     , ioiWeightList :: [Double]
                     , ioiProbabilityList :: [Double]
                     , ioiCounter :: Int
                     } deriving (Show)

data Beat = Beat { triggerProbability :: Double
                 , beatType :: BeatType
                 , syncopation :: Bool
                 } deriving (Show)

 -- or DownBeat, UpBeat
data BeatType = OnBeat | OffBeat
              deriving (Eq, Show)

---------------------------------------------------------------------
-- Instrument
---------------------------------------------------------------------

data Instrument = Instrument { instrumentName :: String
                             , instrumentParameter :: InstrumentParameterMap
                             } deriving (Show)

type InstrumentParameterMap = M.Map String [Datum]

---------------------------------------------------------------------
-- OSC
---------------------------------------------------------------------

data OscScLang = OscScLang { scPath :: String
                           , scMessage :: [Datum]
                           } deriving (Show)

data OscOFLang = OscOFLang { oFPath :: String
                           , oFMessage :: [Datum]
                           } deriving (Show)
