module Sound.Fluere.Data where

import Control.Concurrent.STM (TVar)
import Data.Map (Map)
import Sound.OSC.FD (Datum)


data DataBase = DataBase { dataBaseName :: String
                          ,clockMMap :: TVar (Map String Clock)
                          ,agentMMap :: TVar (Map String Agent)
                         }

data Tempo = Tempo { cps :: Double
                    ,beat :: Double
                   } deriving (Show)

data TempoHistory = TempoHistory { tempo :: Tempo
                                  ,startTime :: Double
                                  ,startBar :: Double
                                  ,startBeat :: Double
                                  ,lastBar :: Double
                                  ,lastBeat :: Double
                                 } deriving (Show)

data Clock = Clock { clockName :: String
                    ,tempoHistories :: [TempoHistory]
                   } deriving (Show)

data Agent = Agent { agentName :: String
                    ,agentClock :: String
                    ,agentOscMessage :: [Datum]
                    ,agentScore :: [[Int]]
                    ,agentStatus :: AgentStatus
                    ,beatToStart :: Double
                    ,scoreCounter :: (Int, Int)
                   } deriving (Show)

data AgentStatus =  Playing
                  | Pausing deriving (Show, Eq)
