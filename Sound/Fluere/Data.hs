module Sound.Fluere.Data where

import Control.Concurrent.STM (TVar)
import Data.Map (Map)
import Sound.OSC.FD (Datum)


data DataBase = DataBase { dataBaseName :: String
                          ,clockMMap :: TVar (Map String Clock)
                          ,agentMMap :: TVar (Map String Agent)
                          ,actionMMap :: TVar (Map String Action)
                          ,patternMMap :: TVar (Map String Pattern)
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
                    ,agentAction :: String
                    ,agentPattern :: String
                    ,agentOscMessage :: [Datum]
                    ,agentStatus :: AgentStatus
                    ,agentBeat :: Double
                   } deriving (Show)

data Action = Action { actionName :: String
                      ,actionFunc :: (DataBase -> String -> IO ())
                     }

data Pattern = Pattern { patternName :: String
                        ,interval :: [Double]
                        ,counter :: Int
                       }

data AgentStatus =  Playing
                  | Pausing deriving (Show, Eq)
