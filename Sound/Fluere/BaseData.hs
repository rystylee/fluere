module Sound.Fluere.BaseData where

import Control.Concurrent.STM (TVar)
import Data.Map (Map)
import Sound.OSC.FD (Datum)


-- FluereWorld
--
data FluereWorld = FluereWorld { worldName :: String
                                ,wClockMMap :: TVar (Map String Clock)
                                ,wPlayerMMap ::TVar (Map String Player)
                               }

--instance Show FluereWorld where
--    show x = worldName x
--
--

-- Clock
--
data Clock = Clock { clockName :: String
                    ,clockBpm ::Double
                    ,clockBeat :: Double
                    ,lastEventTime :: Double
                    ,nextEventTime :: Double
                    ,startTime :: Double
                   } deriving (Show)
--
--

-- Player
--
data Player = Player { playerName :: String
                      ,playerOscMessage :: [Datum]
                      ,playerScore :: [[Int]]
                      ,playerStatus :: PlayerStatus
                     } deriving (Show)

data PlayerStatus =   Playing
                    | Pausing deriving (Show, Eq)
--
--
