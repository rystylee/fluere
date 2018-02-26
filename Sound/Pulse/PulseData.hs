module Sound.Pulse.PulseData where

import Control.Concurrent.STM (TVar)
import Data.Map (Map)
import Sound.OSC.FD (Datum)


-- PulseWorld
--
data PulseWorld = PulseWorld { worldName :: String
                              ,wClockPulseMMap :: TVar (Map String Clock)
                              ,wPlayerPulseMMap ::TVar (Map String Player)
                             }

--instance Show PulseWorld where
--    show x = worldName x
--
--

-- Clock
--
data Clock = Clock { clockName :: String
                    ,clockBpm :: Int
                    ,clockStatus :: ClockStatus
                   } deriving (Show)

data ClockStatus =   Started
                   | Stopped deriving (Show, Eq)
--
--

-- Player
--
data Player = Player { playerName :: String
                      ,playerType :: PlayerType
                      ,playerOscMessage :: [Datum]
                      ,playerScore :: [[Int]]
                      ,playerStatus :: PlayerStatus
                     } deriving (Show)

data PlayerType =   Regular
                  | Debug deriving (Show, Eq)

data PlayerStatus =   Playing
                    | Pausing deriving (Show, Eq)
--
--
