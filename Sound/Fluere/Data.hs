module Sound.Fluere.Data where

import Control.Concurrent.STM (TVar)
import Data.Map (Map)
import Sound.OSC.FD (Datum)


data DataBase = DataBase { dataBaseName :: String
                          ,clockMMap :: TVar (Map String Clock)
                          ,playerMMap ::TVar (Map String Player)
                         }

--instance Show DataBase where
--    show x = name x

data Clock = Clock { clockName :: String
                    ,cps :: Double
                    ,beat :: Double
                    ,startTime :: Double
                    ,elapsedTime :: Double
                    ,elapsedBar :: Double
                    ,elapsedBeat :: Double
                   } deriving (Show)

data Player = Player { playerName :: String
                      ,playerOscMessage :: [Datum]
                      ,playerScore :: [[Int]]
                      ,playerStatus :: PlayerStatus
                      ,lastBeat :: Double
                      ,scoreCounter :: (Int, Int)
                     } deriving (Show)

data PlayerStatus =   Playing
                    | Pausing deriving (Show, Eq)
