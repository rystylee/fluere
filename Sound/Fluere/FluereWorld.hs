module Sound.Fluere.FluereWorld where

import Control.Concurrent.STM (TVar)
import Data.Map (Map)

import Sound.Fluere.FluereData


-- These functions are used to create data with FluereWorld
--
newFluereWorld ::  String
               -> TVar (Map String Clock)
               -> TVar (Map String Player)
               -> FluereWorld
newFluereWorld wname cmmap pmmap =
    FluereWorld { worldName = wname
                 ,wClockMMap = cmmap
                 ,wPlayerMMap = pmmap
                }
--
--
