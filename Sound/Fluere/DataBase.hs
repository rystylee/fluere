module Sound.Fluere.DataBase where

import Control.Concurrent.STM (TVar)
import Data.Map (Map)

import Sound.Fluere.Data


newDataBase :: String
               -> TVar (Map String Clock)
               -> TVar (Map String Player)
               -> TVar (Map String Action)
               -> TVar (Map String Pattern)
               -> DataBase
newDataBase dbname clmmap playermmap actmmap patternmmap =
  DataBase { dataBaseName = dbname
            ,clockMMap = clmmap
            ,playerMMap = playermmap
            ,actionMMap = actmmap
            ,patternMMap = patternmmap
           }
