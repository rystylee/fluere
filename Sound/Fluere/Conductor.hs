module Sound.Fluere.Conductor where

import Control.Concurrent.STM (TVar)
import Data.Map (Map)

import Sound.Fluere.Data
import Sound.Fluere.MutableMap ( newMMap
                                ,findValueFromMMap
                                ,addValToMMap
                               )


-- Used to create a new Conductor
newConductor :: String -> [String] -> Conductor
newConductor cname tagents = Conductor {
     conductorName = cname
    ,targetAgents = tagents
}

-- Used to create a new Conductor MutableMap
newConductorMMap :: Conductor -> IO (TVar (Map String Conductor))
newConductorMMap conductor = newMMap [(conductorName conductor, conductor)]

-- Used to add a new Conductor to MutableMap
addConductor :: DataBase -> Conductor -> IO ()
addConductor db conductor = do
    addValToMMap (conductorName conductor, conductor) (conductorMMap db)

-- The base function to change Conductor
changeConductor :: DataBase -> String -> (Conductor -> Conductor) -> IO ()
changeConductor db cname f = do
    let cmmap = conductorMMap db
    Just conductor <- findValueFromMMap cname cmmap
    let newConductor = f conductor
    addValToMMap (cname, newConductor) cmmap
