module Sound.Pulse.PulseMutableMap where

import Control.Concurrent.STM
import qualified Data.Map as M

type PulseMutableMap k v = (TVar (M.Map k v))


-- Used to create a newPulseMutableMMap
newPulseMMap :: Ord k => [(k, a)] -> IO (TVar (M.Map k a))
newPulseMMap kvs = do
    newMMap <- newTVarIO $ M.fromList kvs
    return newMMap

-- Used to change the existing PulseMutableMap
changePulseMMap :: TVar a -> (a -> a) -> IO ()
changePulseMMap mmap f = atomically $ modifyTVar' mmap f

-- Used to add value to the existing PulseMutableMap
addValToPulseMMap :: Ord k => TVar (M.Map k a) -> (k, a) -> IO () 
addValToPulseMMap mmap (k, v) = changePulseMMap mmap $ M.insert k v

-- Used to delete value from the existing PulseMutableMap
deleteValFromPulseMMap :: Ord k => TVar (M.Map k a) -> k -> IO ()
deleteValFromPulseMMap mmap k = changePulseMMap mmap $ M.delete k
