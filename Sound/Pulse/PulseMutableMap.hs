module Sound.Pulse.PulseMutableMap where

import Control.Concurrent.STM ( TVar
                               ,atomically
                               ,newTVarIO
                               ,modifyTVar'
                               ,readTVarIO
                              )
import qualified Data.Map as M


-- Used to create a newPulseMutableMMap
newPulseMMap :: Ord k => [(k, a)] -> IO (TVar (M.Map k a))
newPulseMMap kvs = newTVarIO $ M.fromList kvs

-- Used to find a value from the existing PulseMutableMap
findValueFromPulseMMap :: Ord k => k -> TVar (M.Map k a) -> IO (Maybe a)
findValueFromPulseMMap k mmap = do
    -- mmap  :: TVar (Map String Player)
    -- mmap' :: Map String Player
    mmap' <- readTVarIO mmap
    return $ M.lookup k mmap'


-- Used to change the existing PulseMutableMap
-- do the operation given to the argument on the PulseMutableMap
changePulseMMap :: TVar a -> (a -> a) -> IO ()
changePulseMMap mmap f = atomically $ modifyTVar' mmap f

-- Used to add value to the existing PulseMutableMap
addValToPulseMMap :: Ord k => (k, a) -> TVar (M.Map k a) -> IO () 
addValToPulseMMap (k, v) mmap = changePulseMMap mmap $ M.insert k v

-- Used to delete value from the existing PulseMutableMap
deleteValFromPulseMMap :: Ord k => k -> TVar (M.Map k a) -> IO ()
deleteValFromPulseMMap k mmap  = changePulseMMap mmap $ M.delete k
