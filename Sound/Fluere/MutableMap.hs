module Sound.Fluere.MutableMap where

import Control.Concurrent.STM ( TVar
                               ,atomically
                               ,newTVarIO
                               ,modifyTVar'
                               ,readTVarIO
                              )
import qualified Data.Map as M


-- Used to create a new MutableMMap
newMMap :: Ord k => [(k, a)] -> IO (TVar (M.Map k a))
newMMap kvs = newTVarIO $ M.fromList kvs

-- Used to find a value from the existing MutableMap
findValueFromMMap :: Ord k => k -> TVar (M.Map k a) -> IO (Maybe a)
findValueFromMMap k mmap = do
    mmap' <- readTVarIO mmap
    return $ M.lookup k mmap'

-- Used to change the existing MutableMap
-- do the operation given to the argument on the MutableMap
changeMMap :: TVar a -> (a -> a) -> IO ()
changeMMap mmap f = atomically $ modifyTVar' mmap f

-- Used to add value to the existing MutableMap
addValToMMap :: Ord k => (k, a) -> TVar (M.Map k a) -> IO () 
addValToMMap (k, v) mmap = changeMMap mmap $ M.insert k v

-- Used to delete value from the existing MutableMap
deleteValFromMMap :: Ord k => k -> TVar (M.Map k a) -> IO ()
deleteValFromMMap k mmap  = changeMMap mmap $ M.delete k
