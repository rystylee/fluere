module Sound.Fluere.MutableMap where

import qualified Data.Map as M
import Control.Concurrent.STM (TVar, newTVarIO, readTVarIO, atomically, modifyTVar')

type MutableMap k a = TVar (M.Map k a)


---------------------------------------------------------------------
-- Query
---------------------------------------------------------------------

-- lookup of MutableMap version
lookupM :: Ord k => k -> MutableMap k a -> IO (Maybe a)
lookupM k mmap = do
    mmap' <- readTVarIO mmap
    return $ M.lookup k mmap'

---------------------------------------------------------------------
-- Insertion
---------------------------------------------------------------------

-- insert of MutableMap version
insertM :: Ord k => k -> a -> MutableMap k a -> IO ()
insertM k v mmap = modifyMMap' mmap $ M.insert k v

---------------------------------------------------------------------
-- Delete/Update
---------------------------------------------------------------------

deleteM :: Ord k => k -> MutableMap k a -> IO ()
deleteM k mmap = modifyMMap' mmap $ M.delete k

---------------------------------------------------------------------
-- Conversion
---------------------------------------------------------------------

keysM :: MutableMap k a -> IO [k]
keysM mmap = do
    mmap' <- readTVarIO mmap
    return $ M.keys mmap'

elemsM :: MutableMap k a -> IO [a]
elemsM mmap = do
    mmap' <- readTVarIO mmap
    return $ M.elems mmap'

---------------------------------------------------------------------
-- Lists
---------------------------------------------------------------------

-- fromList of MutableMap version
fromListM :: Ord k => [(k, a)] -> IO (MutableMap k a)
fromListM kvs = newTVarIO $ M.fromList kvs

---------------------------------------------------------------------
-- Utils
---------------------------------------------------------------------

-- modifyTVarIO, strict function
modifyMMap' :: TVar a -> (a -> a) -> IO ()
modifyMMap' mmap f = atomically $ modifyTVar' mmap f