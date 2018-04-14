module Sound.Fluere.MutableMap ( MutableMap
                               , lookupM
                               , insertM
                               , deleteM
                               , keysM
                               , elemsM
                               , fromListM
                               ) where

import qualified Data.Map.Strict as M
import Control.Concurrent.STM (TVar, newTVarIO, readTVarIO, atomically, modifyTVar')

type MutableMap k a = TVar (M.Map k a)


---------------------------------------------------------------------
-- Query
---------------------------------------------------------------------

-- lookup of MutableMap version
lookupM :: Ord k => k -> MutableMap k a -> IO (Maybe a)
lookupM k mmap = withMMap mmap $ M.lookup k

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
keysM mmap = withMMap mmap M.keys

elemsM :: MutableMap k a -> IO [a]
elemsM mmap = withMMap mmap M.elems

---------------------------------------------------------------------
-- Lists
---------------------------------------------------------------------

-- fromList of MutableMap version
fromListM :: Ord k => [(k, a)] -> IO (MutableMap k a)
fromListM kvs = newTVarIO $ M.fromList kvs

---------------------------------------------------------------------
-- Utils
---------------------------------------------------------------------

withMMap :: TVar t -> (t -> b) -> IO b
withMMap mmap f = do
    mmap' <- readTVarIO mmap
    return $ f mmap'

-- modifyTVarIO, strict function
modifyMMap' :: TVar a -> (a -> a) -> IO ()
modifyMMap' mmap f = atomically $ modifyTVar' mmap f
