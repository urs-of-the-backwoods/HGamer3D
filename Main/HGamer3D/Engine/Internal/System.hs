{-# Language StandaloneDeriving, ExistentialQuantification #-}
{-# OPTIONS_HADDOCK hide #-}

-- This source file is part of HGamer3D
-- (A project to enable 3D game development in Haskell)
-- For the latest info, see http://www.hgamer3d.org
--
-- (c) 2011-2014 Peter Althainz
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

-- System.hs

-- | the System in Entity-Component-System

module HGamer3D.Engine.Internal.System

where

import Control.Concurrent
import Control.Concurrent.MVar
import qualified Data.Map as M
import Data.Maybe
import Data.Typeable
import Data.IORef

import HGamer3D.Data as D
import HGamer3D.Engine.Internal.Component
import HGamer3D.Engine.Internal.ComponentType
import HGamer3D.Engine.Internal.Entity

import System.Clock
import System.Mem.StableName
import Data.Hashable
import qualified Data.HashTable.IO as HT


-- the class of data types which have a pure and engine implementation and which can be updated



-- utility functions for systems

type IdHashTable v = HT.BasicHashTable EntityId v

data ListAndCache schema engine = ListAndCache {
  -- access from two threads
  lacAddList :: MVar [(EntityId, Component)],      
  lacRemoveList :: MVar [EntityId],   
  -- access from g3ds thread only
  lacList ::  IORef [(EntityId, Component)],    
  lacCache :: IdHashTable (engine, StampedValue schema),
  lacType :: ComponentType
  }

-- initialize
lacInitialize :: (Typeable schema, Eq schema) => ComponentType -> IO (ListAndCache schema engine)
lacInitialize ct = do
  listAdd <- newMVar []
  listRemove <- newMVar []
  listIO <- newIORef []
  cache <- HT.new
  return (ListAndCache listAdd listRemove listIO cache ct)
  
-- add component for entry
lacAdd :: (Typeable schema, Eq schema) => Entity -> ListAndCache schema engine -> IO ()
lacAdd e lac = do
  oldList <- takeMVar (lacAddList lac)
  let mCom = e #? (lacType lac)
  let newList = case mCom of
        Just com -> ((idE e, com) : oldList)
        Nothing -> oldList
  putMVar (lacAddList lac) newList

-- add component for delete
lacRemove :: (Typeable schema, Eq schema) => Entity -> ListAndCache schema engine -> IO ()
lacRemove e lac = do
  oldList <- takeMVar (lacRemoveList lac)
  putMVar (lacRemoveList lac) (idE e : oldList)


-- step system, apply all changes, this is for main components, where direct responsibility exists
lacApplyChanges :: (Typeable schema, Eq schema) =>
                   ListAndCache schema engine          -- ^ list and cache data structure
                   -> (schema -> IO engine)            -- ^ create function for new entries
                   -> (engine -> schema -> IO engine)  -- ^ update function for changing entries
                   -> (engine -> IO ())                -- ^ remove functions for removed entries
                   -> IO ()
lacApplyChanges lac create update remove = do
  -- insert
  insertList <- takeMVar (lacAddList lac)
  mapM (\(eid, c) -> do
           stampedVal <- readC c >>= return . fromJust
           let val = fromStamped stampedVal
           engineVal <- create val
           HT.insert (lacCache lac) eid (engineVal, stampedVal)
           modifyIORef (lacList lac) ( (:) (eid, c) )
           ) insertList
  putMVar (lacAddList lac) []
  
  -- remove
  removeList <- takeMVar (lacRemoveList lac)
  mapM (\eid -> do
           -- get cached value and remove
           mCacheVal <- HT.lookup (lacCache lac) eid
           case mCacheVal of
             Just (engineVal, stampedCacheVal) -> do
               remove engineVal
               HT.delete (lacCache lac) eid
             Nothing -> error "HGamer3D.Engine.Internal.System.applyAnyChanges: cache value not found"
           -- remove from current list
           modifyIORef (lacList lac) (filter (\(eid', c) -> eid /= eid'))
           ) removeList
  putMVar (lacRemoveList lac) []

  -- apply changes from compenents, taken from stamp
  currList <- readIORef (lacList lac)
  mapM (\(eid, c) -> do
           -- handle Events
           evts <- _popEvents c
           mCacheVal <- HT.lookup (lacCache lac) eid
           case mCacheVal of
             Just (engineVal, stampedCacheVal) -> do
               newStampedVal <- readC c >>= return . fromJust
               if stampedCacheVal /= newStampedVal then do
                 newEngineVal <- update engineVal (fromStamped newStampedVal)
                 HT.insert (lacCache lac) eid (newEngineVal, newStampedVal)
                 return ()
                 else return ()
             Nothing -> return (error "HGamer3D.Engine.Internal.System.applyAnyChanges: cache value not found")
           return ()
             ) currList
  return ()

-- step changes, apply all changes for supplementary components, where responsibility reside in other components
lacApplyOtherChanges :: (Typeable schema, Eq schema) =>
                        ListAndCache schema ()                   -- ^ this component, where add and remove functions are done (list ops only)
                        -> ListAndCache schema' engine'              -- ^ the main component, which is influenced by this component
                        -> (schema -> engine' -> schema' -> IO ())   -- ^ update function, updates from this component to main engine
                        -> IO ()
lacApplyOtherChanges lac lac' update' = do
  -- insert
  insertList <- takeMVar (lacAddList lac)
  mapM (\(eid, c) -> do
           stampedVal <- readC c >>= return . fromJust
           let val = fromStamped stampedVal
           HT.insert (lacCache lac) eid ((), stampedVal) -- empty engine value, only need to store stampedValue
           modifyIORef (lacList lac) ( (:) (eid, c) )
           ) insertList
  putMVar (lacAddList lac) []
  
  -- remove
  removeList <- takeMVar (lacRemoveList lac)
  mapM (\eid -> do
           -- get cached value and remove
           mCacheVal <- HT.lookup (lacCache lac) eid
           case mCacheVal of
             Just (engineVal, stampedCacheVal) -> do
               HT.delete (lacCache lac) eid
             Nothing -> error "HGamer3D.Engine.Internal.System.applyAnyChanges: cache value not found"
           -- remove from current list
           modifyIORef (lacList lac) (filter (\(eid', c) -> eid /= eid'))
           ) removeList
  putMVar (lacRemoveList lac) []

  -- apply changes from compenents, taken from stamp
  currList <- readIORef (lacList lac)
  mapM (\(eid, c) -> do
           -- handle Events
           evts <- _popEvents c
           mCacheVal <- HT.lookup (lacCache lac) eid
           case mCacheVal of
             Just (engineVal, stampedCacheVal) -> do -- engineVal is ()
               newStampedVal <- readC c >>= return . fromJust
               if stampedCacheVal /= newStampedVal then do
                 let newVal = fromStamped newStampedVal
                 -- lookup engine val and val from main current value
                 mMainCacheVal <- HT.lookup (lacCache lac') eid
                 case mMainCacheVal of
                   Just (engineMainVal, stampedMainCacheVal) -> update' newVal engineMainVal (fromStamped stampedMainCacheVal)
                   Nothing -> return (error "HGamer3D.Engine.Internal.System.applyAnyChanges: cache value not found")
                 HT.insert (lacCache lac) eid ((), newStampedVal)
                 return ()
                 else return ()
             Nothing -> return (error "HGamer3D.Engine.Internal.System.applyAnyChanges: cache value not found")
           return ()
             ) currList
  return ()

  
-- the system of entity component system, in general a system has internal state
-- entities can be added to it and the system has a step function, to run it

class System a where
      addEntity :: a -> Entity -> IO a
      removeEntity :: a -> Entity -> IO a
      stepSystem :: a -> IO (a, Bool)
      initializeSystem :: IO a
      shutdownSystem :: a -> IO ()

      runSystem :: D.TimeMS -> IO a
      runSystem stepT = do
        mv <- newEmptyMVar
        forkOS $ (\mv' -> do
                     status <- initializeSystem
                     putMVar mv' status
                     let runS s = do
                            let (TimeMS msecStepT) = stepT
                            nowT <- getTime Monotonic
                            (s', qFlag) <- stepSystem s
                            if qFlag then do
                              shutdownSystem s'
                              return ()
                              else do
                                nowT' <- getTime Monotonic
                                let diffSec = (fromIntegral ((sec nowT') - (sec nowT)))::Integer
                                let diffNSec = (fromIntegral ((nsec nowT') - (nsec nowT)))::Integer
                                let diffMSec = diffSec * 1000 + (diffNSec `div` 1000000)
                                let msecDelay = (fromIntegral msecStepT) - diffMSec
                                if msecDelay > 0 then do
                                  threadDelay ((fromIntegral msecDelay) * 1000)
                                  else do
                                    return ()
                                runS s'
                     runS status
                     ) mv
        status' <- takeMVar mv
        return status'


-- management of systems
--
        
data SomeSystem = forall a . System a => SomeSystem a

(#+) :: forall a. System a => a -> [SomeSystem] -> [SomeSystem]
a #+ as = (SomeSystem a : as) 
infixr #+

-- ECS World functions, to manage entities in systems

addToWorld :: [SomeSystem] -> Entity -> IO ()
addToWorld systems e = mapM (f e) systems >> return () where
  f e (SomeSystem s) = addEntity s e >> return ()

removeFromWorld :: [SomeSystem] -> Entity -> IO ()
removeFromWorld systems e = mapM (f e) systems >> return () where
  f e (SomeSystem s) = removeEntity s e >> return ()
    
