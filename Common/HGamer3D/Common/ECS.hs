{-# Language ExistentialQuantification #-}
{-# OPTIONS_HADDOCK hide #-}
-- This source file is part of HGamer3D
-- (A project to enable 3D game development in Haskell)
-- For the latest info, see http://www.althainz.de/HGamer3D.html
--
-- (c) 2014 Peter Althainz
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

-- | The Entity Component System for HGamer3D
module HGamer3D.Common.ECS
where

import Data.Maybe
import Data.Dynamic
import Data.Typeable
import qualified Data.Map as M
import Data.IORef

import Control.Concurrent
import Control.Concurrent.STM
import Control.Applicative

import HGamer3D.Data


-- Components

-- | Possible Components, which are known, this list needs to be extended, if
--   additional components are introduced. Each component can occur only once in
--   an Entity.

data Component =       CTPos    -- ^ Position
                     | CTOri    -- ^ Orientation
                     | CTSiz    -- ^ Size
                     | CTFig    -- ^ Figure
                     | CTASr    -- ^ Audio Source
                     | CTALs    -- ^ Audio Listener
                     | CTCam    -- ^ Camera
                     | CTLig    -- ^ Light
                     | CTScP    -- ^ Scene Parameter
                     | CTGFo    -- ^ GUI Form
                     | CTWin    -- ^ Window
                     | CTJoI    -- ^ JoystickInfo
                     | CTJoV    -- ^ JoystickValue
                     | CTMou    -- ^ Mouse
                     | CTNNo    -- ^ Network Node
                     | CTCmd    -- ^ internal, used for sending commands, created automatically
                     | CTEvt    -- ^ internal, used for receiving events, created automatically 
                       deriving (Eq, Ord, Show)

-- | Entities

-- | Entity, Maps from Component to Dynamic
type Entity = M.Map Component Dynamic

-- | Pair builder for nice construction syntax, allows [ ct #: val, ...] syntax
(#:) :: Typeable a => Component -> a -> (Component, Dynamic)
c #: val = (c, toDyn val)

-- | Builder for entities, allows newE = entity [ct #: val, ...] syntax
entity :: [(Component, Dynamic)] -> Entity
entity clist = M.fromList clist

-- | does the entity have the component
(#?) :: Entity -> Component -> Bool
e #? c = elem c $ M.keys e

-- | get the component, throws exception, if component not present, or wrong type
(#) :: Typeable a => Entity -> Component -> a
e # c = fromJust $ M.lookup c e >>= fromDynamic

-- | get the component as an maybe, in case wrong type
(?#) :: Typeable a => Entity -> Component -> Maybe a
e ?# c = M.lookup c e >>= fromDynamic

-- | modification function, throws exception, if component not present
updateEntity :: Typeable a => Entity -> Component -> (a -> a) -> Entity
updateEntity e c f = M.insert c ((toDyn . f) (e # c)) e

-- | modification function, sets entity component, needed for events
_setComponent :: Typeable a => Entity -> Component -> a -> Entity
_setComponent e c val = M.insert c (toDyn val) e


-- References to Entities

-- besides Entity, we need atomic references to entities, we call them ERef
-- ERefs also have listeners for updates

-- Listener Map, for each k, manages a map of writers, writers geting the old and the new value after a change

type Listeners = TVar (M.Map Component [Entity -> Entity -> IO ()])

addListener :: Listeners -> Component -> (Entity -> Entity -> IO ()) -> IO ()
addListener tls c l = atomically $ do
            ls <- readTVar tls
            let l' = case M.lookup c ls of
                           Just ol -> (l:ol)
                           Nothing -> [l]
            writeTVar tls (M.insert c l' ls)

clearAllListeners :: Listeners -> IO ()
clearAllListeners tls = atomically $ do
              ls <- readTVar tls
              writeTVar tls (M.fromList [])
              return ()

fireListeners :: Listeners -> Component -> Entity -> Entity -> IO ()
fireListeners tls c val val' = do
              ls <- atomically $ readTVar tls
              case M.lookup c ls of
                   Just l -> mapM (\f -> f val val') l >> return ()
                   Nothing -> return ()

-- ERef, composable objects, referenced Entities with listeners

data ERef = ERef (TVar Entity) Listeners deriving (Eq)

newE :: [(Component, Dynamic)] -> IO ERef
newE inlist = do
     let e = entity (inlist ++ [CTCmd #: (), CTEvt #: ()])
     te <- newTVarIO e
     tl <- newTVarIO (M.fromList [])
     return $ ERef te tl

readE :: ERef -> IO Entity
readE (ERef te _) = atomically $ readTVar te

updateE :: Typeable a => ERef -> Component -> (a -> a) -> IO ()
updateE (ERef te tl) c f = do
        (val, val') <- atomically $ do
                   e <- readTVar te
                   let e' = updateEntity e c f
                   seq e' (writeTVar te e')
                   return (e, e')
        fireListeners tl c val val'
        return ()

_setE :: Typeable a => ERef -> Component -> a -> IO ()
_setE (ERef te tl) c val = do
        (eold, enew) <- atomically $ do
                   e <- readTVar te
                   let e' = _setComponent e c val
                   seq e' (writeTVar te e')
                   return (e, e')
        fireListeners tl c eold enew
        return ()

sendCmd :: Typeable a => ERef -> a -> IO ()
sendCmd eref cmd = _setE eref CTCmd cmd

sendEvt :: Typeable a => ERef -> a -> IO ()
sendEvt eref cmd = _setE eref CTEvt cmd

regEvtH :: Typeable a => ERef -> (a -> IO ()) -> IO ()
regEvtH (ERef te tl) h = addListener tl CTEvt (\_ e' -> case (e' ?# CTEvt) of
                                                             Just val -> h val
                                                             Nothing -> return () )

-- ComponentListener

-- | ComponentListener are tracking the change of a component of a specific entity. Ones this component changes, they contain the latest value of the entity. ComponentListener are implemented with the Listener mechanism for ERefs

type ComponentListener = (TVar (Maybe (Entity, Entity)))

componentListener :: ERef -> Component -> IO ComponentListener
componentListener er@(ERef te tl) c  = do
           tv <- newTVarIO Nothing
           let w e e' = do
                        seq e (atomically $ do
                                          v <- readTVar tv
                                          case v of
                                               Nothing -> writeTVar tv (Just (e, e'))
                                               Just (old, new) -> writeTVar tv (Just (old, e'))  -- need to keep old, if not queried in the meantime !
                                               )
                        return ()
           addListener tl c w
           return tv

queryComponentListener :: ComponentListener -> IO (Maybe (Entity, Entity))
queryComponentListener tv = do
            atomically $ do
                v <- readTVar tv
                writeTVar tv Nothing
                return v

-- System

{-
        A system reacts towards changes/updates in entities and their components. The system does it by creating for each component it handles an internal represenation, which it modifies upon changes to this component and to potentially additional changes in other components. For example an entity representing a light could have a Light component, which is created and modified upon change in the light component data. The entity may well have also a position component and the light entity is moved, in case the position component is modified.

        In general the system works by keeping a list of component listener on each added (each observes one component of an entity) and functions, which are called, in case the component listener exhibits a component change. 

-}

type OnUpdateFunction = Entity -> Entity -> IO ()
type OnDeleteFunction = IO ()
type SystemRecord = (ComponentListener, OnUpdateFunction, OnDeleteFunction)
type SystemFunction a = a -> ERef -> IO [SystemRecord]

data SystemData a = SystemData {
     sdLock :: MVar (),
     sdNewERefs :: IORef [ERef], 
     sdDelERefs :: IORef [ERef],
     sdRecords :: [SystemRecord],
     sdSystem :: a,
     sdSystemFunction :: SystemFunction a
}

class System a where

-- to be implemented by instances

      initializeSystem :: IO (SystemData a)
      stepSystem :: (SystemData a) -> IO Bool

-- to be called from outside the runloop

      addERef :: (SystemData a) -> ERef -> IO ()
      addERef sd eref = do
                let ref = sdNewERefs sd
                let lock = sdLock sd
                takeMVar lock
                nrefs <- readIORef ref
                writeIORef ref (eref : nrefs)
                putMVar lock ()
                return ()
                
      removeERef :: (SystemData a) -> ERef -> IO ()
      removeERef sd eref = do
                let ref = sdDelERefs sd
                let lock = sdLock sd
                takeMVar lock
                drefs <- readIORef ref
                writeIORef ref (eref : drefs)
                putMVar lock ()
                return ()

      runSystem :: GameTime -> IO (SystemData a)
      runSystem stepT = do
        mv <- newEmptyMVar
        forkOS $ (\mv' -> do
                     status <- initializeSystem 
                     putMVar mv' status
                     let runS s = do
                            nowT <- getTime
                            (s', qFlag) <- stepSystem' s
                            if qFlag then do
                              shutdownSystem s'
                              return ()
                              else do
                                nowT' <- getTime
                                let timeUsed = nowT' - nowT
                                if timeUsed < stepT then do
                                  threadDelay ((fromIntegral . usec) (stepT - timeUsed) )
                                  else do
                                    return ()
                                runS s'
                     runS status
                     ) mv
        status' <- takeMVar mv
        return status'

      -- called within the run loop

      shutdownSystem :: (SystemData a) -> IO ()
      shutdownSystem system = return ()

      stepSystem' :: (SystemData a) -> IO ((SystemData a), Bool)
      stepSystem' sd@(SystemData lock nrefs drefs records system systemfunction) = do
                 -- add and delete erefs
                 takeMVar lock
                 adds <- readIORef nrefs
                 writeIORef nrefs []
                 dels <- readIORef drefs
                 writeIORef drefs []
                 putMVar lock ()

                 -- add new instances
                 newRecords <- mapM ((sdSystemFunction sd)(sdSystem sd)) adds
                 let records' = (concat newRecords) ++ records

                 -- remove instances
                 -- to be done
                 let records'' = records'
                 
                 -- run stepfunction on tuples
                 let stepRecord (listener, updateF, deleteF) = do
                     me <- queryComponentListener listener
                     case me of
                          Just (e, e') -> updateF e e'
                          Nothing -> return ()
                 mapM stepRecord records''

                 -- run specific stepSystem
                 let newSD = (SystemData lock nrefs drefs records'' system systemfunction)
                 qFlag <- stepSystem newSD

                 -- return new values
                 return (newSD, qFlag) -- need to add quit condition here
                 

-- management of systems
--
        
data SomeSystem = forall a . System a => SomeSystem (SystemData a)

(#+) :: [SomeSystem] -> [SomeSystem] -> [SomeSystem]
vals #+ vals' = vals ++ vals'
infixr #+

-- ECS World functions, to manage entities in systems

addToWorld :: [SomeSystem] -> ERef -> IO ()
addToWorld systems e = mapM (f e) systems >> return () where
  f e (SomeSystem sd) = addERef sd e >> return ()

removeFromWorld :: [SomeSystem] -> ERef -> IO ()
removeFromWorld systems e = mapM (f e) systems >> return () where
  f e (SomeSystem sd) = removeERef sd e >> return ()
    

                          
