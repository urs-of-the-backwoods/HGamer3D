{-# Language StandaloneDeriving #-}
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

-- Entity.hs

-- | the Entity in Entity-Component-System

module HGamer3D.Engine.Internal.Entity
where

import HGamer3D.Engine.Internal.Component
import HGamer3D.Engine.Internal.ComponentType
import HGamer3D.Engine.Internal.Event

import Control.Concurrent.MVar
import qualified Data.Map as M
import Data.Maybe
import Data.Typeable
import System.Mem.StableName

type EntityId = StableName (M.Map ComponentType Component)
data Entity = Entity {
  entityCMap :: (M.Map ComponentType Component),
  entityId :: EntityId ,
  entityU2CEvents :: MVar [HG3DEvent],
  entityC2UEvents :: MVar [HG3DEvent]
}

--
-- pure Data functions, creating, traversing and getting components from entities
--

entity :: [(ComponentType, IO Component)] -> IO Entity
entity inList  = do
  outList <- mapM (\(c, val) -> do
                     val' <- val
                     return (c, val')) inList
  let cMap = M.fromList outList
  eId <- makeStableName cMap
  evtsU2C <- newMVar []
  evtsC2U <- newMVar []
  return $ Entity cMap eId evtsU2C evtsC2U

instance Eq Entity where
  e1 == e2 = (entityId e1) == (entityId e2)

idE :: Entity -> EntityId
idE (Entity _ eId _ _) = eId

sendEvent :: Entity -> HG3DEvent -> IO ()
sendEvent (Entity map _ evtsU2C _) event = do
  evts <- takeMVar evtsU2C
  putMVar evtsU2C (evts ++ [event])

_popEntityU2CEvents :: Entity -> IO [HG3DEvent]
_popEntityU2CEvents (Entity map _ evtsU2C _) = do
  evts <- takeMVar evtsU2C
  putMVar evtsU2C []
  return evts

receiveEvents :: Entity -> IO [HG3DEvent]
receiveEvents (Entity map _ _ evtsC2U) = do
  evts <- takeMVar evtsC2U
  putMVar evtsC2U []
  return evts

_pushEntityC2UEvents :: Entity -> [HG3DEvent] -> IO ()
_pushEntityC2UEvents (Entity map _ _ evtsC2U) events = do
  evts <- takeMVar evtsC2U
  putMVar evtsC2U (evts ++ events)

(#:) :: Typeable a => ComponentType -> a -> (ComponentType, IO Component)
(#:) ec val = (ec, newC val)

(#?) :: Entity -> ComponentType -> Maybe Component
(Entity map _ _ _) #? name = M.lookup name map
infix 8 #?

(#) :: Entity -> ComponentType -> Component
e # name = fromJust (e #? name)
infix 8 #



