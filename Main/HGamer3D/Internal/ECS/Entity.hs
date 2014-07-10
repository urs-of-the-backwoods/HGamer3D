{-# Language StandaloneDeriving #-}

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

module HGamer3D.Internal.ECS.Entity

(
  -- * Entity
  Entity,
  sendEvent,

  entity,
  (#:),
  (#?),
  (#)
  
)

where

import HGamer3D.Internal.ECS.ComponentType
import HGamer3D.Internal.Event

import Control.Concurrent.MVar
import HGamer3D.Internal.ECS.Component
import qualified Data.Map as M
import Data.Maybe
import Data.Typeable

data Entity = Entity (M.Map ComponentType Component) 

--
-- pure Data functions, creating, traversing and getting components from entities
--

entity :: [(ComponentType, IO Component)] -> IO Entity
entity inList  = do
  outList <- mapM (\(c, val) -> do
                     val' <- val
                     return (c, val')) inList
  return $ Entity (M.fromList outList)

sendEvent :: Entity -> HG3DEvent -> IO ()
sendEvent (Entity map) event = mapM (\(ct, c) -> _pushEvent c event) (M.toList map) >> return ()

(#:) :: Typeable a => ComponentType -> a -> (ComponentType, IO Component)
(#:) ec val = (ec, newC val)

(#?) :: Entity -> ComponentType -> Maybe Component
(Entity map) #? name = M.lookup name map
infix 8 #?

(#) :: Entity -> ComponentType -> Component
e # name = fromJust (e #? name)
infix 8 #



