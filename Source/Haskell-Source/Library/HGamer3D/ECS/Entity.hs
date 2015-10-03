-- This source file is part of HGamer3D
-- (A project to enable 3D game development in Haskell)
-- For the latest info, see http://www.althainz.de/HGamer3D.html
--
-- (c) 2015 Peter Althainz
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

-- HGamer3D/ECS/Entity

{-# Language ExistentialQuantification #-}

-- | Entity of the Entity ComponentType System for HGamer3D
module HGamer3D.ECS.Entity (

-- * EntityData Type
--   Entities are a kind of simplified extensible record system. They are basically a Map from ComponentType (64 bit id) to a data item with 
--   ComponentClass Typeclass. Basic entities are non-mutable but their exists the entity reference.

  EntityData,
  (#:),
  (#!),
  (#),

-- * Entity Type
--   The ERef type, which puts an EntityData into
--   an IORef and serves as mutable data structure.
--   In HGamer3D those ERefs are also used as thread-safe communication vehicle towards the C/C++ implementation of multimedia functionality.

  Entity (..),
  newE,
  readE,
  readC,
  updateC,
  setC,
  _setC',

-- * Listener
-- Mechanism to register listener on ComponentTypes of ERef

  addListener,
  clearListeners

)
where

import Data.Maybe
import Data.ByteString
--import Data.Dynamic
--import Data.Typeable
import qualified Data.Map as M
import Data.IORef

import Control.Concurrent
import Control.Applicative

import Foreign
import Foreign.C

import HGamer3D.Binding
import HGamer3D.Data 

-- | EntityData, a simple non-mutable record type, implemented as Map
type EntityData = M.Map Word64 Component

-- | pair builder for nice construction syntax, allows [ ct #: val, ...] syntax
(#:) :: ComponentClass a => ComponentType a -> a -> (Word64, Component)
(ComponentType c) #: val = (c, toMsg val)

-- | Builder for entities, allows newE = entity [ct #: val, ...] syntax
entityData :: [(Word64, Component)] -> EntityData
entityData clist = M.fromList clist

-- | does the entity have the ComponentType
(#?) :: EntityData -> ComponentType a -> Bool
e #? (ComponentType c) = Prelude.elem c $ M.keys e

-- | get the ComponentType, throws exception, if ComponentType not present
(#!) :: ComponentClass a => EntityData -> ComponentType a -> a
e #! (ComponentType c) = fromJust $ M.lookup c e >>= fromMsg

-- | get the ComponentType as an maybe, in case wrong type
(#) :: ComponentClass a => EntityData -> ComponentType a -> Maybe a
e # (ComponentType c) = M.lookup c e >>= fromMsg

-- | modification function, throws exception, if ComponentType not present
updateDataC :: ComponentClass a => EntityData -> ComponentType a -> (a -> a) -> EntityData
updateDataC e c'@(ComponentType c) f = M.insert c ((toMsg . f) (e #! c')) e

-- | modification function, sets entity ComponentType, needed for events
setDataC :: ComponentClass a => EntityData -> ComponentType a -> a -> EntityData
setDataC e (ComponentType c) val = M.insert c (toMsg val) e


-- References to Entities

-- besides Entity, we need atomic references to entities, we call them ERef
-- ERefs also have listeners for updates

-- Listener Map, for each k, manages a map of writers, writers geting the old and the new value after a change

type Listeners = IORef (M.Map Word64 [EntityData -> EntityData -> IO ()])

-- | ERef, composable objects, referenced Entities with listeners
data Entity = Entity (IORef EntityData) Listeners deriving (Eq)

-- | Add an action (IO function), which will be executed when value of ComponentType is changed
addListener :: Entity -> ComponentType a -> (EntityData -> EntityData -> IO ()) -> IO ()
addListener (Entity _ tls) (ComponentType c) l = atomicModifyIORef tls (\m -> let
            l' = case M.lookup c m of
                           Just ol -> ol ++ [l]
                           Nothing -> [l]
            in (M.insert c l' m, ()))

-- | Clear all listeners from Entity
clearListeners :: Entity -> IO ()
clearListeners (Entity _ tls) = atomicWriteIORef tls (M.fromList [])

fireListeners :: Entity -> ComponentType a -> EntityData -> EntityData -> IO ()
fireListeners (Entity _ tls) (ComponentType c) val val' = do
              ls <- readIORef tls
              case M.lookup c ls of
                   Just l -> mapM (\f -> f val val') l >> return ()
                   Nothing -> return ()

-- | creates an Entity
newE :: [(Word64, Component)] -> IO Entity
newE inlist = do
     let e = entityData inlist
     te <- newIORef e
     tl <- newIORef (M.fromList [])
     return $ Entity te tl

-- | reads the EntityData from an Entity
readE :: Entity -> IO EntityData
readE (Entity te _) = readIORef te

-- | reads one ComponentType, throws exception, if ComponentType not present, or wrong type
readC :: ComponentClass a => Entity -> ComponentType a -> IO a
readC er c = readE er >>= \e -> return (e #! c)

-- | updates one ComponentType
updateC :: ComponentClass a => Entity -> ComponentType a -> (a -> a) -> IO ()
updateC er@(Entity te tl) c f = do
        (e, e') <- atomicModifyIORef te (\olde -> let
                    newe = updateDataC olde c f
                    in (newe, (olde, newe)))
        fireListeners er c e e'
        return ()

-- | sets one ComponentType
setC :: ComponentClass a => Entity -> ComponentType a -> a -> IO ()
setC er@(Entity te tl) c val = do
        (e, e') <- atomicModifyIORef te (\olde -> let
                    newe = setDataC olde c val
                    in (newe, (olde, newe)))
        fireListeners er c e e'
        return ()

-- | sets one ComponentType as Component
_setC' :: Entity -> Word64 -> Component -> IO ()
_setC' er@(Entity te tls) c val = do
        (e, e') <- atomicModifyIORef te (\olde -> let
                    newe = M.insert c val olde
                    in (newe, (olde, newe)))
        ls <- readIORef tls
        case M.lookup c ls of
             Just l -> mapM (\f -> f e e') l >> return ()
             Nothing -> return ()
        return ()

