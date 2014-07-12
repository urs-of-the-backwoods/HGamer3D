{-# Language StandaloneDeriving, DeriveDataTypeable, DeriveGeneric, PatternGuards #-}
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

-- Component.hs

-- | the Component in Entity-Component-System

module HGamer3D.Engine.Internal.Component
where

import Data.Dynamic
import Data.Typeable
import Control.Concurrent.MVar
import System.Mem.StableName
import GHC.Generics (Generic)
import Data.Hashable
import System.Clock

import HGamer3D.Engine.Internal.Event

deriving instance Generic TimeSpec
instance Hashable TimeSpec

data StampedValue a = SV a Int
     deriving (Typeable)

instance Eq a => Eq (StampedValue a) where
  (==) (SV val1 h1) (SV val2 h2) = if h1 == h2 then True else val1 == val2

_stamp :: a -> IO (StampedValue a)
_stamp val = do
  tstamp <- (getTime Monotonic >>= return . hash)
  return $ SV val tstamp

fromStamped :: StampedValue a -> a
fromStamped (SV val _) = val

type ComponentId = StableName Dynamic

data Component = Component {
  componentData :: Dynamic,
  componentId :: ComponentId,
  componentType :: TypeRep,
  componentEvents :: MVar [HG3DEvent]
} 

instance Eq Component where
    c1 == c2 = (idC c1) == (idC c2)

newC :: Typeable a => a -> IO Component
newC val = do
  tval <- _stamp val
  mv <- newMVar tval
  let dyn = toDyn mv
  ident <- makeStableName dyn
  evts <- newMVar []
  return (Component dyn ident (typeOf val) evts)

readC :: Typeable a => Component -> IO (Maybe (StampedValue a))
readC (Component dyn _ _ _)
    | Just mv  <- fromDynamic dyn = do
                    val <- takeMVar mv
                    putMVar mv val
                    return $ Just val
    | otherwise = do
              return Nothing

isTypeC :: Typeable a => a -> Component -> Bool
isTypeC val (Component dyn _ tr _) = (typeOf val == tr)
  
updateC :: Typeable a => Component -> (a -> a) -> IO ()
updateC (Component dyn _ _ _) f 
    | Just mv  <- fromDynamic dyn = do
                    val <- (takeMVar mv >>= (return . fromStamped))
                    newTVal <- _stamp (f val)
                    putMVar mv newTVal
                    return ()
    | otherwise = do
              return ()
    
idC :: Component -> ComponentId
idC (Component _ ident _ _) = ident

_pushEvent :: Component -> HG3DEvent -> IO ()
_pushEvent c evt = do
  let mv = (componentEvents c)
  evts <- takeMVar mv
  putMVar mv (evts ++ [evt])

_popEvents :: Component -> IO [HG3DEvent]
_popEvents c = do
  let mv = (componentEvents c)
  evts <- takeMVar mv
  putMVar mv []
  return evts

 
