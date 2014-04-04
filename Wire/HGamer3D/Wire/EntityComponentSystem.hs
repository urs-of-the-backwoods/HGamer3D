{-# LANGUAGE Arrows #-}

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

module HGamer3D.Wire.EntityComponentSystem where

import HGamer3D.Wire.Types

import HGamer3D  
import HGamer3D.Audio
import HGamer3D.InputSystem

import Control.Monad.Trans
import Control.Monad.Trans.Maybe

import Control.Wire
import Control.Wire.Unsafe.Event

import Prelude hiding ((.), id)
import Data.List
import Data.IORef
import Data.Dynamic
import Data.Maybe
import qualified Data.Map as M

createSystem :: SystemData d -> GameWire (Event Command) (Event Command)
createSystem sdata  = let
  
  (SystemData initD addE remE runAction) = sdata
  dOut dIn evt = case evt of
    (Event (CmdAddEntity ent)) -> (addE ent dIn)
    (Event (CmdRemEntity oid)) -> (remE oid dIn)
    _ -> dIn
  wire dIn = mkGen (\s evt -> do 
                       let out = dOut dIn evt
                       runAction out s
                       return (Right evt, wire out) )
  in wire initD

-- a wire which deconstructs a CmdArr into a sequence of single events
-- single commands will be ignored and not added to the list
-- to be added into a chain of command senders, receivers
cmdFifo :: GameWire (Event Command) (Event Command)
cmdFifo = fifo_ [] where
  fifo_ list = mkPureN (\cmdIn -> let
                          newList = case cmdIn of
                            (Event (CmdArr addList)) -> list ++ addList
                            _ -> list    -- do not add single command here, intentionally, this will create an infinite loop in a chain
                          (evtOut, listOut) = case newList of
                            [] -> (NoEvent,[])
                            (e:es) -> (Event e, es)
                          in (Right evtOut, fifo_ listOut) )

readIORef' :: IORef a -> MaybeT IO a
readIORef' ref = MaybeT $ do 
  val <- readIORef ref
  return $ Just val
                             
writeIORef' :: IORef a -> a -> MaybeT IO ()
writeIORef' ref val = MaybeT $ do 
  writeIORef ref val
  return $ Just ()

modifyIORef'' :: IORef a -> (a -> a) -> MaybeT IO ()
modifyIORef'' ref f = MaybeT $ do
    x <- readIORef ref
    let x' =  (f x)
    x' `seq` writeIORef ref x'
    return $ Just ()

getC :: Typeable a => Entity -> Maybe a
getC entity = case find isJust (map fromDynamic (entComps entity)) of
                  Just (Just c) -> Just c
                  _ -> Nothing

getC' :: Typeable a => Entity -> MaybeT IO a
getC' entity = MaybeT $ 
  do
    let rval = getC entity
    return rval                
  
-- LOCATION                    

getLocation :: Entity -> MaybeT IO Vec3 
getLocation e = do
  LocationC l <- getC' e
  val <- readIORef' l
  return  val
  
setLocation :: Entity -> (Vec3 -> Vec3) -> MaybeT IO ()
setLocation e f = do
  LocationC l <- getC' e
  modifyIORef'' l f
  return ()
  
locationSystem :: SystemData (M.Map ObId (IORef Vec3, Object3D))
locationSystem = let
  fInit = M.fromList [] :: M.Map ObId (IORef Vec3, Object3D)
  fAdd e mapIn = case (getC e, getC e) of
    (Just (LocationC ref), Just (ObjectC o3d)) -> M.insert (entId e) (ref, o3d) mapIn
    _ -> mapIn
  fRem = M.delete
  runAction mapIn _ = do
    mapM (\(ref, obj3D) -> do
             pos <- readIORef ref
             positionTo3D obj3D pos ) (map snd (M.toList mapIn))
    return ()
  in (SystemData fInit fAdd fRem runAction)

-- ORIENTATION

getOrientation :: Entity -> MaybeT IO U
getOrientation e = do
    OrientationC o <- getC' e     
    val <- readIORef' o 
    return $ val
  
setOrientation :: Entity -> (U -> U) -> MaybeT IO ()
setOrientation e f = do
    OrientationC o <- getC' e     
    modifyIORef'' o f
    return ()
  
orientationSystem :: SystemData (M.Map ObId (IORef UnitQuaternion, Object3D))
orientationSystem = let
  fInit = M.fromList [] :: M.Map ObId (IORef UnitQuaternion, Object3D)
  fAdd e mapIn = case (getC e, getC e) of
    (Just (OrientationC ref), Just (ObjectC o3d)) -> M.insert (entId e) (ref, o3d) mapIn
    _ -> mapIn
  fRem = M.delete
  runAction mapIn _ = do
    mapM (\(ref, obj3D) -> do
             ori <- readIORef ref
             orientationTo3D obj3D ori) (map snd (M.toList mapIn)) 
    return ()
  in (SystemData fInit fAdd fRem runAction)

-- VELOCITY  
  
getVelocity :: Entity -> MaybeT IO Vec3
getVelocity e = do
    VelocityC v <- getC' e     
    val <- readIORef' v
    return $ val
  
setVelocity :: Entity -> (Vec3 -> Vec3) -> MaybeT IO ()
setVelocity e f = do
    VelocityC v <- getC' e     
    modifyIORef'' v f
    return ()
  
velocitySystem :: SystemData (M.Map ObId (IORef Vec3, IORef Vec3))
velocitySystem = let
  fInit = M.fromList [] :: M.Map ObId (IORef Vec3, IORef Vec3)
  fAdd e mapIn = case (getC e, getC e) of
    (Just (VelocityC rV), Just (LocationC rP)) -> M.insert (entId e) (rV, rP) mapIn
    _ -> mapIn
  fRem = M.delete
  runAction mapIn s = do
    mapM (\(rV, rP) -> do
             vel <- readIORef rV
             let t = realToFrac $ dtime s :: Float
             runMaybeT (modifyIORef'' rP (\pos -> pos &+ (vel &* t))) ) (map snd (M.toList mapIn)) 
    return ()
  in (SystemData fInit fAdd fRem runAction)


-- ENTITY CREATION

-- create entity with a graphics object, a location and a velocity
veloEntity :: String -> Object3D -> Vec3 -> IO Entity
veloEntity name obj pos = do
  ref <- newIORef pos
  vel <- newIORef (Vec3 0.0 0.0 0.0)
  let e = Entity name [
        toDyn (LocationC ref),
        toDyn (VelocityC vel),
        toDyn (ObjectC obj)
        ]
  return e
    
-- create entity with a graphics object and a rotation, only
rotEntity :: String -> Object3D -> UnitQuaternion -> IO Entity
rotEntity name obj ori = do
  ref <- newIORef ori
  let e = Entity name [
        toDyn (OrientationC ref),
        toDyn (ObjectC obj)
        ]
  return e
