{-# LANGUAGE Arrows #-}

-- Cuboid2, a 3D puzzle game, thanks to Pedro Martins for game idea (https://github.com/pedromartins/cuboid)
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

module EntityComponentSystem where

import Control.Wire as W
import Control.Wire.Unsafe.Event as U

import Prelude hiding ((.), id)
import Data.List
import Data.IORef
import qualified Data.Map as M

import HGamer3D

import GameWire

type CommandWire = GameWire (W.Event Command) (W.Event Command)

-- Commands
-----------

-- all commands go into one type (for now)
data Command = CmdArr [Command]
               | CmdAddEntity Entity 
               | CmdRemEntity ObId
               | CmdMoveTo Vec3 Time
               | CmdNoOp
                 
               
-- a wire which deconstructs a CmdArr into a sequence of single events
-- single commands will be ignored and not added to the list
-- to be added into a chain of command senders, receivers
cmdFifo :: CommandWire
cmdFifo = fifo_ [] where
  fifo_ list = mkPureN (\cmdIn -> let
                          newList = case cmdIn of
                            (U.Event (CmdArr addList)) -> list ++ addList
                            _ -> list    -- do not add single command here, intentionally, this will create an infinite loop in a chain
                          (evtOut, listOut) = case newList of
                            [] -> (U.NoEvent,[])
                            (e:es) -> (U.Event e, es)
                          in (Right evtOut, fifo_ listOut) )
    
-- Entity Component System
--------------------------

-- this is a prototype implementation to show the potential
-- IORef's are solely used in the system data aka components of the engine
-- (reading some ECS literature, I thought implementing components as pure FRP might be a big hassle)
  
-- object ids will be simple string
type ObId = String

-- an entity has an object id and components
data Entity = Entity { entId :: ObId, entComps :: [Component] } 

-- SYSTEMS

data SystemData d = SystemData {
  sdInitialize :: d,
  sdAddEntity :: Entity -> d -> d,
  sdRemoveEntity :: ObId -> d -> d,
  sdRunAction :: d -> RunState -> IO ()  -- only side effects on the IORef data, but complete set as input!
  }
                
createSystem :: SystemData d -> CommandWire
createSystem sdata  = let
  
  (SystemData initD addE remE runAction) = sdata
  dOut dIn evt = case evt of
    (U.Event (CmdAddEntity ent)) -> (addE ent dIn)
    (U.Event (CmdRemEntity oid)) -> (remE oid dIn)
    _ -> dIn
  wire dIn = mkGen (\s evt -> do 
                       let out = dOut dIn evt
                       runAction out s
                       return (Right evt, wire out) )
  in wire initD
          
-- COMPONENTS
                    
modifyIORef' :: IORef a -> (a -> a) -> IO ()
modifyIORef' ref f = do
    x <- readIORef ref
    let x' =  (f x)
    x' `seq` writeIORef ref x'

-- a component holds the "data", a state with an identity, there are different types of components
-- each component comes with its onwn type of system
data Component = Location (IORef Vec3) 
                 | Orientation (IORef UnitQuaternion)
                 | GObj Object3D
                 | Velocity (IORef Vec3)                                             
                   
-- LOCATION                    

location :: Component -> Bool
location (Location _) = True
location _ = False

getLocation :: Entity -> IO (Maybe Vec3)
getLocation e = case e `gC` location of
    Just (Location l) -> do
      val <- readIORef l 
      return $ seq val (Just val)
    _ -> return Nothing
  
setLocation :: Entity -> (Vec3 -> Vec3) -> IO ()
setLocation e f = do
  case e `gC` location of
    Just (Location l) -> do
      modifyIORef' l f
      return ()
    _ -> return ()
  
locationSystem :: SystemData (M.Map ObId (IORef Vec3, Object3D))
locationSystem = let
  fInit = M.fromList [] :: M.Map ObId (IORef Vec3, Object3D)
  fAdd e mapIn = case (e `gC` location, e `gC` gObj) of
    (Just (Location ref), Just (GObj o3d)) -> M.insert (entId e) (ref, o3d) mapIn
    _ -> mapIn
  fRem = M.delete
  runAction mapIn _ = do
    mapM (\(ref, obj3D) -> do
             pos <- readIORef ref
             positionTo3D obj3D pos ) (map snd (M.toList mapIn))
    return ()
  in (SystemData fInit fAdd fRem runAction)


-- move position of Entity by a vector (per second)
move :: Entity -> Vec3 -> GameWire (W.Event a) (W.Event a)
move e vec = mkGen $ \s evt -> case evt of
    U.Event _ -> do
      let t = realToFrac (dtime s) 
      setLocation e (\pos -> pos &+ (vec &* t))
      return $ (Right evt, move e vec)
    _ -> return (Right evt, move e vec)
 

-- ORIENTATION

orientation :: Component -> Bool
orientation (Orientation _) = True
orientation _ = False

getOrientation :: Entity -> IO (Maybe U)
getOrientation e = case e `gC` orientation of
    Just (Orientation o) -> do
      val <- readIORef o 
      return $ seq val (Just val)
    _ -> return Nothing
  
setOrientation :: Entity -> (U -> U) -> IO ()
setOrientation e f = do
  case e `gC` orientation of
    Just (Orientation o) -> modifyIORef' o f
    _ -> return ()
  
orientationSystem :: SystemData (M.Map ObId (IORef UnitQuaternion, Object3D))
orientationSystem = let
  fInit = M.fromList [] :: M.Map ObId (IORef UnitQuaternion, Object3D)
  fAdd e mapIn = case (e `gC` orientation, e `gC` gObj) of
    (Just (Orientation ref), Just (GObj o3d)) -> M.insert (entId e) (ref, o3d) mapIn
    _ -> mapIn
  fRem = M.delete
  runAction mapIn _ = do
    mapM (\(ref, obj3D) -> do
             ori <- readIORef ref
             orientationTo3D obj3D ori) (map snd (M.toList mapIn)) 
    return ()
  in (SystemData fInit fAdd fRem runAction)

rotate :: Entity -> Vec3 -> Float -> GameWire (W.Event a) (W.Event a)
rotate e rv a = mkGen $ \s evt -> case (evt, (e `gC` orientation)) of
  (U.Event _, Just (Orientation r)) -> do
    let t = realToFrac (dtime s) 
    modifyIORef' r (\ori -> ori .*. (rotU rv (a*t))) 
    return $ (Right evt, rotate e rv a)  
  _ -> return $ (Right evt, rotate e rv a)


-- Object3D COMPONENT
  
gObj :: Component -> Bool
gObj (GObj _) = True
gObj _ = False

gC :: Entity -> (Component -> Bool) -> Maybe Component
gC e f = find f (entComps e)

-- VELOCITY  
  
velocity :: Component -> Bool
velocity (Velocity _) = True
velocity _ = False

setVelocity :: Entity -> (Vec3 -> Vec3) -> IO ()
setVelocity e f = do
  case e `gC` velocity of
    Just (Velocity v) -> modifyIORef' v f
    _ -> return ()
  
velocitySystem :: SystemData (M.Map ObId (IORef Vec3, IORef Vec3))
velocitySystem = let
  fInit = M.fromList [] :: M.Map ObId (IORef Vec3, IORef Vec3)
  fAdd e mapIn = case (e `gC` velocity, e `gC` location) of
    (Just (Velocity rV), Just (Location rP)) -> M.insert (entId e) (rV, rP) mapIn
    _ -> mapIn
  fRem = M.delete
  runAction mapIn s = do
    mapM (\(rV, rP) -> do
             vel <- readIORef rV
             let t = realToFrac $ dtime s :: Float
             modifyIORef' rP (\pos -> pos &+ (vel &* t)) ) (map snd (M.toList mapIn)) 
    return ()
  in (SystemData fInit fAdd fRem runAction)

accelerate :: Entity -> Vec3 -> GameWire (W.Event a) (W.Event a)
accelerate e vec = mkGen_ $ \evt -> case (evt, (e `gC` velocity)) of
  (U.Event _, Just (Velocity r)) -> do
    modifyIORef' r (\vel -> vel &+ vec) 
    return $ (Right evt)  
  _ -> return $ (Right evt)


-- move position to target within specific time 
moveTo :: Entity -> Vec3 -> Float -> GameWire a ()
moveTo e target dt = let  
  startAction = do
    mLoc <- getLocation e
    case mLoc of 
      Just loc -> do
          let vel = (target &- loc) &* (1.0 / dt)
          setVelocity e (\_ -> vel)
          return $ Left ()
      _ -> return $ Left ()
  endAction = do
    setVelocity e (\_ -> (Vec3 0.0 0.0 0.0))
    setLocation e (\_ -> target)
    return $ Left ()
  in  (mkGen_ (\_ -> startAction)) --> for (realToFrac dt) . pure () --> (mkGen_ (\_ -> endAction)) 

-- create entity with a graphics object, a location and a velocity
veloEntity :: String -> Object3D -> Vec3 -> IO Entity
veloEntity name obj pos = do
  ref <- newIORef pos
  vel <- newIORef (Vec3 0.0 0.0 0.0)
  let e = Entity name [
        Location ref,
        Velocity vel,
        GObj obj
        ]
  return e
    
-- create entity with a graphics object and a rotation, only
rotEntity :: String -> Object3D -> UnitQuaternion -> IO Entity
rotEntity name obj ori = do
  ref <- newIORef ori
  let e = Entity name [
        Orientation ref,
        GObj obj
        ]
  return e

