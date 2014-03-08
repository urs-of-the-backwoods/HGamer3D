{-# LANGUAGE Arrows #-}

-- Some useful wires for game programming
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

module HGamer3D.Wire.ECSWire where

import Control.Wire as W
import Control.Wire.Unsafe.Event as U

import Prelude hiding ((.), id)

import HGamer3D
import HGamer3D.Audio
import HGamer3D.InputSystem

import HGamer3D.Wire.Types
import HGamer3D.Wire.EntityComponentSystem

import Control.Monad.Trans.Maybe

-- ECS Wires

-- move position of Entity by a vector (per second)
move :: Entity -> Vec3 -> GameWire (Event a) (Event a)
move e vec = mkGen $ \s evt -> case evt of
    Event _ -> do
      let t = realToFrac (dtime s) 
      runMaybeT $ setLocation e (\pos -> pos &+ (vec &* t))
      return (Right evt, move e vec)
    _ -> return (Right evt, move e vec)
 
rotate :: Entity -> Vec3 -> Float -> GameWire (Event a) (Event a)
rotate e rv a = mkGen $ \s evt -> case evt of
  Event _ -> do
    let t = realToFrac (dtime s) 
    runMaybeT $ setOrientation e (\ori -> ori .*. (rotU rv (a*t))) 
    return (Right evt, rotate e rv a)
  _ -> return (Right evt, rotate e rv a)

accelerate :: Entity -> Vec3 -> GameWire (Event a) (Event a)
accelerate e vec = mkGen $ \s evt -> case evt of
  Event _ -> do
    runMaybeT $ setVelocity e (\vel -> vel &+ vec) 
    return $ (Right evt, accelerate e vec)  
  _ -> return $ (Right evt, accelerate e vec)


-- move position to target within specific time 
moveTo :: Entity -> Vec3 -> Float -> GameWire a ()
moveTo e target dt = let  
  startAction = do 
    runMaybeT (do
                  loc <- getLocation e
                  let vel = (target &- loc) &* (1.0 / dt)
                  setVelocity e (\_ -> vel)
                  return () ) 
    return $ Left ()
  endAction = do 
    runMaybeT (do
                  setVelocity e (\_ -> (Vec3 0.0 0.0 0.0))
                  setLocation e (\_ -> target)
                  return () ) 
    return $ Left ()
  in  (mkGen_ (\_ -> startAction)) --> for (realToFrac dt) . pure () --> (mkGen_ (\_ -> endAction)) 


-- USEFUL WIRES

-- sends the event one time and then inhibit !

sendOnce :: GameWire (W.Event a) (W.Event a)
sendOnce = proc inevt -> do
  rec
    devt <- delay (U.NoEvent) -< devt'
    devt' <- id -< inevt 
  case devt of
      U.NoEvent -> do
        returnA -< inevt
      U.Event evt -> do
        x <- pure U.NoEvent . mkEmpty -< ()
        returnA -< x
    
-- wire to play a sound
soundW :: AudioSource -> Wire s e IO (W.Event a) (W.Event a)
soundW aSource = onEventM (\aIn -> do
   playAudioSource aSource
   return aIn )
                 
-- samples generated keypress
keyW :: EnumKey -> Wire s e IO a (W.Event a)
keyW key = mkGen_ ( \inVal -> do
                       press <- isKeyPressed key
                       let evt = if press then U.Event inVal else U.NoEvent
                       return $ (Right evt) )

-- Cycles through a list, stop at end
cycleW :: [b] -> Wire s () IO (W.Event a) (W.Event b)
cycleW lin = if length lin > 0 then
               mkPureN (\evt -> case evt of
                        U.Event _ -> (Right (U.Event (head lin)), cycleW (tail lin))
                        _ -> (Right U.NoEvent, cycleW lin) )
               else
                 mkConst $ Left ()
               

printEvt :: Show a => GameWire (W.Event a) (W.Event a) 
printEvt = mkGen_ (\evt -> do case evt of 
                                U.Event x -> do
                                             print x
                                             return (Right evt)
                                _ -> return (Right evt))


-- generic switch, in event is two-fold, left side creates new wire of a -> b
-- right side is a output is b, runs sequentially, first do new wire switched in, then restart switch logic

-- different types of switching logic, eases up game programming
----------------------------------------------------------------

-- this wire creates a wire from a function which creates a wire from an input status
-- the resulting wire switches into the created wire, runs this until it inihibits and then
-- switches back to the switchIntoAndRun semantics

-- useful for providing game states, which run for a certain amount of time and then wait for a new
-- initializer, during running the created wire additional state input is ignored

switchIntoAndRunW :: (gst -> GameWire (Event a) (Event b)) -> GameWire (Event gst, Event a) (Event b)
switchIntoAndRunW inWireF = switch $ mkPure_ (\(gsEvt, aEvt) -> case gsEvt of
              Event gs -> let
                newWire = (inWireF gs) . mkPure_ (\(a, b) -> Right b) --> switchIntoAndRunW inWireF
                in Right (NoEvent, Event (newWire))
              _ -> Right (NoEvent, NoEvent) )

-- same logic, but does not need to process events during run phase
-- usefule if something needs to happen, but no input processing
switchIntoAndRunW' :: (gst -> GameWire () (Event b)) -> GameWire (Event gst) (Event b)
switchIntoAndRunW' inWireF = switch $ mkPure_ (\gsEvt -> case gsEvt of
              Event gs -> let
                newWire = (inWireF gs) . pure () --> switchIntoAndRunW' inWireF
                in Right (NoEvent, Event (newWire))
              _ -> Right (NoEvent, NoEvent) )
                             
  

