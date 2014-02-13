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

module GameWire where

import Control.Wire as W
import Control.Wire.Unsafe.Event as U

import Prelude hiding ((.), id)

import HGamer3D.Audio
import HGamer3D.InputSystem

-- TYPES

type Time = Double
type RunState = (Timed NominalDiffTime ())
type GameWire = Wire RunState () IO





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

