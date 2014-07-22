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

-- HGamer3D/Audio/Internal/SystemAudio.hs


module HGamer3D.Audio.Internal.SystemAudio

where

import Control.Concurrent.MVar
import qualified Data.Map as M
import Data.Maybe
import Data.Typeable
import Data.Dynamic
import Data.Hashable
import qualified Data.HashTable.IO as HT

import qualified HGamer3D.Data as D

import HGamer3D.Engine.Internal.Event
import HGamer3D.Engine.Internal.Entity
import HGamer3D.Engine.Internal.Component
import HGamer3D.Engine.Internal.ComponentType
import HGamer3D.Engine.Internal.System

import qualified HGamer3D.Audio.Internal.Base as A
import qualified HGamer3D.Audio.Schema.AudioSource as AS
import qualified HGamer3D.Audio.Schema.AudioListener as AL


-- | the Audio System of the Entity-Component-System World
data ECSAudio = ECSAudio {
  audioSlots :: ListAndCache AS.AudioSlots A.AudioSlots,
  positions :: ListAndCache D.Position ()
  }

_handleEvent aslots evt = do
      let (A.AudioSlots map schema) = aslots
      case evt of
        AudioEvt (PlaySound slot) -> do
          let mAS = (M.lookup slot map)
          case mAS of
            Just as -> A.playAudioSource as
            Nothing -> return ()
        AudioEvt (StopSound slot) -> do
          let mAS = (M.lookup slot map)
          case mAS of
            Just as -> A.stopAudioSource as
            Nothing -> return ()

instance System ECSAudio where

    addEntity system entity = do
      lacAdd entity (audioSlots system)
      lacAdd entity (positions system)
      return system

    removeEntity system entity = do
      lacRemove entity (audioSlots system)
      lacRemove entity (positions system)
      return system

    initializeSystem = do
      audioSlots <- lacInitialize CTASl
      positions <- lacInitialize CTPos
      return $ (ECSAudio audioSlots positions)

    stepSystem system = do
      let handleUserEvents evts aslots = do
            mapM (_handleEvent aslots) evts
            return ()
      lacApplyChanges (audioSlots system) A.audioSlots A.updateAudioSlots A.removeAudioSlots handleUserEvents lacHandleC2UEvents
      let update' pos edata schema = D.positionTo edata pos 
      lacApplyOtherChanges (positions system) (audioSlots system) update'
      return (system, False)
      
    shutdownSystem system = return ()

runSystemAudio :: D.GameTime -> IO ECSAudio
runSystemAudio sleepT = runSystem sleepT


