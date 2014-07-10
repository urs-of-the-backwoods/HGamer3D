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

-- HGamer3D/AudioModule/Internal/SystemAudio.hs


module HGamer3D.AudioModule.Internal.SystemAudio

where

import Control.Concurrent.MVar
import qualified Data.Map as M
import Data.Maybe
import Data.Typeable
import Data.Dynamic
import Data.Hashable
import qualified Data.HashTable.IO as HT

import qualified HGamer3D.Data as D

import HGamer3D.Internal.Event
import HGamer3D.Internal.ECS.Entity
import HGamer3D.Internal.ECS.Component
import HGamer3D.Internal.ECS.ComponentType
import HGamer3D.Internal.ECS.System

import qualified HGamer3D.Audio.Internal.Base as A
import qualified HGamer3D.Audio.Schema.AudioSource as AS
import qualified HGamer3D.Audio.Schema.AudioListener as AL

type IdHashTable v = HT.BasicHashTable ComponentId v

-- | the Audio System of the Entity-Component-System World
data ECSAudio = ECSAudio {
  audioSlots :: MVar [(Component, Maybe Component)], -- AudioSlots, Position
  audioSlotsCache :: IdHashTable (
     A.AudioSlots,
     StampedValue AS.AudioSlots,
     Maybe (StampedValue D.Position)
     )
  }

_handleEvent map evt = do
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
      oldList <- takeMVar (audioSlots system)
      let mCom = entity #? CTASl
      let newList = case mCom of
            Just com -> let
              p = entity #? CTPos
              in ((com, p) : oldList)
            Nothing -> oldList
      putMVar (audioSlots system) newList
      return system

    removeEntity system entity = return system
    
{-
    removeEntity ecsg3d entity = do
      let (g3ds, guis, camera, viewport) = g3d
      let removeFunction mvList cache = do
            oldList <- takeMVar mvList
            
            let newList = filter ( (/=) 
-}

    initializeSystem = do
        audioSlots <- newMVar []
        audioSlotsCache <- HT.new
        return $ (ECSAudio audioSlots audioSlotsCache)

    stepSystem system = do
      -- update audio objects
      cList <- takeMVar (audioSlots system)
      mapM (\(com, mcp) -> do
               mOldCache <- HT.lookup (audioSlotsCache system) (idC com)
               -- handle events
               evts <- _popEvents com
               case mOldCache of
                 Just (A.AudioSlots map _, _, _) -> mapM (_handleEvent map) evts >> return ()
                 Nothing -> return ()
               -- handle changes per single entity item
               comTVal <- readC com >>= return . fromJust
               let newCom = fromStamped comTVal
               -- first create/update the audiosource object
               newOb <- case mOldCache of
                 Nothing -> A.audioSlots newCom
                 Just (oldOb, oldComTVal, mPos) -> if oldComTVal /= comTVal then A.updateAudioSlots oldOb newCom else return oldOb
               -- then handle the PO information - Position
               newPos <- case mcp of
                 Just cp -> do
                   posTVal <- readC cp >>= return . fromJust
                   let posVal = fromStamped posTVal
                   case mOldCache of
                     Nothing -> D.positionTo newOb posVal
                     Just (oldOb, oldComTVal, mPos) -> if (fromJust mPos) /= posTVal then D.positionTo newOb posVal else return ()
                   return $ Just posTVal
                 Nothing -> return Nothing
               -- insert new values into cache
               HT.insert (audioSlotsCache system) (idC com) (newOb, comTVal, newPos)
           ) cList
      putMVar (audioSlots system) cList
      return (system, False)

    shutdownSystem system = return ()

runSystemAudio :: D.TimeMS -> IO ECSAudio
runSystemAudio sleepT = runSystem sleepT


