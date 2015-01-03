{-# Language StandaloneDeriving, DeriveDataTypeable #-}
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

module HGamer3D.Audio.SystemAudio

where

import qualified Data.Map as M
import Data.Maybe
import Data.Typeable
import Data.Dynamic
import Data.IORef
import Data.Traversable
import Control.Concurrent

import qualified HGamer3D.Data as D
import HGamer3D.Common
import qualified HGamer3D.BaseAPI.Audio as A

import qualified HGamer3D.Audio.AudioSource as AS
import qualified HGamer3D.Audio.AudioListener as AL

data AudioMap = AudioMap (M.Map String A.AudioSource) AS.AudioSource

{- In this module the AS.AudioSource is a map of AS.AudioSlots, where each slot aligns to a
   AudioSource from the base Audio module. So the AS.AudioSource is a map of real sources, 
   to be able to have multiple sounds played by one object.
-}

-- | create AudioMap from pure data definition out of Schema
audioMap :: AS.AudioSource -> IO AudioMap
audioMap mapAS = do

  let makeSource asource = do
        -- create source
        (source, parameter) <- case asource of
          AS.Music parameter -> A.musicAudioSource (AS.aspFile parameter) >>= \s -> return (fromJust s, parameter)
          AS.Sound parameter -> A.soundAudioSource (AS.aspFile parameter) >>= \s -> return (fromJust s, parameter)
        -- set all parameters
        A.setAudioSourceVolume source (AS.aspVolume parameter)
        A.setAudioSourceLoop source (AS.aspLoopFlag parameter)
        A.setAudioSourcePositionDependent source (AS.aspPositionFlag parameter)
        A.setAudioSourcePitch source (AS.aspPitch parameter)
        A.setAudioSourceAttenuation source (AS.aspAttenuation parameter)
        A.setAudioSourceMinDistance source (AS.aspMinDistance parameter)
        return source

  obMap <- traverse makeSource mapAS
  return $ AudioMap obMap mapAS

-- | update AudioSlots from change in data definition, tbd
updateAudioMap :: AudioMap -> AS.AudioSource -> IO AudioMap
updateAudioMap inSlots newSlots = return inSlots

-- | remove AudioSlots, tbd
removeAudioMap :: AudioMap -> IO ()
removeAudioMap as = return ()

setPosition :: AudioMap -> D.Position -> IO ()
setPosition (AudioMap obMap schema) pos = do
            traverse ((flip A.setAudioSourcePosition) pos) obMap
            return ()

playCmd :: AudioMap -> Maybe AS.AudioCmd -> IO ()
playCmd (AudioMap obMap schema) (Just (AS.PlayAudio slot)) = A.playAudioSource (fromJust (M.lookup slot obMap))
playCmd (AudioMap obMap schema) (Just (AS.StopAudio slot)) = A.stopAudioSource (fromJust (M.lookup slot obMap))
playCmd _ _ = return ()

data ECSAudio = ECSAudio     -- no audio specific data here, but never mind

instance System ECSAudio where

    initializeSystem = do
         
      lock <- newMVar ()
      newERefs <- newIORef []
      delERefs <- newIORef []
      let records = []
      let system = ECSAudio

      let systemFunction system eref = do

          e <- readE eref -- this e is used to create the representation

          if e #? CTASr then do

             rep <- audioMap ((e # CTASr) :: AS.AudioSource)
             ref <- newIORef rep
             if e #? CTPos then setPosition rep (e # CTPos) else return ()

             let createRecord ct = do
                 if e #? ct then do
                    l <- componentListener eref ct
                    let uf = case ct of
                                  CTASr -> \ _ e' -> do
                                                              updateAudioMap rep (e' # CTASr)
                                                              if e #? CTPos then setPosition rep (e' # CTPos) else return ()
                                                              return () 
                                  CTCmd -> \ _ e' -> do
                                                              playCmd rep (e' ?# CTCmd)
                                                              return ()
                                  _ -> \ _ _ -> return ()

                    let df = return ()
                    return $ Just (l, uf, df)
                    else return Nothing

             newRecords <- Prelude.mapM createRecord [CTASr, CTCmd]
             return (map fromJust (filter isJust newRecords))

             else return []
        
      return (SystemData lock newERefs delERefs records system systemFunction)

    stepSystem (SystemData lock newERefs delERefs records system systemFunction) = do
      return False

forkAudioWorld :: D.GameTime -> IO [SomeSystem]
forkAudioWorld sleepT = do
                       system <- (runSystem sleepT) :: IO (SystemData ECSAudio)
                       return $ [SomeSystem system]





