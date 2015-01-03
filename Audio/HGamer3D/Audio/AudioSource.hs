{-# LANGUAGE StandaloneDeriving, DeriveDataTypeable #-}
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

-- | data types which describes an Audio Source
module HGamer3D.Audio.AudioSource

where

import qualified HGamer3D.Data as D
import qualified Data.Map as M
import Data.Typeable

data AudioSlotParameter = AudioSlotParameter {
  aspFile :: String,         -- ^ Audio file on disk
  aspVolume :: Float,        -- ^ Volume 0.0 - 100.0
  aspLoopFlag :: Bool,       -- ^ is sound looping
  aspPositionFlag :: Bool,   -- ^ is sound position dependent
  aspPitch :: Float,         -- ^ Frequency change, 1.0 no change
  aspAttenuation :: Float,   -- ^ Damping factor by distance, default 1.0, 0.0 no damping
  aspMinDistance :: Float    -- ^ at min distance, sound is played with volume 100.0
} deriving (Eq, Show, Typeable)

data AudioSlot = Music AudioSlotParameter
               | Sound AudioSlotParameter deriving (Eq, Show, Typeable)

type AudioSource = M.Map String AudioSlot

musicSlot :: String -> AudioSlot
musicSlot file = Music (AudioSlotParameter file 100.0 False True 1.0 1.0 10.0)

soundSlot :: String -> AudioSlot
soundSlot file = Sound (AudioSlotParameter file 100.0 False True 1.0 1.0 10.0)


data AudioCmd   = PlayAudio String
                | StopAudio String
                  deriving (Eq, Typeable, Show)


