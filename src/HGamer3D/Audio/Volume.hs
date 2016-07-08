{-
	Audio: Master Volume for different categories
	HGamer3D Library (A project to enable 3D game development in Haskell)
	Copyright 2015 Peter Althainz
	
	Distributed under the Apache License, Version 2.0
	(See attached file LICENSE or copy at 
	http://www.apache.org/licenses/LICENSE-2.0)
 
	file: HGamer3D/Audio/Volume.hs
-}

-- | Module providing the Mouse functionality and settings
module HGamer3D.Audio.Volume
(
    Volume (..),
    ctVolume
)

where

import Fresco
import Data.MessagePack
import Debug.Trace
import Data.Text

import HGamer3D.Data

data Volume = Volume Text Float deriving (Eq, Show)

instance ComponentClass Volume where
    toObj (Volume cat vol) = ObjectArray [toObj cat, ObjectFloat vol]
    fromObj (ObjectArray [cat_o, ObjectFloat vol]) = Volume (fromObj cat_o) vol

ctVolume :: ComponentType Volume
ctVolume = ComponentType 0x659d20e6e65f85fe
  

