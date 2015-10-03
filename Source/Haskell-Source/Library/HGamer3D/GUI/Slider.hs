{-
	GUI: Slider functionality
	HGamer3D Library (A project to enable 3D game development in Haskell)
	Copyright 2015 Peter Althainz
	
	Distributed under the Apache License, Version 2.0
	(See attached file LICENSE or copy at 
	http://www.apache.org/licenses/LICENSE-2.0)
 
	file: HGamer3D/GUI/Slider.hs
-}

-- | Module providing the Mouse functionality and settings
module HGamer3D.GUI.Slider
(
    ctSlider,
    Slider (..)
)

where

import Data.MessagePack
import Debug.Trace
import Data.Text

import HGamer3D.Data

data Slider = Slider Float Float        -- range end (start = 0) and value
              deriving (Eq, Show)           

instance ComponentClass Slider where
    toObj (Slider range value) = ObjectArray [ObjectFloat range, ObjectFloat value]
    fromObj (ObjectArray [ObjectFloat range, ObjectFloat value]) = Slider range value

ctSlider :: ComponentType Slider
ctSlider = ComponentType 0x60636b107c77a533
  

