{-
	GUI: Slider functionality
	HGamer3D Library (A project to enable 3D game development in Haskell)
	Copyright 2015 - 2017 Peter Althainz
	
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

import Fresco
import Data.Binary.Serialise.CBOR
import Data.Binary.Serialise.CBOR.Encoding
import Data.Binary.Serialise.CBOR.Decoding

import Data.Text
import Data.Monoid
import Control.Applicative


data Slider = Slider {
    sliderRange::Float,
    sliderValue::Float
    } deriving (Eq, Read, Show)

ctSlider :: ComponentType Slider
ctSlider = ComponentType 0x60636b107c77a533

instance Serialise Slider where
    encode (Slider v1 v2) = encodeListLen 2 <> encode v1 <> encode v2
    decode = decodeListLenOf 2 >> Slider <$> decode <*> decode



  

