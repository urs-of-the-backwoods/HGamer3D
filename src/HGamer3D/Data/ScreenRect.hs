{-
	Datatypes to specify a geometric angle
	HGamer3D Library (A project to enable 3D game development in Haskell)
	Copyright 2011-2017 Peter Althainz
	
	Distributed under the Apache License, Version 2.0
	(See attached file LICENSE or copy at 
	http://www.apache.org/licenses/LICENSE-2.0)
 
	file: HGamer3D/Data/ScreenRect.hs
-}

module HGamer3D.Data.ScreenRect

where

import Fresco
import Data.Binary.Serialise.CBOR
import Data.Binary.Serialise.CBOR.Decoding

import Data.Text
import Data.Monoid
import Control.Applicative

import Data.Word


data ScreenRect = ScreenRect {
    screenRectX::Int,
    screenRectY::Int,
    screenRectWidth::Int,
    screenRectHeight::Int
    } deriving (Eq, Read, Show)

ctScreenRect :: ComponentType ScreenRect
ctScreenRect = ComponentType 0x16877957e32da6b1

instance Serialise ScreenRect where
    encode (ScreenRect v1 v2 v3 v4) = encode v1 <> encode v2 <> encode v3 <> encode v4
    decode = ScreenRect <$> decode <*> decode <*> decode <*> decode

