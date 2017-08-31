{-
	Datatypes to specify a geometric angle
	HGamer3D Library (A project to enable 3D game development in Haskell)
	Copyright 2011 - 2017 Peter Althainz
	
	Distributed under the Apache License, Version 2.0
	(See attached file LICENSE or copy at 
	http://www.apache.org/licenses/LICENSE-2.0)
 
	file: HGamer3D/Data/Colour.hs
-}
    
-- | The Colour type and some standard colours
module HGamer3D.Data.Colour

(
    Colour (..),
    ctColour,
    white,
    silver,
    grey,
    darkgrey,
    black,
    red,
    maroon,
    yellow,
    olive,
    lime,
    green,
    aqua,
    teal,
    blue,
    navy,
    fuchsia,
    purple
)

where

import Fresco
import Data.Binary.Serialise.CBOR
import Data.Binary.Serialise.CBOR.Decoding
import Data.Binary.Serialise.CBOR.Encoding

import Data.Text
import Data.Monoid
import Control.Applicative


data Colour = Colour {
    colourRed::Float,
    colourGreen::Float,
    colourBlue::Float,
    colourAlpha::Float
    } deriving (Eq, Read, Show)

ctColour :: ComponentType Colour
ctColour = ComponentType 0xe202add0521cde41


instance Serialise Colour where
    encode (Colour v1 v2 v3 v4) = encodeListLen 4 <> encode v1 <> encode v2 <> encode v3 <> encode v4
    decode = decodeListLenOf 4 >> Colour <$> decode <*> decode <*> decode <*> decode



-- generated

white :: Colour
white = Colour 1.0 1.0 1.0 1.0

silver:: Colour
silver  = Colour 0.75 0.75 0.75 1.0

grey:: Colour
grey = Colour 0.5 0.5 0.5 1.0

darkgrey :: Colour
darkgrey = Colour 0.25 0.25 0.25 1.0

black :: Colour
black = Colour 0.0 0.0 0.0 1.0

red :: Colour
red = Colour 1.0 0.0 0.0 1.0

maroon :: Colour
maroon = Colour 0.5 0.0 0.0 1.0

yellow :: Colour
yellow = Colour 1.0 1.0 0.0 1.0

olive :: Colour
olive = Colour 0.5 0.5 0.0 1.0

lime :: Colour
lime = Colour 0.0 1.0 0.0 1.0

green :: Colour
green = Colour 0.0 0.5 0.0 1.0

aqua :: Colour
aqua = Colour 0.0 1.0 1.0 1.0

teal :: Colour
teal = Colour 0.0 0.5 0.5 1.0

blue :: Colour
blue = Colour 0.0 0.0 1.0 1.0

navy :: Colour
navy = Colour 0.0 0.0 0.5 1.0

fuchsia :: Colour
fuchsia = Colour 1.0 0.0 1.0 1.0

purple :: Colour
purple = Colour 0.5 0.0 0.5 1.0



