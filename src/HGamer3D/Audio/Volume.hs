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
where

import Fresco
import Data.Binary.Serialise.CBOR
import Data.Binary.Serialise.CBOR.Encoding
import Data.Binary.Serialise.CBOR.Decoding

import Data.Text
import Data.Monoid
import Control.Applicative


data Volume = Volume {
    volumeGroup::Text,
    volumeGain::Float
    } deriving (Eq, Read, Show)

ctVolume :: ComponentType Volume
ctVolume = ComponentType 0x659d20e6e65f85fe

instance Serialise Volume where
    encode (Volume v1 v2) = encodeListLen 2 <> encode v1 <> encode v2
    decode = decodeListLenOf 2 >> Volume <$> decode <*> decode




