{-
	Sound Source
	HGamer3D Library (A project to enable 3D game development in Haskell)
	Copyright 2015 - 2017 Peter Althainz
	
	Distributed under the Apache License, Version 2.0
	(See attached file LICENSE or copy at 
	http://www.apache.org/licenses/LICENSE-2.0)
 
	file: HGamer3D/Audio/SoundSource.hs
-}

-- | Module providing a Sound Source
module HGamer3D.Audio.SoundSource
where

import Fresco
import Data.Binary.Serialise.CBOR
import Data.Binary.Serialise.CBOR.Decoding

import Data.Text
import Data.Monoid
import Control.Applicative


data SoundType = Sound
    | Sound3D
    | Music
    deriving (Eq, Read, Show)


instance Serialise SoundType where
    encode (Sound) = encode (0::Int) 
    encode (Sound3D) = encode (1::Int) 
    encode (Music) = encode (2::Int) 
    decode = do
        i <- decode :: Decoder Int
        case i of
            0 -> (pure Sound)
            1 -> (pure Sound3D)
            2 -> (pure Music)

data SoundSource = SoundSource {
    soundSourceType::SoundType,
    soundSourceResource::Text,
    soundSourceLoop::Bool,
    soundSourceVolume::Float,
    soundSourceVolumeGroup::Text
    } deriving (Eq, Read, Show)


instance Serialise SoundSource where
    encode (SoundSource v1 v2 v3 v4 v5) = encode v1 <> encode v2 <> encode v3 <> encode v4 <> encode v5
    decode = SoundSource <$> decode <*> decode <*> decode <*> decode <*> decode

ctSoundSource :: ComponentType SoundSource
ctSoundSource = ComponentType 0xafcef7aa41d88c0d
