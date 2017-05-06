{-
    Light Datatype
	HGamer3D Library (A project to enable 3D game development in Haskell)
	Copyright 2011 - 2017 Peter Althainz
	
	Distributed under the Apache License, Version 2.0
	(See attached file LICENSE or copy at 
	http://www.apache.org/licenses/LICENSE-2.0)
 
	file: HGamer3D/Graphics3D/Light.hs
-}

-- | Module providing the Light type
module HGamer3D.Graphics3D.Light
where

import Fresco
import Data.Binary.Serialise.CBOR
import Data.Binary.Serialise.CBOR.Encoding
import Data.Binary.Serialise.CBOR.Decoding

import Data.Text
import Data.Monoid
import Control.Applicative

import HGamer3D.Data.Angle


data LightType = PointLight
    | DirectionalLight
    | SpotLight Angle Float
    deriving (Eq, Read, Show)

data Light = Light {
    lightType::LightType,
    lightBrightness::Float,
    lightRange::Float,
    lightSpecularIntensity::Float
    } deriving (Eq, Read, Show)

ctLight :: ComponentType Light
ctLight = ComponentType 0x981e80e50d994ea9

instance Serialise LightType where
    encode (PointLight) = encodeListLen 1 <>  encode (0::Int) 
    encode (DirectionalLight) = encodeListLen 1 <>  encode (1::Int) 
    encode (SpotLight v1 v2) = encodeListLen 3 <>  encode (2::Int) <> encode v1<> encode v2
    decode = do
        decodeListLen
        i <- decode :: Decoder Int
        case i of
            0 -> (pure PointLight)
            1 -> (pure DirectionalLight)
            2 -> (SpotLight <$> decode <*> decode)

instance Serialise Light where
    encode (Light v1 v2 v3 v4) = encodeListLen 4 <> encode v1 <> encode v2 <> encode v3 <> encode v4
    decode = decodeListLenOf 4 >> Light <$> decode <*> decode <*> decode <*> decode

