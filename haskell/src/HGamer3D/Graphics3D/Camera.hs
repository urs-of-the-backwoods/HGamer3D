{-
    Camera Datatype
	HGamer3D Library (A project to enable 3D game development in Haskell)
	Copyright 2011-2015 Peter Althainz
	
	Distributed under the Apache License, Version 2.0
	(See attached file LICENSE or copy at 
	http://www.apache.org/licenses/LICENSE-2.0)
 
	file: HGamer3D/Graphics3D/Camera.hs
-}

-- | Module providing the Camera type
module HGamer3D.Graphics3D.Camera

(
        Camera (..),
        ctCamera,

        Frustum (..),
        ctFrustum
)

where

import Fresco
import Data.Binary.Serialise.CBOR
import Data.Binary.Serialise.CBOR.Encoding
import Data.Binary.Serialise.CBOR.Decoding

import Data.Text
import Data.Monoid
import Control.Applicative

import HGamer3D.Data.Angle
import HGamer3D.Data.ScreenRect

data Camera = FullViewCamera
    | OverlayCamera ScreenRect Float
    deriving (Eq, Read, Show)

ctCamera :: ComponentType Camera
ctCamera = ComponentType 0xd3b0d455ab1f4716

data Frustum = Frustum {
    frustumNearDistance::Float,
    frustumFarDistance::Float,
    frustumFieldOfViewHorizontal::Angle
    } deriving (Eq, Read, Show)

ctFrustum :: ComponentType Frustum
ctFrustum = ComponentType 0xf3ce3235d4f8e73d

instance Serialise Camera where
    encode (FullViewCamera) = encodeListLen 1 <>  encode (0::Int) 
    encode (OverlayCamera v1 v2) = encodeListLen 3 <>  encode (1::Int) <> encode v1<> encode v2
    decode = do
        decodeListLen
        i <- decode :: Decoder s Int
        case i of
            0 -> (pure FullViewCamera)
            1 -> (OverlayCamera <$> decode <*> decode)

instance Serialise Frustum where
    encode (Frustum v1 v2 v3) = encodeListLen 3 <> encode v1 <> encode v2 <> encode v3
    decode = decodeListLenOf 3 >> Frustum <$> decode <*> decode <*> decode






