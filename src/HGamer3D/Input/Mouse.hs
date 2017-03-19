{-
	Mouse functionality and settings
	HGamer3D Library (A project to enable 3D game development in Haskell)
	Copyright 2015 - 2017 Peter Althainz
	
	Distributed under the Apache License, Version 2.0
	(See attached file LICENSE or copy at 
	http://www.apache.org/licenses/LICENSE-2.0)
 
	file: HGamer3D/Input/Mouse.hs
-}

-- | Module providing the Mouse functionality and settings
module HGamer3D.Input.Mouse
where

import Fresco
import Data.Binary.Serialise.CBOR
import Data.Binary.Serialise.CBOR.Encoding
import Data.Binary.Serialise.CBOR.Decoding

import Data.Text
import Data.Monoid
import Control.Applicative


data MouseMode = Absolute
    | Relative
    | Wrap
    deriving (Eq, Read, Show)

data MouseConfig = MouseConfig {
    mouseConfigMode::MouseMode
    } deriving (Eq, Read, Show)

ctMouseConfig :: ComponentType MouseConfig
ctMouseConfig = ComponentType 0xa532f43b1c1c6bc7

data MouseButtonData = MouseButtonData {
    mouseButtonDataButton::Int,
    mouseButtonDataButtons::Int,
    mouseButtonDataQualifiers::Int
    } deriving (Eq, Read, Show)

data MouseMoveData = MouseMoveData {
    mouseMoveDataX::Int,
    mouseMoveDataY::Int,
    mouseMoveDataDx::Int,
    mouseMoveDataDy::Int,
    mouseMoveDataButtons::Int,
    mouseMoveDataQualifiers::Int
    } deriving (Eq, Read, Show)

data MouseWheelData = MouseWheelData {
    mouseWheelDataWheel::Int,
    mouseWheelDataButtons::Int,
    mouseWheelDataQualifiers::Int
    } deriving (Eq, Read, Show)

data MouseEvent = NoMouseEvent
    | MouseButtonUpEvent MouseButtonData
    | MouseButtonDownEvent MouseButtonData
    | MouseMoveEvent MouseMoveData
    | MouseWheelEvent MouseWheelData
    deriving (Eq, Read, Show)

ctMouseEvent :: ComponentType MouseEvent
ctMouseEvent = ComponentType 0x27eaf3fd46595d08

instance Serialise MouseMode where
    encode (Absolute) = encodeListLen 1 <>  encode (0::Int) 
    encode (Relative) = encodeListLen 1 <>  encode (1::Int) 
    encode (Wrap) = encodeListLen 1 <>  encode (2::Int) 
    decode = do
        decodeListLen
        i <- decode :: Decoder Int
        case i of
            0 -> (pure Absolute)
            1 -> (pure Relative)
            2 -> (pure Wrap)

instance Serialise MouseConfig where
    encode (MouseConfig v1) = encodeListLen 1 <> encode v1
    decode = decodeListLenOf 1 >> MouseConfig <$> decode

instance Serialise MouseButtonData where
    encode (MouseButtonData v1 v2 v3) = encodeListLen 3 <> encode v1 <> encode v2 <> encode v3
    decode = decodeListLenOf 3 >> MouseButtonData <$> decode <*> decode <*> decode

instance Serialise MouseMoveData where
    encode (MouseMoveData v1 v2 v3 v4 v5 v6) = encodeListLen 6 <> encode v1 <> encode v2 <> encode v3 <> encode v4 <> encode v5 <> encode v6
    decode = decodeListLenOf 6 >> MouseMoveData <$> decode <*> decode <*> decode <*> decode <*> decode <*> decode

instance Serialise MouseWheelData where
    encode (MouseWheelData v1 v2 v3) = encodeListLen 3 <> encode v1 <> encode v2 <> encode v3
    decode = decodeListLenOf 3 >> MouseWheelData <$> decode <*> decode <*> decode

instance Serialise MouseEvent where
    encode (NoMouseEvent) = encodeListLen 1 <>  encode (0::Int) 
    encode (MouseButtonUpEvent v1) = encodeListLen 2 <>  encode (1::Int) <> encode v1
    encode (MouseButtonDownEvent v1) = encodeListLen 2 <>  encode (2::Int) <> encode v1
    encode (MouseMoveEvent v1) = encodeListLen 2 <>  encode (3::Int) <> encode v1
    encode (MouseWheelEvent v1) = encodeListLen 2 <>  encode (4::Int) <> encode v1
    decode = do
        decodeListLen
        i <- decode :: Decoder Int
        case i of
            0 -> (pure NoMouseEvent)
            1 -> (MouseButtonUpEvent <$> decode)
            2 -> (MouseButtonDownEvent <$> decode)
            3 -> (MouseMoveEvent <$> decode)
            4 -> (MouseWheelEvent <$> decode)


