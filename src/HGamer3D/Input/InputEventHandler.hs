{-
	Input Event handler and settings
	HGamer3D Library (A project to enable 3D game development in Haskell)
	Copyright 2015 - 2017 Peter Althainz
	
	Distributed under the Apache License, Version 2.0
	(See attached file LICENSE or copy at 
	http://www.apache.org/licenses/LICENSE-2.0)
 
	file: HGamer3D/Input/Mouse.hs
-}

-- | Module providing settings for all input events (Mouse, Keyboard, Joystick)
module HGamer3D.Input.InputEventHandler
where

import Fresco
import Data.Binary.Serialise.CBOR
import Data.Binary.Serialise.CBOR.Encoding
import Data.Binary.Serialise.CBOR.Decoding

import Data.Text
import Data.Monoid
import Control.Applicative


data InputEventType = IEMouseButtonUp
    | IEMouseButtonDown
    | IEMouseMove
    | IEMouseButtonWheel
    | IEMouseVisible
    | IEKeyUp
    | IEKeyDown
    | IEExitRequested
    deriving (Eq, Read, Show)

data InputEventHandler = DefaultEventHandler
    | SpecificEventHandler [InputEventType]
    deriving (Eq, Read, Show)

ctInputEventHandler :: ComponentType InputEventHandler
ctInputEventHandler = ComponentType 0xfc0edefcebcb5878

type ExitRequestedEvent = ()

ctExitRequestedEvent :: ComponentType ExitRequestedEvent
ctExitRequestedEvent = ComponentType 0x824517eb48d5c653

instance Serialise InputEventType where
    encode (IEMouseButtonUp) = encodeListLen 1 <>  encode (0::Int) 
    encode (IEMouseButtonDown) = encodeListLen 1 <>  encode (1::Int) 
    encode (IEMouseMove) = encodeListLen 1 <>  encode (2::Int) 
    encode (IEMouseButtonWheel) = encodeListLen 1 <>  encode (3::Int) 
    encode (IEMouseVisible) = encodeListLen 1 <>  encode (4::Int) 
    encode (IEKeyUp) = encodeListLen 1 <>  encode (5::Int) 
    encode (IEKeyDown) = encodeListLen 1 <>  encode (6::Int) 
    encode (IEExitRequested) = encodeListLen 1 <>  encode (7::Int) 
    decode = do
        decodeListLen
        i <- decode :: Decoder s Int
        case i of
            0 -> (pure IEMouseButtonUp)
            1 -> (pure IEMouseButtonDown)
            2 -> (pure IEMouseMove)
            3 -> (pure IEMouseButtonWheel)
            4 -> (pure IEMouseVisible)
            5 -> (pure IEKeyUp)
            6 -> (pure IEKeyDown)
            7 -> (pure IEExitRequested)

instance Serialise InputEventHandler where
    encode (DefaultEventHandler) = encodeListLen 1 <>  encode (0::Int) 
    encode (SpecificEventHandler v1) = encodeListLen 2 <>  encode (1::Int) <> encode v1
    decode = do
        decodeListLen
        i <- decode :: Decoder s Int
        case i of
            0 -> (pure DefaultEventHandler)
            1 -> (SpecificEventHandler <$> decode)

