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
import Data.Binary.Serialise.CBOR.Decoding

import Data.Text
import Data.Monoid
import Control.Applicative


data InputEventType = IEMouseButtonUp
    | IEMouseButtonDown
    | IEMouseButtonWheel
    | IEMouseVisible
    | IEKeyUp
    | IEKeyDown
    | IEExitRequested
    deriving (Eq, Read, Show)

data InputEventHandler = DefaultEventHandler
    | SpecificEventHandler [InputEventType]
    deriving (Eq, Read, Show)

data ExitRequestedEvent = ExitRequestedEvent
    deriving (Eq, Read, Show)

ctInputEventHandler :: ComponentType InputEventHandler
ctInputEventHandler = ComponentType 0xfc0edefcebcb5878

ctExitRequestedEvent :: ComponentType ExitRequestedEvent
ctExitRequestedEvent = ComponentType 0x824517eb48d5c653

instance Serialise InputEventType where
    encode (IEMouseButtonUp) = encode (0::Int) 
    encode (IEMouseButtonDown) = encode (1::Int) 
    encode (IEMouseButtonWheel) = encode (2::Int) 
    encode (IEMouseVisible) = encode (3::Int) 
    encode (IEKeyUp) = encode (4::Int) 
    encode (IEKeyDown) = encode (5::Int) 
    encode (IEExitRequested) = encode (6::Int) 
    decode = do
        i <- decode :: Decoder Int
        case i of
            0 -> (pure IEMouseButtonUp)
            1 -> (pure IEMouseButtonDown)
            2 -> (pure IEMouseButtonWheel)
            3 -> (pure IEMouseVisible)
            4 -> (pure IEKeyUp)
            5 -> (pure IEKeyDown)
            6 -> (pure IEExitRequested)

instance Serialise InputEventHandler where
    encode (DefaultEventHandler) = encode (0::Int) 
    encode (SpecificEventHandler v1) = encode (1::Int) <> encode v1
    decode = do
        i <- decode :: Decoder Int
        case i of
            0 -> (pure DefaultEventHandler)
            1 -> (SpecificEventHandler <$> decode)

instance Serialise ExitRequestedEvent where
    encode (ExitRequestedEvent) = encode (0::Int) 
    decode = do
        i <- decode :: Decoder Int
        case i of
            0 -> (pure ExitRequestedEvent)

