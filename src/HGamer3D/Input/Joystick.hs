{-
	Joystick functionality and settings
	HGamer3D Library (A project to enable 3D game development in Haskell)
	Copyright 2015 Peter Althainz
	
	Distributed under the Apache License, Version 2.0
	(See attached file LICENSE or copy at 
	http://www.apache.org/licenses/LICENSE-2.0)
 
	file: HGamer3D/Input/Joystick.hs
-}

-- | Module providing the Joystick functionality and settings
module HGamer3D.Input.Joystick

where

import Fresco
import Data.Binary.Serialise.CBOR
import Data.Binary.Serialise.CBOR.Encoding
import Data.Binary.Serialise.CBOR.Decoding

import Data.Text
import Data.Monoid
import Control.Applicative


data Joystick = Joystick {
    -- | index of joystick, starting at 0 (first one)
    joystickIndex::Int
    } deriving (Eq, Read, Show)

ctJoystick :: ComponentType Joystick
ctJoystick = ComponentType 0xe5ea0d693a04ff71

data JoystickEvent = NoJoystickEvent
    | ButtonDown Int -- ^ Button Id 
    | ButtonUp Int -- ^ Button Id 
    | AxisMove Int Float -- ^ Axis Id, Move Position 
    | HatMove Int Int -- ^ Axis Id, Hat Position 
    | JoystickChange Int Int -- ^ joystick plugged, unplugged, min, max indicees 
    deriving (Eq, Read, Show)

ctJoystickEvent :: ComponentType JoystickEvent
ctJoystickEvent = ComponentType 0x1cdc5b0a65479346

data JoystickButton = A
    | B
    | Back
    | DPadDown
    | DPadLeft
    | DPadRight
    | DPadUp
    | Guide
    | LeftShoulder
    | LeftStick
    | RightShoulder
    | RightStick
    | Start
    | X
    | Y
    deriving (Eq, Read, Show)

data JoystickAxis = LeftX
    | LeftY
    | RightX
    | RightY
    | TriggerLeft
    | TriggerRight
    deriving (Eq, Read, Show)

instance Serialise Joystick where
    encode (Joystick v1) = encodeListLen 1 <> encode v1
    decode = decodeListLenOf 1 >> Joystick <$> decode

instance Serialise JoystickEvent where
    encode (NoJoystickEvent) = encodeListLen 1 <>  encode (0::Int) 
    encode (ButtonDown v1) = encodeListLen 2 <>  encode (1::Int) <> encode v1
    encode (ButtonUp v1) = encodeListLen 2 <>  encode (2::Int) <> encode v1
    encode (AxisMove v1 v2) = encodeListLen 3 <>  encode (3::Int) <> encode v1<> encode v2
    encode (HatMove v1 v2) = encodeListLen 3 <>  encode (4::Int) <> encode v1<> encode v2
    encode (JoystickChange v1 v2) = encodeListLen 3 <>  encode (5::Int) <> encode v1<> encode v2
    decode = do
        decodeListLen
        i <- decode :: Decoder s Int
        case i of
            0 -> (pure NoJoystickEvent)
            1 -> (ButtonDown <$> decode)
            2 -> (ButtonUp <$> decode)
            3 -> (AxisMove <$> decode <*> decode)
            4 -> (HatMove <$> decode <*> decode)
            5 -> (JoystickChange <$> decode <*> decode)

instance Serialise JoystickButton where
    encode (A) = encodeListLen 1 <>  encode (0::Int) 
    encode (B) = encodeListLen 1 <>  encode (1::Int) 
    encode (Back) = encodeListLen 1 <>  encode (2::Int) 
    encode (DPadDown) = encodeListLen 1 <>  encode (3::Int) 
    encode (DPadLeft) = encodeListLen 1 <>  encode (4::Int) 
    encode (DPadRight) = encodeListLen 1 <>  encode (5::Int) 
    encode (DPadUp) = encodeListLen 1 <>  encode (6::Int) 
    encode (Guide) = encodeListLen 1 <>  encode (7::Int) 
    encode (LeftShoulder) = encodeListLen 1 <>  encode (8::Int) 
    encode (LeftStick) = encodeListLen 1 <>  encode (9::Int) 
    encode (RightShoulder) = encodeListLen 1 <>  encode (10::Int) 
    encode (RightStick) = encodeListLen 1 <>  encode (11::Int) 
    encode (Start) = encodeListLen 1 <>  encode (12::Int) 
    encode (X) = encodeListLen 1 <>  encode (13::Int) 
    encode (Y) = encodeListLen 1 <>  encode (14::Int) 
    decode = do
        decodeListLen
        i <- decode :: Decoder s Int
        case i of
            0 -> (pure A)
            1 -> (pure B)
            2 -> (pure Back)
            3 -> (pure DPadDown)
            4 -> (pure DPadLeft)
            5 -> (pure DPadRight)
            6 -> (pure DPadUp)
            7 -> (pure Guide)
            8 -> (pure LeftShoulder)
            9 -> (pure LeftStick)
            10 -> (pure RightShoulder)
            11 -> (pure RightStick)
            12 -> (pure Start)
            13 -> (pure X)
            14 -> (pure Y)

instance Serialise JoystickAxis where
    encode (LeftX) = encodeListLen 1 <>  encode (0::Int) 
    encode (LeftY) = encodeListLen 1 <>  encode (1::Int) 
    encode (RightX) = encodeListLen 1 <>  encode (2::Int) 
    encode (RightY) = encodeListLen 1 <>  encode (3::Int) 
    encode (TriggerLeft) = encodeListLen 1 <>  encode (4::Int) 
    encode (TriggerRight) = encodeListLen 1 <>  encode (5::Int) 
    decode = do
        decodeListLen
        i <- decode :: Decoder s Int
        case i of
            0 -> (pure LeftX)
            1 -> (pure LeftY)
            2 -> (pure RightX)
            3 -> (pure RightY)
            4 -> (pure TriggerLeft)
            5 -> (pure TriggerRight)


