{-
	Keyboard functionality and settings
	HGamer3D Library (A project to enable 3D game development in Haskell)
	Copyright 2015 Peter Althainz
	
	Distributed under the Apache License, Version 2.0
	(See attached file LICENSE or copy at 
	http://www.apache.org/licenses/LICENSE-2.0)
 
	file: HGamer3D/Input/Keyboard.hs
-}

-- | Module providing the keyboard functionality and settings
module HGamer3D.Input.Keyboard
where

import Fresco
import Data.Binary.Serialise.CBOR
import Data.Binary.Serialise.CBOR.Decoding

import Data.Text
import Data.Monoid
import Control.Applicative


data KeyData = KeyData {
    keyDataKey::Int,
    keyDataScancode::Int,
    keyDataName::Text
    } deriving (Eq, Read, Show)

data KeyEvent = NoKeyEvent
    | KeyUpEvent KeyData
    | KeyDownEvent KeyData
    deriving (Eq, Read, Show)

ctKeyEvent :: ComponentType KeyEvent
ctKeyEvent = ComponentType 0x5ba1617fb50e97e5

instance Serialise KeyData where
    encode (KeyData v1 v2 v3) = encode v1 <> encode v2 <> encode v3
    decode = KeyData <$> decode <*> decode <*> decode

instance Serialise KeyEvent where
    encode (NoKeyEvent) = encode (0::Int) 
    encode (KeyUpEvent v1) = encode (1::Int) <> encode v1
    encode (KeyDownEvent v1) = encode (2::Int) <> encode v1
    decode = do
        i <- decode :: Decoder Int
        case i of
            0 -> (pure NoKeyEvent)
            1 -> (KeyUpEvent <$> decode)
            2 -> (KeyDownEvent <$> decode)


