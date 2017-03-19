{-
	GUI: Button functionality
	HGamer3D Library (A project to enable 3D game development in Haskell)
	Copyright 2015 - 2017 Peter Althainz
	
	Distributed under the Apache License, Version 2.0
	(See attached file LICENSE or copy at 
	http://www.apache.org/licenses/LICENSE-2.0)
 
	file: HGamer3D/GUI/Button.hs
-}

-- | Module providing the Button functionality and settings
module HGamer3D.GUI.Button
where

import Fresco
import Data.Binary.Serialise.CBOR
import Data.Binary.Serialise.CBOR.Encoding
import Data.Binary.Serialise.CBOR.Decoding

import Data.Text
import Data.Monoid
import Control.Applicative


data Button = Button {
    buttonPressed::Bool,
    buttonLabel::Text
    } deriving (Eq, Read, Show)

ctButton :: ComponentType Button
ctButton = ComponentType 0x68a1857c27690b30

instance Serialise Button where
    encode (Button v1 v2) = encodeListLen 2 <> encode v1 <> encode v2
    decode = decodeListLenOf 2 >> Button <$> decode <*> decode


