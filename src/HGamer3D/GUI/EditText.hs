{-
	GUI: EditText functionality
	HGamer3D Library (A project to enable 3D game development in Haskell)
	Copyright 2015 Peter Althainz
	
	Distributed under the Apache License, Version 2.0
	(See attached file LICENSE or copy at 
	http://www.apache.org/licenses/LICENSE-2.0)
 
	file: HGamer3D/GUI/EditText.hs
-}

-- | Module providing the Mouse functionality and settings
module HGamer3D.GUI.EditText
where

import Fresco
import Data.Binary.Serialise.CBOR
import Data.Binary.Serialise.CBOR.Decoding

import Data.Text
import Data.Monoid
import Control.Applicative


type EditText = Text

ctEditText :: ComponentType EditText
ctEditText = ComponentType 0x8c79de2199331f3a


