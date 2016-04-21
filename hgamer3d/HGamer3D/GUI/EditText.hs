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
(
    ctEditText
)

where

import Fresco
import Data.MessagePack
import Debug.Trace
import Data.Text

import HGamer3D.Data

ctEditText :: ComponentType Text
ctEditText = ComponentType 0x8c79de2199331f3a
