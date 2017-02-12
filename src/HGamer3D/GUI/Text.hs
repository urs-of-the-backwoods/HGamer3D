{-
	GUI: Text functionality
	HGamer3D Library (A project to enable 3D game development in Haskell)
	Copyright 2015 Peter Althainz
	
	Distributed under the Apache License, Version 2.0
	(See attached file LICENSE or copy at 
	http://www.apache.org/licenses/LICENSE-2.0)
 
	file: HGamer3D/GUI/Text.hs
-}

-- | Module providing the Mouse functionality and settings
module HGamer3D.GUI.Text
where

import Fresco
import Data.Binary.Serialise.CBOR
import Data.Binary.Serialise.CBOR.Decoding

import Data.Text
import Data.Monoid
import Control.Applicative


type StaticText = Text

ctStaticText :: ComponentType StaticText
ctStaticText = ComponentType 0xda9601eaf3319280


