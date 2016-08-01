{-
	GUI: Button functionality
	HGamer3D Library (A project to enable 3D game development in Haskell)
	Copyright 2015 Peter Althainz
	
	Distributed under the Apache License, Version 2.0
	(See attached file LICENSE or copy at 
	http://www.apache.org/licenses/LICENSE-2.0)
 
	file: HGamer3D/GUI/Button.hs
-}

-- | Module providing the Button functionality and settings
module HGamer3D.GUI.Button
(
	Button (..),
    ctButton
)

where

import Fresco
import Debug.Trace
import Data.Text
import Data.MessagePack

import HGamer3D.Data


data Button = Button {
    buttonPressed::Bool,
    buttonLabel::Text
} deriving (Eq, Show, Read)

instance ComponentClass Button where
    toObj (Button v1 v2) = ObjectArray [ObjectBool v1, (toObj v2)]
    fromObj (ObjectArray [ObjectBool v1, v2]) = Button v1 (fromObj v2)

ctButton :: ComponentType Button
ctButton = ComponentType 0x68a1857c27690b30

