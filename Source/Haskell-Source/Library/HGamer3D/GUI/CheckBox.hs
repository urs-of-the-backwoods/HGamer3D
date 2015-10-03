{-
	GUI: Checkbox functionality
	HGamer3D Library (A project to enable 3D game development in Haskell)
	Copyright 2015 Peter Althainz
	
	Distributed under the Apache License, Version 2.0
	(See attached file LICENSE or copy at 
	http://www.apache.org/licenses/LICENSE-2.0)
 
	file: HGamer3D/GUI/CheckBox.hs
-}

-- | Module providing the CheckBox gui element
module HGamer3D.GUI.CheckBox
(
    ctCheckBox
)

where

import Data.MessagePack
import Debug.Trace
import Data.Text

import HGamer3D.Data

ctCheckBox :: ComponentType Bool
ctCheckBox = ComponentType 0xd2425f880fcdd9a4  
  

