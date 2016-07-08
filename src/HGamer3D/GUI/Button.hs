{-
	GUI: Button functionality
	HGamer3D Library (A project to enable 3D game development in Haskell)
	Copyright 2015 Peter Althainz
	
	Distributed under the Apache License, Version 2.0
	(See attached file LICENSE or copy at 
	http://www.apache.org/licenses/LICENSE-2.0)
 
	file: HGamer3D/GUI/Button.hs
-}

-- | Module providing the Mouse functionality and settings
module HGamer3D.GUI.Button
(
    ctButton
)

where

import Fresco
import Debug.Trace
import Data.Text

import HGamer3D.Data

ctButton :: ComponentType Bool
ctButton = ComponentType 0x68a1857c27690b30  
  

