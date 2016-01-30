{-
	GUI: DropDownList functionality
	HGamer3D Library (A project to enable 3D game development in Haskell)
	Copyright 2015 Peter Althainz
	
	Distributed under the Apache License, Version 2.0
	(See attached file LICENSE or copy at 
	http://www.apache.org/licenses/LICENSE-2.0)
 
	file: HGamer3D/GUI/DropDownList.hs
-}

-- | Module providing the Mouse functionality and settings
module HGamer3D.GUI.DropDownList
(
    ctDropDownList,
    DropDownList (..)
)

where

import Fresco
import Data.MessagePack
import Debug.Trace
import Data.Text

import HGamer3D.Data

data DropDownList = DropDownList [Text] (Maybe Int) deriving (Eq, Show)   -- int is selected item index, starting at 0

instance ComponentClass DropDownList where
    toObj (DropDownList textlist mb) = ObjectArray [ ObjectArray (Prelude.map toObj textlist), toObj mb]
    fromObj (ObjectArray [ ObjectArray textlist_o, mb]) = DropDownList (Prelude.map fromObj textlist_o) (fromObj mb)

ctDropDownList :: ComponentType DropDownList
ctDropDownList = ComponentType 0x200de0e837a8e590
  

