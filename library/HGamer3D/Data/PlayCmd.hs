{-
	The Play Command
	HGamer3D Library (A project to enable 3D game development in Haskell)
	Copyright 2011-2015 Peter Althainz
	
	Distributed under the Apache License, Version 2.0
	(See attached file LICENSE or copy at 
	http://www.apache.org/licenses/LICENSE-2.0)
 
	file: HGamer3D/Data/PlayCmd.hs
-}

-- | Type for a generic play command
module HGamer3D.Data.PlayCmd
(
  PlayCmd (..),
  ctPlayCmd
    
) where

import Data.MessagePack
import Fresco


data PlayCmd = Play
               | Pause
               | Stop
               deriving (Eq, Show)

instance ComponentClass PlayCmd where
    toObj Play = ObjectInt 1
    toObj Pause = ObjectInt 2
    toObj Stop = ObjectInt 3
    fromObj (ObjectInt 1) = Play
    fromObj (ObjectInt 2) = Pause
    fromObj (ObjectInt 3) = Stop

ctPlayCmd :: ComponentType PlayCmd
ctPlayCmd = ComponentType 0x35f7752020f7f1cd


