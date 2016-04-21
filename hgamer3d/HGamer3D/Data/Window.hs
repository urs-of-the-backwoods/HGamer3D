{-
	The Window Instance Type
	HGamer3D Library (A project to enable 3D game development in Haskell)
	Copyright 2011-2015 Peter Althainz
	
	Distributed under the Apache License, Version 2.0
	(See attached file LICENSE or copy at 
	http://www.apache.org/licenses/LICENSE-2.0)
 
	file: HGamer3D/Data/Window.hs
-}

-- | Type for a system window as its window handle
module HGamer3D.Data.Window
(
  -- * A Window Handle as Int
  Window (..),
  ctWindow
    
) where

import Fresco

type Window = Int

-- properties
ctWindow :: ComponentType Window
ctWindow = ComponentType 0xad5b3c63f7f7dd1b


