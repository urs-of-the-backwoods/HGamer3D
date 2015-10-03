{-
	Transformation of 3D entities by scale, position, ...
	HGamer3D Library (A project to enable 3D game development in Haskell)
	Copyright 2011-2015 Peter Althainz
	
	Distributed under the Apache License, Version 2.0
	(See attached file LICENSE or copy at 
	http://www.apache.org/licenses/LICENSE-2.0)
 
	file: HGamer3D/Data/TypeSynonyms.hs
-}

-- | Some common type synonyms
module HGamer3D.Data.TypeSynonyms

where

import HGamer3D.Data.Component
import HGamer3D.Data.Vector
  
-- | Size implemented as a Vec3
type Scale = Vec3

-- | Position implemented as a Vec3
type Position = Vec3

-- | Orientation implemented as a UnitQuaternion
type Orientation = UnitQuaternion

-- properties
ctPosition :: ComponentType Position
ctPosition = ComponentType 0x29aacbbb10c84016

ctScale :: ComponentType Scale
ctScale = ComponentType 0x2f9c124bc8fd41c4

-- CH4-3s
ctOrientation :: ComponentType Orientation
ctOrientation = ComponentType 0x815eb4d9c7bfaa74
-- CH4-3e

ctVisible :: ComponentType Bool
ctVisible = ComponentType 0x98e7a78e949e1c6e


