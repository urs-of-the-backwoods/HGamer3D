{-
	Transformation of 3D entities by scale, position, ...
	HGamer3D Library (A project to enable 3D game development in Haskell)
	Copyright 2011-2015 Peter Althainz
	
	Distributed under the Apache License, Version 2.0
	(See attached file LICENSE or copy at 
	http://www.apache.org/licenses/LICENSE-2.0)
 
	file: HGamer3D/Data/Transform3D.hs
-}
  
-- | Functions for basic 3D transformations
module HGamer3D.Data.Transform3D
(
  translate,
  scale, 
  yaw,
  pitch,
  roll
)

where

import HGamer3D.Data.Vector
import HGamer3D.Data.Angle

type Position = Vec3
type Orientation = UnitQuaternion
type Size = Vec3

-- | move the position 
translate :: Position -> Position -> Position
translate = (&+)

-- | scale the size
scale :: Size -> Vec3 -> Size
scale  = (&!)

-- yaw, roll, pitch functions
-- functions, to rotate on axis, relative to object
rotRelativeToObjectAxis :: Orientation -> Vec3 -> Float -> Orientation
rotRelativeToObjectAxis ori axis val = let
  odir = actU ori axis
  qrot = rotU odir val
  nrot = qrot .*. ori
  in nrot
	
-- | rotate object on own axis (yaw) by angle
yaw :: Orientation -> Angle -> Orientation
yaw ori val = rotRelativeToObjectAxis ori vec3Y (asRad val)

-- | rotate object on own axis (roll) by angle
roll :: Orientation -> Angle -> Orientation
roll ori val = rotRelativeToObjectAxis ori vec3Z (asRad val)

-- | rotate object on own axis (pitch) by angle
pitch :: Orientation -> Angle -> Orientation
pitch ori val = rotRelativeToObjectAxis ori vec3X (asRad val)

