-- This source file is part of HGamer3D
-- (A project to enable 3D game development in Haskell)
-- For the latest info, see http://www.hgamer3d.org
--
-- (c) 2011-2013 Peter Althainz
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

-- Transform3D.hs

-- | Typeclasses for basic 3D transformations

module HGamer3D.Data.Transform3D
(
        -- * Types
	HasPosition (..),
	HasSize (..),
	HasOrientation (..),
        
        -- * Functions
	translate,
        scale, 
        yaw,
        pitch,
        roll
)

where

import HGamer3D.Data.Vector
import HGamer3D.Data.Angle
import HGamer3D.Data.TypeSynonyms

-- | a type with a HasPosition instance has a Position
class HasPosition t where

        -- | get position function
	position :: t -> IO Position
        -- | set position function
	positionTo :: t -> Position -> IO ()

-- | a type with a HasSize instance has a Size
class HasSize t where	
	
        -- | get scale function
	size :: t -> IO Size
        -- | set scale function
	sizeTo :: t -> Size -> IO ()

-- | a type with an HasOrientation instance has an oriented in space
class HasOrientation t where
        -- | get orientation function
	orientation :: t -> IO Orientation
        -- | set orientation function
	orientationTo :: t -> Orientation -> IO ()


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
yaw ori val = rotRelativeToObjectAxis ori vec3Y (fromAngle val)

-- | rotate object on own axis (roll) by angle
roll :: Orientation -> Angle -> Orientation
roll ori val = rotRelativeToObjectAxis ori vec3Z (fromAngle val)

-- | rotate object on own axis (pitch) by angle
pitch :: Orientation -> Angle -> Orientation
pitch ori val = rotRelativeToObjectAxis ori vec3X (fromAngle val)

