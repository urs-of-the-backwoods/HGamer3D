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
	Position (..),
	Size (..),
	Orientation (..),
        
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

-- | a type with a Position instance has a position
class Position t where

        -- | get position function
	position :: t -> IO Vec3
        -- | set position function
	positionTo :: t -> Vec3 -> IO ()
	
-- | move the position 
translate :: Position t => t -> Vec3 -> IO ()
translate t v = do
	p <- position t
	positionTo t ( v &+ p )
	return ()

-- | a type with a Size instance has a size
class Size t where	
	
        -- | get scale function
	size :: t -> IO Vec3
        -- | set scale function
	sizeTo :: t -> Vec3 -> IO ()

-- | scale the size
scale :: Size t => t -> Vec3 -> IO ()
scale t v = do
	s <- size t
	sizeTo t ( v &! s )
	return ()

-- | a type with an Orientation instance has an oriented in space
class Orientation t where
        -- | get orientation function
	orientation :: t -> IO UnitQuaternion
        -- | set orientation function
	orientationTo :: t -> UnitQuaternion -> IO ()

-- yaw, roll, pitch functions
-- functions, to rotate on axis, relative to object
rotRelativeToObjectAxis :: Orientation t => t -> Vec3 -> Float -> IO ()
rotRelativeToObjectAxis object axis val = do
	qob <- orientation object
	let odir = actU qob axis
	let qrot = rotU odir val
	let nrot = qrot .*. qob
	orientationTo object nrot
	return ()
	
-- | rotate object on own axis (yaw) by angle
yaw :: Orientation t => t -> Angle -> IO ()
yaw object val = rotRelativeToObjectAxis object vec3Y (fromAngle val)

-- | rotate object on own axis (roll) by angle
roll :: Orientation t => t -> Angle -> IO ()
roll object val = rotRelativeToObjectAxis object vec3Z (fromAngle val)

-- | rotate object on own axis (pitch) by angle
pitch :: Orientation t => t -> Angle -> IO ()
pitch object val = rotRelativeToObjectAxis object vec3X (fromAngle val)

