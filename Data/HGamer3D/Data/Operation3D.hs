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

-- Operation3D.hs

-- | Typeclasses for basic 3D operations
module HGamer3D.Data.Operation3D
(
        -- * Types
	Position3D (..),
	Scale3D (..),
	Direction3D (..),
	Orientation3D (..),
        
        -- * Functions
	translate3D,
        yaw3D,
        pitch3D,
        roll3D
)

where

import HGamer3D.Data.Vector
import HGamer3D.Data.Angle

-- | a type with a Position3D instance can be positioned
class Position3D t where

        -- | get position function
	position3D :: t -> IO Vec3
        -- | set position function
	positionTo3D :: t -> Vec3 -> IO ()
	
-- | move position function
translate3D :: Position3D t => t -> Vec3 -> IO ()
translate3D t v = do
	p <- position3D t
	positionTo3D t ( v &+ p )
	return ()

-- | a type with a Scale3D instance can be scaled
class Scale3D t where	
	
        -- | get scale function
	scale3D :: t -> IO Vec3
        -- | set scale function
	scaleTo3D :: t -> Vec3 -> IO ()

-- | a type with a Direction3D instance can be oriented towards a point (Camera for example)
class Direction3D t where
        -- | get direction function
	direction3D :: t -> IO Vec3
        -- | set direction function
	directionTo3D :: t -> Vec3 -> IO ()

-- | a type with an Orientation3D instance can be oriented in space
class Orientation3D t where
        -- | get orientation function
	orientation3D :: t -> IO UnitQuaternion
        -- | set orientation function
	orientationTo3D :: t -> UnitQuaternion -> IO ()

-- yaw, roll, pitch functions
-- functions, to rotate on axis, relative to object
rotRelativeToObjectAxis :: Orientation3D t => t -> Vec3 -> Float -> IO ()
rotRelativeToObjectAxis object axis val = do
	qob <- orientation3D object
	let odir = actU qob axis
	let qrot = rotU odir val
	let nrot = qrot .*. qob
	orientationTo3D object nrot
	return ()
	
-- | rotate object on own axis (yaw) by angle
yaw3D :: Orientation3D t => t -> Angle -> IO ()
yaw3D object val = rotRelativeToObjectAxis object (Vec3 0.0 1.0 0.0) (fromAngle val)

-- | rotate object on own axis (roll) by angle
roll3D :: Orientation3D t => t -> Angle -> IO ()
roll3D object val = rotRelativeToObjectAxis object (Vec3 0.0 0.0 1.0) (fromAngle val)

-- | rotate object on own axis (pitch) by angle
pitch3D :: Orientation3D t => t -> Angle -> IO ()
pitch3D object val = rotRelativeToObjectAxis object (Vec3 1.0 0.0 0.0) (fromAngle val)

