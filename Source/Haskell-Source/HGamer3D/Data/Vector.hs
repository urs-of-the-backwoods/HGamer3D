-- This source file is part of HGamer3D
-- (A project to enable 3D game development in Haskell)
-- For the latest info, see http://www.hgamer3d.org
--
-- (c) 2011-2015 Peter Althainz
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

-- HGamer3D/Data/Vector.hs

{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving #-}

-- | Vector library for HGamer3D
module HGamer3D.Data.Vector
(
	
        -- * the Vect libray of Balazs Komuves, see: <http://hackage.haskell.org/package/vect>
        
        module Data.Vect.Float,
		module Data.Vect.Float.Util.Quaternion,

        -- * some constants

        zeroVec2,
        unitVec2,

        zeroVec3,
        unitVec3

)

where

import Data.Vect.Float
import Data.Vect.Float.Util.Quaternion
import Data.MessagePack
import HGamer3D.Data.Component

deriving instance Eq UnitQuaternion
deriving instance Eq Vec2
deriving instance Eq Vec3
deriving instance Eq Vec4

instance ComponentClass Vec2 where
	toObj (Vec2 x y) = ObjectArray [ObjectFloat x, ObjectFloat y]
	fromObj (ObjectArray [ObjectFloat x, ObjectFloat y]) = (Vec2 x y)

instance ComponentClass Vec3 where
	toObj (Vec3 x y z) = ObjectArray [ObjectFloat x, ObjectFloat y, ObjectFloat z]
	fromObj (ObjectArray [ObjectFloat x, ObjectFloat y, ObjectFloat z]) = (Vec3 x y z)

instance ComponentClass Vec4 where
	toObj (Vec4 w x y z) = ObjectArray [ObjectFloat w, ObjectFloat x, ObjectFloat y, ObjectFloat z]
	fromObj (ObjectArray [ObjectFloat w, ObjectFloat x, ObjectFloat y, ObjectFloat z]) = (Vec4 w x y z)

instance ComponentClass Quaternion where
	toObj (Q v) = toObj v
	fromObj o = Q (fromObj o)	

instance ComponentClass UnitQuaternion where
	toObj (U v) = toObj v
	fromObj o = U (fromObj o)	

-- | the 2d zero vector
zeroVec2 = Vec2 0.0 0.0

-- | the 2d unity vector
unitVec2 = Vec2 1.0 1.0

-- | the 3d Zero vector
zeroVec3 = Vec3 0.0 0.0 0.0

-- | the 3d Unity vector
unitVec3 = Vec3 1.0 1.0 1.0

