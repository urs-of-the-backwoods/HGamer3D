{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving, DeriveDataTypeable #-}

-- This source file is part of HGamer3D
-- (A project to enable 3D game development in Haskell)
-- For the latest info, see http://www.hgamer3d.org
--
-- (c) 2011-2014 Peter Althainz
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


-- | Vector library for HGamer3D
module HGamer3D.Data.Vector
(
	
        -- * the Vect libray of Balazs Komuves, see: <http://hackage.haskell.org/package/vect>
        
        module Data.Vect.Float,
	module Data.Vect.Float.Util.Quaternion,

        -- * some constants

        zeroVec3,
        unitVec2,

        zeroVec3,
        unitVec3

)

where

import Data.Typeable
import Data.Vect.Float
import Data.Vect.Float.Util.Quaternion

deriving instance Eq UnitQuaternion
deriving instance Eq Vec2
deriving instance Eq Vec3
deriving instance Eq Vec4

deriving instance Typeable UnitQuaternion
deriving instance Typeable Vec2
deriving instance Typeable Vec3
deriving instance Typeable Vec4

-- | the 2d zero vector
zeroVec2 = Vec2 0.0 0.0

-- | the 2d unity vector
unitVec2 = Vec2 1.0 1.0

-- | the 3d Zero vector
zeroVec3 = Vec3 0.0 0.0 0.0

-- | the 3d Unity vector
unitVec3 = Vec3 1.0 1.0 1.0

