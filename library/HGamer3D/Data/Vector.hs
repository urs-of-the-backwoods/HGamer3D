{-
    Vector library, imported from package Vect 
	HGamer3D Library (A project to enable 3D game development in Haskell)
	Copyright 2011-2015 Peter Althainz
	
	Distributed under the Apache License, Version 2.0
	(See attached file LICENSE or copy at 
	http://www.apache.org/licenses/LICENSE-2.0)
 
	file: HGamer3D/Data/Vector.hs
-}

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
import Fresco

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

