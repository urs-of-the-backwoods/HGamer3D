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

import Fresco
import Data.Binary.Serialise.CBOR
import Data.Binary.Serialise.CBOR.Decoding

import Data.Text
import Data.Monoid
import Control.Applicative

deriving instance Eq UnitQuaternion
deriving instance Eq Vec2
deriving instance Eq Vec3
deriving instance Eq Vec4


-- | the 2d zero vector
zeroVec2 = Vec2 0.0 0.0

-- | the 2d unity vector
unitVec2 = Vec2 1.0 1.0

-- | the 3d Zero vector
zeroVec3 = Vec3 0.0 0.0 0.0

-- | the 3d Unity vector
unitVec3 = Vec3 1.0 1.0 1.0

instance Serialise Vec3 where
    encode (Vec3 v1 v2 v3) = encode v1 <> encode v2 <> encode v3
    decode = Vec3 <$> decode <*> decode <*> decode

instance Serialise UnitQuaternion where
    encode (U (Vec4 v1 v2 v3 v4)) = encode v1 <> encode v2 <> encode v3 <> encode v4
    decode = do
        v4 <- Vec4 <$> decode <*> decode <*> decode <*> decode
        return $ U v4

