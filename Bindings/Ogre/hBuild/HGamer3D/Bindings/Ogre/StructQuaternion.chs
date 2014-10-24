{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- This source file is part of HGamer3D, a project to enable 3D game development 
-- in Haskell. For the latest info, see http://www.hgamer3d.org .
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
-- 


-- StructQuaternion.chs

-- 

module HGamer3D.Bindings.Ogre.StructQuaternion where

import Foreign
import Foreign.Ptr
import Foreign.C

import HGamer3D.Data.HG3DClass
import HGamer3D.Data.Vector
import HGamer3D.Data.Colour
import HGamer3D.Data.Angle


#include "StructQuaternion.h"

import Data.Bits
import HGamer3D.Data.Vector

newtype Quat = Quat Quaternion

instance Storable Quat where
  alignment _ = alignment (undefined :: CDouble)
  sizeOf _ = {#sizeof quaternion_struct#}
  peek p = do
	fw <- {#get quaternion_struct.fw #} p
	fx <- {#get quaternion_struct.fx #} p
	fy <- {#get quaternion_struct.fy #} p
	fz <- {#get quaternion_struct.fz #} p
	let q = Quat ( Q (Vec4 (realToFrac fw) (realToFrac fx) (realToFrac fy) (realToFrac fz) ))
	return q
  poke p (Quat (Q ( Vec4 w x y z))) = do
    {#set quaternion_struct.fw #} p (realToFrac w)
    {#set quaternion_struct.fx #} p (realToFrac x)
    {#set quaternion_struct.fy #} p (realToFrac y)
    {#set quaternion_struct.fz #} p (realToFrac z)
 
type QuatPtr = Ptr (Quat)

{#pointer *quaternion_struct as QuaternionPtr -> Quaternion #}

withQuaternion :: Quaternion -> (QuaternionPtr -> IO b) -> IO b
withQuaternion q f = with q' f' where
     q' = Quat q
     f' p = f (castPtr p)
peekQuaternion p = do
   (Quat q) <- peek ((castPtr p)::QuatPtr)
   return q



