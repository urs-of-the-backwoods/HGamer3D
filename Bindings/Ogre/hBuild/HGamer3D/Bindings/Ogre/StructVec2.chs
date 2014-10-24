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


-- StructVec2.chs

-- 

module HGamer3D.Bindings.Ogre.StructVec2 where

import Foreign
import Foreign.Ptr
import Foreign.C

import HGamer3D.Data.HG3DClass
import HGamer3D.Data.Vector
import HGamer3D.Data.Colour
import HGamer3D.Data.Angle


#include "StructVec2.h"

import Data.Bits
import HGamer3D.Data.Vector

newtype Vector2 = Vector2 Vec2

instance Storable Vector2 where
  alignment _ = alignment (undefined :: CDouble)
  sizeOf _ = {#sizeof vector2_struct#}
  peek p = do
	x <- {#get vector2_struct.x #} p
	y <- {#get vector2_struct.y #} p
	let v = Vector2 ( Vec2 (realToFrac x) (realToFrac y))
	return v
  poke p (Vector2 (Vec2 x y)) = do
    {#set vector2_struct.x #} p (realToFrac x)
    {#set vector2_struct.y #} p (realToFrac y)

type Vector2Ptr = Ptr (Vector2)
    
{#pointer *vector2_struct as Vec2Ptr -> Vec2 #}

withVec2 :: Vec2 -> (Vec2Ptr -> IO b) -> IO b
withVec2 v f = with v' f' where
     v' = Vector2 v
     f' p = f (castPtr p)
peekVec2 :: Vec2Ptr -> IO Vec2
peekVec2 p = do
   (Vector2 v2) <- peek ((castPtr p)::Vector2Ptr)
   return v2


