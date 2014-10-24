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


-- StructDegrees.chs

-- 

module HGamer3D.Bindings.Ogre.StructDegrees where

import Foreign
import Foreign.Ptr
import Foreign.C

import HGamer3D.Data.HG3DClass
import HGamer3D.Data.Vector
import HGamer3D.Data.Colour
import HGamer3D.Data.Angle


#include "StructDegrees.h"

import Data.Bits
import HGamer3D.Data.Angle

instance Storable Degrees where
  alignment _ = alignment (undefined :: CDouble)
  sizeOf _ = {#sizeof degree_struct#}
  peek p = do
	d <- {#get degree_struct.d #} p
	let de = Degrees (realToFrac d)
	return de
  poke p (Degrees d) = do
    {#set degree_struct.d #} p (realToFrac d)
  
{#pointer *degree_struct as DegreesPtr -> Degrees #}

withDegrees :: Degrees -> (DegreesPtr -> IO b) -> IO b
withDegrees = with
peekDegrees :: DegreesPtr -> IO Degrees
peekDegrees = peek
  
