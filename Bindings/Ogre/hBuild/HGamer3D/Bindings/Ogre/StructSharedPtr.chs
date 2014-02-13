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


-- StructSharedPtr.chs

-- 

module HGamer3D.Bindings.Ogre.StructSharedPtr where

import Foreign
import Foreign.Ptr
import Foreign.C

import HGamer3D.Data.HG3DClass
import HGamer3D.Data.Vector
import HGamer3D.Data.Colour
import HGamer3D.Data.Angle


#include "StructSharedPtr.h"

import Data.Bits

data SharedPtr = SharedPtr {
  spPT :: Ptr (),
  spPCount :: Ptr (),
  spPFreeFunc :: Ptr ()
  } deriving (Eq, Show)

instance Storable SharedPtr where
  alignment _ = alignment (undefined :: CDouble)
  sizeOf _ = {#sizeof sharedptr_struct#}
  peek p = do
	pt <- {#get sharedptr_struct.pT #} p
	pc <- {#get sharedptr_struct.pCount #} p
	pf <- {#get sharedptr_struct.pFreeFunc #} p
	let sp = SharedPtr pt pc pf
	return sp
  poke p (SharedPtr pt pc pf) = do
    {#set sharedptr_struct.pT #} p pt
    {#set sharedptr_struct.pCount #} p pc
    {#set sharedptr_struct.pFreeFunc #} p pf
 
{#pointer *sharedptr_struct as SharedPtrPtr -> SharedPtr #}
   
withSharedPtr :: SharedPtr -> (SharedPtrPtr -> IO b) -> IO b
withSharedPtr = with
peekSharedPtr :: SharedPtrPtr -> IO SharedPtr
peekSharedPtr = peek

