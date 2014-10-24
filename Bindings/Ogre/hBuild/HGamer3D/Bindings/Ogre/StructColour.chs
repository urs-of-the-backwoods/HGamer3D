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


-- StructColour.chs

-- 

module HGamer3D.Bindings.Ogre.StructColour where

import Foreign
import Foreign.Ptr
import Foreign.C

import HGamer3D.Data.HG3DClass
import HGamer3D.Data.Vector
import HGamer3D.Data.Colour
import HGamer3D.Data.Angle


#include "StructColour.h"

import Data.Bits
import HGamer3D.Data.Colour

instance Storable Colour where
  alignment _ = alignment (undefined :: CDouble)
  sizeOf _ = {#sizeof colourvalue_struct#}
  peek p = do
	r <- {#get colourvalue_struct.r #} p
	g <- {#get colourvalue_struct.g #} p
	b <- {#get colourvalue_struct.b #} p
	a <- {#get colourvalue_struct.a #} p
	let cv = Colour (realToFrac r) (realToFrac g) (realToFrac b) (realToFrac a)
	return cv
  poke p (Colour r g b a) = do
    {#set colourvalue_struct.r #} p (realToFrac r)
    {#set colourvalue_struct.g #} p (realToFrac g)
    {#set colourvalue_struct.b #} p (realToFrac b)
    {#set colourvalue_struct.a #} p (realToFrac a)
    
{#pointer *colourvalue_struct as ColourPtr -> Colour #}

withColour :: Colour -> (ColourPtr -> IO b) -> IO b
withColour = with
peekColour :: ColourPtr -> IO Colour
peekColour = peek

