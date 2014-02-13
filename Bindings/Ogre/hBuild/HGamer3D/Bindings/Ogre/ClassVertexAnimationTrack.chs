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


-- ClassVertexAnimationTrack.chs

-- 

module HGamer3D.Bindings.Ogre.ClassVertexAnimationTrack where

import Foreign
import Foreign.Ptr
import Foreign.C

import HGamer3D.Data.HG3DClass
import HGamer3D.Data.Vector
import HGamer3D.Data.Colour
import HGamer3D.Data.Angle

{# import HGamer3D.Bindings.Ogre.Utils #}
{# import HGamer3D.Bindings.Ogre.ClassPtr #}
{# import HGamer3D.Bindings.Ogre.StructHG3DClass #}
{# import HGamer3D.Bindings.Ogre.EnumVertexAnimationType #}
{# import HGamer3D.Bindings.Ogre.EnumVertexAnimationTrackTargetMode #}

#include "ClassVertexAnimationTrack.h"
{- function VertexAnimationTrack -}
{#fun ogre_vat_construct as new 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 cIntFromEnum `EnumVertexAnimationType' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getAnimationType -}
{#fun ogre_vat_getAnimationType as getAnimationType 
{ withHG3DClass* `HG3DClass' ,
 alloca- `EnumVertexAnimationType' peekEnumUtil*} -> `()'  #}

{- function getVertexAnimationIncludesNormals -}
{#fun ogre_vat_getVertexAnimationIncludesNormals as getVertexAnimationIncludesNormals 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function apply -}
{#fun ogre_vat_apply as apply 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' ,
 realToFrac `Float' ,
 realToFrac `Float' } -> `()'  #}

{- function setTargetMode -}
{#fun ogre_vat_setTargetMode as setTargetMode 
{ withHG3DClass* `HG3DClass' ,
 cIntFromEnum `EnumVertexAnimationTrackTargetMode' } -> `()'  #}

{- function getTargetMode -}
{#fun ogre_vat_getTargetMode as getTargetMode 
{ withHG3DClass* `HG3DClass' ,
 alloca- `EnumVertexAnimationTrackTargetMode' peekEnumUtil*} -> `()'  #}

{- function hasNonZeroKeyFrames -}
{#fun ogre_vat_hasNonZeroKeyFrames as hasNonZeroKeyFrames 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function optimise -}
{#fun ogre_vat_optimise as optimise 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

