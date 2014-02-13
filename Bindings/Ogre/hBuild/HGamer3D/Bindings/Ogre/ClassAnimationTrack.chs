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


-- ClassAnimationTrack.chs

-- 

module HGamer3D.Bindings.Ogre.ClassAnimationTrack where

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

#include "ClassAnimationTrack.h"
{- function ~AnimationTrack -}
{#fun ogre_at_destruct as delete 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function getHandle -}
{#fun ogre_at_getHandle as getHandle 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function getNumKeyFrames -}
{#fun ogre_at_getNumKeyFrames as getNumKeyFrames 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function removeKeyFrame -}
{#fun ogre_at_removeKeyFrame as removeKeyFrame 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' } -> `()'  #}

{- function removeAllKeyFrames -}
{#fun ogre_at_removeAllKeyFrames as removeAllKeyFrames 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function apply -}
{#fun ogre_at_apply as apply 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' ,
 realToFrac `Float' ,
 realToFrac `Float' } -> `()'  #}

{- function hasNonZeroKeyFrames -}
{#fun ogre_at_hasNonZeroKeyFrames as hasNonZeroKeyFrames 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function optimise -}
{#fun ogre_at_optimise as optimise 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function getParent -}
{#fun ogre_at_getParent as getParent 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

