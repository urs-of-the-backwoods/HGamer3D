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


-- ClassSkeleton.chs

-- 

module HGamer3D.Bindings.Ogre.ClassSkeleton where

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
{# import HGamer3D.Bindings.Ogre.EnumSkeletonAnimationBlendMode #}

#include "ClassSkeleton.h"
{- function ~Skeleton -}
{#fun ogre_skl_destruct as delete 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function createBone -}
{#fun ogre_skl_createBone as createBone 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function createBone2 -}
{#fun ogre_skl_createBone2 as createBone2 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function createBone3 -}
{#fun ogre_skl_createBone3 as createBone3 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function createBone4 -}
{#fun ogre_skl_createBone4 as createBone4 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 fromIntegral `Int' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getNumBones -}
{#fun ogre_skl_getNumBones as getNumBones 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function getRootBone -}
{#fun ogre_skl_getRootBone as getRootBone 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getBone -}
{#fun ogre_skl_getBone as getBone 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getBone2 -}
{#fun ogre_skl_getBone2 as getBone2 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function hasBone -}
{#fun ogre_skl_hasBone as hasBone 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function setBindingPose -}
{#fun ogre_skl_setBindingPose as setBindingPose 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function reset -}
{#fun ogre_skl_reset as reset 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function createAnimation -}
{#fun ogre_skl_createAnimation as createAnimation 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 realToFrac `Float' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getAnimation2 -}
{#fun ogre_skl_getAnimation2 as getAnimation2 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function hasAnimation -}
{#fun ogre_skl_hasAnimation as hasAnimation 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function removeAnimation -}
{#fun ogre_skl_removeAnimation as removeAnimation 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' } -> `()'  #}

{- function setAnimationState -}
{#fun ogre_skl_setAnimationState as setAnimationState 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function getNumAnimations -}
{#fun ogre_skl_getNumAnimations as getNumAnimations 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function getAnimation3 -}
{#fun ogre_skl_getAnimation3 as getAnimation3 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getBlendMode -}
{#fun ogre_skl_getBlendMode as getBlendMode 
{ withHG3DClass* `HG3DClass' ,
 alloca- `EnumSkeletonAnimationBlendMode' peekEnumUtil*} -> `()'  #}

{- function setBlendMode -}
{#fun ogre_skl_setBlendMode as setBlendMode 
{ withHG3DClass* `HG3DClass' ,
 cIntFromEnum `EnumSkeletonAnimationBlendMode' } -> `()'  #}

{- function optimiseAllAnimations -}
{#fun ogre_skl_optimiseAllAnimations as optimiseAllAnimations 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function addLinkedSkeletonAnimationSource -}
{#fun ogre_skl_addLinkedSkeletonAnimationSource as addLinkedSkeletonAnimationSource 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 realToFrac `Float' } -> `()'  #}

{- function removeAllLinkedSkeletonAnimationSources -}
{#fun ogre_skl_removeAllLinkedSkeletonAnimationSources as removeAllLinkedSkeletonAnimationSources 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function getManualBonesDirty -}
{#fun ogre_skl_getManualBonesDirty as getManualBonesDirty 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function hasManualBones -}
{#fun ogre_skl_hasManualBones as hasManualBones 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

