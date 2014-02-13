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


-- ClassAnimation.chs

-- 

module HGamer3D.Bindings.Ogre.ClassAnimation where

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
{# import HGamer3D.Bindings.Ogre.EnumAnimationInterpolationMode #}
{# import HGamer3D.Bindings.Ogre.EnumAnimationRotationInterpolationMode #}

#include "ClassAnimation.h"
{- function ~Animation -}
{#fun ogre_anm_destruct as delete 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function getName -}
{#fun ogre_anm_getName as getName 
{ withHG3DClass* `HG3DClass' ,
 alloc64k- `String' peekCString*} -> `()'  #}

{- function getLength -}
{#fun ogre_anm_getLength as getLength 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function setLength -}
{#fun ogre_anm_setLength as setLength 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' } -> `()'  #}

{- function createNodeTrack -}
{#fun ogre_anm_createNodeTrack as createNodeTrack 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function createNumericTrack -}
{#fun ogre_anm_createNumericTrack as createNumericTrack 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function createVertexTrack -}
{#fun ogre_anm_createVertexTrack as createVertexTrack 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 cIntFromEnum `EnumVertexAnimationType' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function createNodeTrack2 -}
{#fun ogre_anm_createNodeTrack2 as createNodeTrack2 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getNumNodeTracks -}
{#fun ogre_anm_getNumNodeTracks as getNumNodeTracks 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function getNodeTrack -}
{#fun ogre_anm_getNodeTrack as getNodeTrack 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function hasNodeTrack -}
{#fun ogre_anm_hasNodeTrack as hasNodeTrack 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function getNumNumericTracks -}
{#fun ogre_anm_getNumNumericTracks as getNumNumericTracks 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function getNumericTrack -}
{#fun ogre_anm_getNumericTrack as getNumericTrack 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function hasNumericTrack -}
{#fun ogre_anm_hasNumericTrack as hasNumericTrack 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function getNumVertexTracks -}
{#fun ogre_anm_getNumVertexTracks as getNumVertexTracks 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function getVertexTrack -}
{#fun ogre_anm_getVertexTrack as getVertexTrack 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function hasVertexTrack -}
{#fun ogre_anm_hasVertexTrack as hasVertexTrack 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function destroyNodeTrack -}
{#fun ogre_anm_destroyNodeTrack as destroyNodeTrack 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' } -> `()'  #}

{- function destroyNumericTrack -}
{#fun ogre_anm_destroyNumericTrack as destroyNumericTrack 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' } -> `()'  #}

{- function destroyVertexTrack -}
{#fun ogre_anm_destroyVertexTrack as destroyVertexTrack 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' } -> `()'  #}

{- function destroyAllTracks -}
{#fun ogre_anm_destroyAllTracks as destroyAllTracks 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function destroyAllNodeTracks -}
{#fun ogre_anm_destroyAllNodeTracks as destroyAllNodeTracks 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function destroyAllNumericTracks -}
{#fun ogre_anm_destroyAllNumericTracks as destroyAllNumericTracks 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function destroyAllVertexTracks -}
{#fun ogre_anm_destroyAllVertexTracks as destroyAllVertexTracks 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function apply -}
{#fun ogre_anm_apply as apply 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' ,
 realToFrac `Float' ,
 realToFrac `Float' } -> `()'  #}

{- function applyToNode -}
{#fun ogre_anm_applyToNode as applyToNode 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' ,
 realToFrac `Float' ,
 realToFrac `Float' ,
 realToFrac `Float' } -> `()'  #}

{- function apply2 -}
{#fun ogre_anm_apply2 as apply2 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' ,
 realToFrac `Float' ,
 realToFrac `Float' ,
 realToFrac `Float' } -> `()'  #}

{- function apply4 -}
{#fun ogre_anm_apply4 as apply4 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' ,
 realToFrac `Float' ,
 realToFrac `Float' ,
 fromBool `Bool' ,
 fromBool `Bool' } -> `()'  #}

{- function setInterpolationMode -}
{#fun ogre_anm_setInterpolationMode as setInterpolationMode 
{ withHG3DClass* `HG3DClass' ,
 cIntFromEnum `EnumAnimationInterpolationMode' } -> `()'  #}

{- function getInterpolationMode -}
{#fun ogre_anm_getInterpolationMode as getInterpolationMode 
{ withHG3DClass* `HG3DClass' ,
 alloca- `EnumAnimationInterpolationMode' peekEnumUtil*} -> `()'  #}

{- function setRotationInterpolationMode -}
{#fun ogre_anm_setRotationInterpolationMode as setRotationInterpolationMode 
{ withHG3DClass* `HG3DClass' ,
 cIntFromEnum `EnumAnimationRotationInterpolationMode' } -> `()'  #}

{- function getRotationInterpolationMode -}
{#fun ogre_anm_getRotationInterpolationMode as getRotationInterpolationMode 
{ withHG3DClass* `HG3DClass' ,
 alloca- `EnumAnimationRotationInterpolationMode' peekEnumUtil*} -> `()'  #}

{- function optimise -}
{#fun ogre_anm_optimise as optimise 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function clone -}
{#fun ogre_anm_clone as clone 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function setUseBaseKeyFrame -}
{#fun ogre_anm_setUseBaseKeyFrame as setUseBaseKeyFrame 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' ,
 realToFrac `Float' ,
 withCString* `String' } -> `()'  #}

{- function getUseBaseKeyFrame -}
{#fun ogre_anm_getUseBaseKeyFrame as getUseBaseKeyFrame 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function getBaseKeyFrameTime -}
{#fun ogre_anm_getBaseKeyFrameTime as getBaseKeyFrameTime 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function getBaseKeyFrameAnimationName -}
{#fun ogre_anm_getBaseKeyFrameAnimationName as getBaseKeyFrameAnimationName 
{ withHG3DClass* `HG3DClass' ,
 alloc64k- `String' peekCString*} -> `()'  #}

{- function setDefaultInterpolationMode -}
{#fun ogre_anm_setDefaultInterpolationMode as setDefaultInterpolationMode 
{ cIntFromEnum `EnumAnimationInterpolationMode' } -> `()'  #}

{- function getDefaultInterpolationMode -}
{#fun ogre_anm_getDefaultInterpolationMode as getDefaultInterpolationMode 
{ alloca- `EnumAnimationInterpolationMode' peekEnumUtil*} -> `()'  #}

{- function setDefaultRotationInterpolationMode -}
{#fun ogre_anm_setDefaultRotationInterpolationMode as setDefaultRotationInterpolationMode 
{ cIntFromEnum `EnumAnimationRotationInterpolationMode' } -> `()'  #}

{- function getDefaultRotationInterpolationMode -}
{#fun ogre_anm_getDefaultRotationInterpolationMode as getDefaultRotationInterpolationMode 
{ alloca- `EnumAnimationRotationInterpolationMode' peekEnumUtil*} -> `()'  #}

