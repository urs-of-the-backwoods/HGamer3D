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


-- ClassFrustum.chs

-- 

module HGamer3D.Bindings.Ogre.ClassFrustum where

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
{# import HGamer3D.Bindings.Ogre.StructRadians #}
{# import HGamer3D.Bindings.Ogre.StructVec2 #}
{# import HGamer3D.Bindings.Ogre.StructVec3 #}
{# import HGamer3D.Bindings.Ogre.EnumFrustumPlane #}
{# import HGamer3D.Bindings.Ogre.StructSharedPtr #}
{# import HGamer3D.Bindings.Ogre.EnumProjectionType #}
{# import HGamer3D.Bindings.Ogre.StructQuaternion #}
{# import HGamer3D.Bindings.Ogre.EnumOrientationMode #}

#include "ClassFrustum.h"
{- function Frustum -}
{#fun ogre_frst_construct as new 
{ withCString* `String' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function ~Frustum -}
{#fun ogre_frst_destruct as delete 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function setFOVy -}
{#fun ogre_frst_setFOVy as setFOVy 
{ withHG3DClass* `HG3DClass' ,
 withRadians* `Radians' } -> `()'  #}

{- function getFOVy -}
{#fun ogre_frst_getFOVy as getFOVy 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Radians' peekRadians*} -> `()'  #}

{- function setNearClipDistance -}
{#fun ogre_frst_setNearClipDistance as setNearClipDistance 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' } -> `()'  #}

{- function getNearClipDistance -}
{#fun ogre_frst_getNearClipDistance as getNearClipDistance 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function setFarClipDistance -}
{#fun ogre_frst_setFarClipDistance as setFarClipDistance 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' } -> `()'  #}

{- function getFarClipDistance -}
{#fun ogre_frst_getFarClipDistance as getFarClipDistance 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function setAspectRatio -}
{#fun ogre_frst_setAspectRatio as setAspectRatio 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' } -> `()'  #}

{- function getAspectRatio -}
{#fun ogre_frst_getAspectRatio as getAspectRatio 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function setFrustumOffset -}
{#fun ogre_frst_setFrustumOffset as setFrustumOffset 
{ withHG3DClass* `HG3DClass' ,
 withVec2* `Vec2' } -> `()'  #}

{- function setFrustumOffset2 -}
{#fun ogre_frst_setFrustumOffset2 as setFrustumOffset2 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' ,
 realToFrac `Float' } -> `()'  #}

{- function getFrustumOffset -}
{#fun ogre_frst_getFrustumOffset as getFrustumOffset 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Vec2' peekVec2*} -> `()'  #}

{- function setFocalLength -}
{#fun ogre_frst_setFocalLength as setFocalLength 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' } -> `()'  #}

{- function getFocalLength -}
{#fun ogre_frst_getFocalLength as getFocalLength 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function setFrustumExtents -}
{#fun ogre_frst_setFrustumExtents as setFrustumExtents 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' ,
 realToFrac `Float' ,
 realToFrac `Float' ,
 realToFrac `Float' } -> `()'  #}

{- function resetFrustumExtents -}
{#fun ogre_frst_resetFrustumExtents as resetFrustumExtents 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function getFrustumExtents -}
{#fun ogre_frst_getFrustumExtents as getFrustumExtents 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*,
 alloca- `Float' peekFloatConv*,
 alloca- `Float' peekFloatConv*,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function isCustomViewMatrixEnabled -}
{#fun ogre_frst_isCustomViewMatrixEnabled as isCustomViewMatrixEnabled 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isCustomProjectionMatrixEnabled -}
{#fun ogre_frst_isCustomProjectionMatrixEnabled as isCustomProjectionMatrixEnabled 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isVisible3 -}
{#fun ogre_frst_isVisible3 as isVisible3 
{ withHG3DClass* `HG3DClass' ,
 withVec3* `Vec3' ,
 alloca- `EnumFrustumPlane' peekEnumUtil*,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function getTypeFlags -}
{#fun ogre_frst_getTypeFlags as getTypeFlags 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function getBoundingRadius -}
{#fun ogre_frst_getBoundingRadius as getBoundingRadius 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function getMovableType -}
{#fun ogre_frst_getMovableType as getMovableType 
{ withHG3DClass* `HG3DClass' ,
 alloc64k- `String' peekCString*} -> `()'  #}

{- function getMaterial -}
{#fun ogre_frst_getMaterial as getMaterial 
{ withHG3DClass* `HG3DClass' ,
 alloca- `SharedPtr' peekSharedPtr*} -> `()'  #}

{- function getSquaredViewDepth -}
{#fun ogre_frst_getSquaredViewDepth as getSquaredViewDepth 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function getWorldSpaceCorners -}
{#fun ogre_frst_getWorldSpaceCorners as getWorldSpaceCorners 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Vec3' peekVec3*} -> `()'  #}

{- function setProjectionType -}
{#fun ogre_frst_setProjectionType as setProjectionType 
{ withHG3DClass* `HG3DClass' ,
 cIntFromEnum `EnumProjectionType' } -> `()'  #}

{- function getProjectionType -}
{#fun ogre_frst_getProjectionType as getProjectionType 
{ withHG3DClass* `HG3DClass' ,
 alloca- `EnumProjectionType' peekEnumUtil*} -> `()'  #}

{- function setOrthoWindow -}
{#fun ogre_frst_setOrthoWindow as setOrthoWindow 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' ,
 realToFrac `Float' } -> `()'  #}

{- function setOrthoWindowHeight -}
{#fun ogre_frst_setOrthoWindowHeight as setOrthoWindowHeight 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' } -> `()'  #}

{- function setOrthoWindowWidth -}
{#fun ogre_frst_setOrthoWindowWidth as setOrthoWindowWidth 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' } -> `()'  #}

{- function getOrthoWindowHeight -}
{#fun ogre_frst_getOrthoWindowHeight as getOrthoWindowHeight 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function getOrthoWindowWidth -}
{#fun ogre_frst_getOrthoWindowWidth as getOrthoWindowWidth 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function disableReflection -}
{#fun ogre_frst_disableReflection as disableReflection 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function isReflected -}
{#fun ogre_frst_isReflected as isReflected 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function disableCustomNearClipPlane -}
{#fun ogre_frst_disableCustomNearClipPlane as disableCustomNearClipPlane 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function isCustomNearClipPlaneEnabled -}
{#fun ogre_frst_isCustomNearClipPlaneEnabled as isCustomNearClipPlaneEnabled 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function getPositionForViewUpdate -}
{#fun ogre_frst_getPositionForViewUpdate as getPositionForViewUpdate 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Vec3' peekVec3*} -> `()'  #}

{- function getOrientationForViewUpdate -}
{#fun ogre_frst_getOrientationForViewUpdate as getOrientationForViewUpdate 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Quaternion' peekQuaternion*} -> `()'  #}

{- function setOrientationMode -}
{#fun ogre_frst_setOrientationMode as setOrientationMode 
{ withHG3DClass* `HG3DClass' ,
 cIntFromEnum `EnumOrientationMode' } -> `()'  #}

{- function getOrientationMode -}
{#fun ogre_frst_getOrientationMode as getOrientationMode 
{ withHG3DClass* `HG3DClass' ,
 alloca- `EnumOrientationMode' peekEnumUtil*} -> `()'  #}

