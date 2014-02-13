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


-- ClassCamera.chs

-- 

module HGamer3D.Bindings.Ogre.ClassCamera where

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
{# import HGamer3D.Bindings.Ogre.StructVec3 #}
{# import HGamer3D.Bindings.Ogre.StructRadians #}
{# import HGamer3D.Bindings.Ogre.StructQuaternion #}
{# import HGamer3D.Bindings.Ogre.EnumFrustumPlane #}

#include "ClassCamera.h"
{- function Camera -}
{#fun ogre_cam_construct as new 
{ withCString* `String' ,
 withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function ~Camera -}
{#fun ogre_cam_destruct as delete 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function getSceneManager -}
{#fun ogre_cam_getSceneManager as getSceneManager 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function setPosition -}
{#fun ogre_cam_setPosition as setPosition 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' ,
 realToFrac `Float' ,
 realToFrac `Float' } -> `()'  #}

{- function setPosition2 -}
{#fun ogre_cam_setPosition2 as setPosition2 
{ withHG3DClass* `HG3DClass' ,
 withVec3* `Vec3' } -> `()'  #}

{- function getPosition -}
{#fun ogre_cam_getPosition as getPosition 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Vec3' peekVec3*} -> `()'  #}

{- function move -}
{#fun ogre_cam_move as move 
{ withHG3DClass* `HG3DClass' ,
 withVec3* `Vec3' } -> `()'  #}

{- function moveRelative -}
{#fun ogre_cam_moveRelative as moveRelative 
{ withHG3DClass* `HG3DClass' ,
 withVec3* `Vec3' } -> `()'  #}

{- function setDirection -}
{#fun ogre_cam_setDirection as setDirection 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' ,
 realToFrac `Float' ,
 realToFrac `Float' } -> `()'  #}

{- function setDirection2 -}
{#fun ogre_cam_setDirection2 as setDirection2 
{ withHG3DClass* `HG3DClass' ,
 withVec3* `Vec3' } -> `()'  #}

{- function getDirection -}
{#fun ogre_cam_getDirection as getDirection 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Vec3' peekVec3*} -> `()'  #}

{- function getUp -}
{#fun ogre_cam_getUp as getUp 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Vec3' peekVec3*} -> `()'  #}

{- function getRight -}
{#fun ogre_cam_getRight as getRight 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Vec3' peekVec3*} -> `()'  #}

{- function lookAt -}
{#fun ogre_cam_lookAt as lookAt 
{ withHG3DClass* `HG3DClass' ,
 withVec3* `Vec3' } -> `()'  #}

{- function lookAt2 -}
{#fun ogre_cam_lookAt2 as lookAt2 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' ,
 realToFrac `Float' ,
 realToFrac `Float' } -> `()'  #}

{- function roll -}
{#fun ogre_cam_roll as roll 
{ withHG3DClass* `HG3DClass' ,
 withRadians* `Radians' } -> `()'  #}

{- function yaw -}
{#fun ogre_cam_yaw as yaw 
{ withHG3DClass* `HG3DClass' ,
 withRadians* `Radians' } -> `()'  #}

{- function pitch -}
{#fun ogre_cam_pitch as pitch 
{ withHG3DClass* `HG3DClass' ,
 withRadians* `Radians' } -> `()'  #}

{- function rotate -}
{#fun ogre_cam_rotate as rotate 
{ withHG3DClass* `HG3DClass' ,
 withVec3* `Vec3' ,
 withRadians* `Radians' } -> `()'  #}

{- function rotate2 -}
{#fun ogre_cam_rotate2 as rotate2 
{ withHG3DClass* `HG3DClass' ,
 withQuaternion* `Quaternion' } -> `()'  #}

{- function setFixedYawAxis -}
{#fun ogre_cam_setFixedYawAxis as setFixedYawAxis 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' ,
 withVec3* `Vec3' } -> `()'  #}

{- function getOrientation -}
{#fun ogre_cam_getOrientation as getOrientation 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Quaternion' peekQuaternion*} -> `()'  #}

{- function setOrientation -}
{#fun ogre_cam_setOrientation as setOrientation 
{ withHG3DClass* `HG3DClass' ,
 withQuaternion* `Quaternion' } -> `()'  #}

{- function getDerivedOrientation -}
{#fun ogre_cam_getDerivedOrientation as getDerivedOrientation 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Quaternion' peekQuaternion*} -> `()'  #}

{- function getDerivedPosition -}
{#fun ogre_cam_getDerivedPosition as getDerivedPosition 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Vec3' peekVec3*} -> `()'  #}

{- function getDerivedDirection -}
{#fun ogre_cam_getDerivedDirection as getDerivedDirection 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Vec3' peekVec3*} -> `()'  #}

{- function getDerivedUp -}
{#fun ogre_cam_getDerivedUp as getDerivedUp 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Vec3' peekVec3*} -> `()'  #}

{- function getDerivedRight -}
{#fun ogre_cam_getDerivedRight as getDerivedRight 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Vec3' peekVec3*} -> `()'  #}

{- function getRealOrientation -}
{#fun ogre_cam_getRealOrientation as getRealOrientation 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Quaternion' peekQuaternion*} -> `()'  #}

{- function getRealPosition -}
{#fun ogre_cam_getRealPosition as getRealPosition 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Vec3' peekVec3*} -> `()'  #}

{- function getRealDirection -}
{#fun ogre_cam_getRealDirection as getRealDirection 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Vec3' peekVec3*} -> `()'  #}

{- function getRealUp -}
{#fun ogre_cam_getRealUp as getRealUp 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Vec3' peekVec3*} -> `()'  #}

{- function getRealRight -}
{#fun ogre_cam_getRealRight as getRealRight 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Vec3' peekVec3*} -> `()'  #}

{- function getMovableType -}
{#fun ogre_cam_getMovableType as getMovableType 
{ withHG3DClass* `HG3DClass' ,
 alloc64k- `String' peekCString*} -> `()'  #}

{- function setLodBias -}
{#fun ogre_cam_setLodBias as setLodBias 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' } -> `()'  #}

{- function getLodBias -}
{#fun ogre_cam_getLodBias as getLodBias 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function setLodCamera -}
{#fun ogre_cam_setLodCamera as setLodCamera 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function getLodCamera -}
{#fun ogre_cam_getLodCamera as getLodCamera 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function setWindow -}
{#fun ogre_cam_setWindow as setWindow 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' ,
 realToFrac `Float' ,
 realToFrac `Float' ,
 realToFrac `Float' } -> `()'  #}

{- function resetWindow -}
{#fun ogre_cam_resetWindow as resetWindow 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function isWindowSet -}
{#fun ogre_cam_isWindowSet as isWindowSet 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function getBoundingRadius -}
{#fun ogre_cam_getBoundingRadius as getBoundingRadius 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function getAutoTrackTarget -}
{#fun ogre_cam_getAutoTrackTarget as getAutoTrackTarget 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getAutoTrackOffset -}
{#fun ogre_cam_getAutoTrackOffset as getAutoTrackOffset 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Vec3' peekVec3*} -> `()'  #}

{- function getViewport -}
{#fun ogre_cam_getViewport as getViewport 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function setAutoAspectRatio -}
{#fun ogre_cam_setAutoAspectRatio as setAutoAspectRatio 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function getAutoAspectRatio -}
{#fun ogre_cam_getAutoAspectRatio as getAutoAspectRatio 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function setCullingFrustum -}
{#fun ogre_cam_setCullingFrustum as setCullingFrustum 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function getCullingFrustum -}
{#fun ogre_cam_getCullingFrustum as getCullingFrustum 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function isVisible3 -}
{#fun ogre_cam_isVisible3 as isVisible3 
{ withHG3DClass* `HG3DClass' ,
 withVec3* `Vec3' ,
 alloca- `EnumFrustumPlane' peekEnumUtil*,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function getWorldSpaceCorners -}
{#fun ogre_cam_getWorldSpaceCorners as getWorldSpaceCorners 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Vec3' peekVec3*} -> `()'  #}

{- function getNearClipDistance -}
{#fun ogre_cam_getNearClipDistance as getNearClipDistance 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function getFarClipDistance -}
{#fun ogre_cam_getFarClipDistance as getFarClipDistance 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function setUseRenderingDistance -}
{#fun ogre_cam_setUseRenderingDistance as setUseRenderingDistance 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function getUseRenderingDistance -}
{#fun ogre_cam_getUseRenderingDistance as getUseRenderingDistance 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function synchroniseBaseSettingsWith -}
{#fun ogre_cam_synchroniseBaseSettingsWith as synchroniseBaseSettingsWith 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function getPositionForViewUpdate -}
{#fun ogre_cam_getPositionForViewUpdate as getPositionForViewUpdate 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Vec3' peekVec3*} -> `()'  #}

{- function getOrientationForViewUpdate -}
{#fun ogre_cam_getOrientationForViewUpdate as getOrientationForViewUpdate 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Quaternion' peekQuaternion*} -> `()'  #}

{- function setUseMinPixelSize -}
{#fun ogre_cam_setUseMinPixelSize as setUseMinPixelSize 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function getUseMinPixelSize -}
{#fun ogre_cam_getUseMinPixelSize as getUseMinPixelSize 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function getPixelDisplayRatio -}
{#fun ogre_cam_getPixelDisplayRatio as getPixelDisplayRatio 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

