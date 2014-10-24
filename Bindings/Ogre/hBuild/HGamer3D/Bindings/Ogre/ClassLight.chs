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


-- ClassLight.chs

-- 

module HGamer3D.Bindings.Ogre.ClassLight where

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
{# import HGamer3D.Bindings.Ogre.EnumLightType #}
{# import HGamer3D.Bindings.Ogre.StructColour #}
{# import HGamer3D.Bindings.Ogre.StructVec3 #}
{# import HGamer3D.Bindings.Ogre.StructRadians #}

#include "ClassLight.h"
{- function Light2 -}
{#fun ogre_lgt_construct as new 
{ withCString* `String' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function ~Light -}
{#fun ogre_lgt_destruct as delete 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function setType -}
{#fun ogre_lgt_setType as setType 
{ withHG3DClass* `HG3DClass' ,
 cIntFromEnum `EnumLightType' } -> `()'  #}

{- function getType -}
{#fun ogre_lgt_getType as getType 
{ withHG3DClass* `HG3DClass' ,
 alloca- `EnumLightType' peekEnumUtil*} -> `()'  #}

{- function setDiffuseColour -}
{#fun ogre_lgt_setDiffuseColour as setDiffuseColour 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' ,
 realToFrac `Float' ,
 realToFrac `Float' } -> `()'  #}

{- function setDiffuseColour2 -}
{#fun ogre_lgt_setDiffuseColour2 as setDiffuseColour2 
{ withHG3DClass* `HG3DClass' ,
 withColour* `Colour' } -> `()'  #}

{- function getDiffuseColour -}
{#fun ogre_lgt_getDiffuseColour as getDiffuseColour 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Colour' peekColour*} -> `()'  #}

{- function setSpecularColour -}
{#fun ogre_lgt_setSpecularColour as setSpecularColour 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' ,
 realToFrac `Float' ,
 realToFrac `Float' } -> `()'  #}

{- function setSpecularColour2 -}
{#fun ogre_lgt_setSpecularColour2 as setSpecularColour2 
{ withHG3DClass* `HG3DClass' ,
 withColour* `Colour' } -> `()'  #}

{- function getSpecularColour -}
{#fun ogre_lgt_getSpecularColour as getSpecularColour 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Colour' peekColour*} -> `()'  #}

{- function setAttenuation -}
{#fun ogre_lgt_setAttenuation as setAttenuation 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' ,
 realToFrac `Float' ,
 realToFrac `Float' ,
 realToFrac `Float' } -> `()'  #}

{- function getAttenuationRange -}
{#fun ogre_lgt_getAttenuationRange as getAttenuationRange 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function getAttenuationConstant -}
{#fun ogre_lgt_getAttenuationConstant as getAttenuationConstant 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function getAttenuationLinear -}
{#fun ogre_lgt_getAttenuationLinear as getAttenuationLinear 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function getAttenuationQuadric -}
{#fun ogre_lgt_getAttenuationQuadric as getAttenuationQuadric 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function setPosition -}
{#fun ogre_lgt_setPosition as setPosition 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' ,
 realToFrac `Float' ,
 realToFrac `Float' } -> `()'  #}

{- function setPosition2 -}
{#fun ogre_lgt_setPosition2 as setPosition2 
{ withHG3DClass* `HG3DClass' ,
 withVec3* `Vec3' } -> `()'  #}

{- function getPosition -}
{#fun ogre_lgt_getPosition as getPosition 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Vec3' peekVec3*} -> `()'  #}

{- function setDirection -}
{#fun ogre_lgt_setDirection as setDirection 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' ,
 realToFrac `Float' ,
 realToFrac `Float' } -> `()'  #}

{- function setDirection2 -}
{#fun ogre_lgt_setDirection2 as setDirection2 
{ withHG3DClass* `HG3DClass' ,
 withVec3* `Vec3' } -> `()'  #}

{- function getDirection -}
{#fun ogre_lgt_getDirection as getDirection 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Vec3' peekVec3*} -> `()'  #}

{- function setSpotlightRange -}
{#fun ogre_lgt_setSpotlightRange as setSpotlightRange 
{ withHG3DClass* `HG3DClass' ,
 withRadians* `Radians' ,
 withRadians* `Radians' ,
 realToFrac `Float' } -> `()'  #}

{- function getSpotlightInnerAngle -}
{#fun ogre_lgt_getSpotlightInnerAngle as getSpotlightInnerAngle 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Radians' peekRadians*} -> `()'  #}

{- function getSpotlightOuterAngle -}
{#fun ogre_lgt_getSpotlightOuterAngle as getSpotlightOuterAngle 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Radians' peekRadians*} -> `()'  #}

{- function getSpotlightFalloff -}
{#fun ogre_lgt_getSpotlightFalloff as getSpotlightFalloff 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function setSpotlightInnerAngle -}
{#fun ogre_lgt_setSpotlightInnerAngle as setSpotlightInnerAngle 
{ withHG3DClass* `HG3DClass' ,
 withRadians* `Radians' } -> `()'  #}

{- function setSpotlightOuterAngle -}
{#fun ogre_lgt_setSpotlightOuterAngle as setSpotlightOuterAngle 
{ withHG3DClass* `HG3DClass' ,
 withRadians* `Radians' } -> `()'  #}

{- function setSpotlightFalloff -}
{#fun ogre_lgt_setSpotlightFalloff as setSpotlightFalloff 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' } -> `()'  #}

{- function setSpotlightNearClipDistance -}
{#fun ogre_lgt_setSpotlightNearClipDistance as setSpotlightNearClipDistance 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' } -> `()'  #}

{- function getSpotlightNearClipDistance -}
{#fun ogre_lgt_getSpotlightNearClipDistance as getSpotlightNearClipDistance 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function setPowerScale -}
{#fun ogre_lgt_setPowerScale as setPowerScale 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' } -> `()'  #}

{- function getPowerScale -}
{#fun ogre_lgt_getPowerScale as getPowerScale 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function getMovableType -}
{#fun ogre_lgt_getMovableType as getMovableType 
{ withHG3DClass* `HG3DClass' ,
 alloc64k- `String' peekCString*} -> `()'  #}

{- function getDerivedPosition -}
{#fun ogre_lgt_getDerivedPosition as getDerivedPosition 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' ,
 alloca- `Vec3' peekVec3*} -> `()'  #}

{- function getDerivedDirection -}
{#fun ogre_lgt_getDerivedDirection as getDerivedDirection 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Vec3' peekVec3*} -> `()'  #}

{- function setVisible -}
{#fun ogre_lgt_setVisible as setVisible 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function getBoundingRadius -}
{#fun ogre_lgt_getBoundingRadius as getBoundingRadius 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function getTypeFlags -}
{#fun ogre_lgt_getTypeFlags as getTypeFlags 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function resetCustomShadowCameraSetup -}
{#fun ogre_lgt_resetCustomShadowCameraSetup as resetCustomShadowCameraSetup 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function setShadowFarDistance -}
{#fun ogre_lgt_setShadowFarDistance as setShadowFarDistance 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' } -> `()'  #}

{- function resetShadowFarDistance -}
{#fun ogre_lgt_resetShadowFarDistance as resetShadowFarDistance 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function getShadowFarDistance -}
{#fun ogre_lgt_getShadowFarDistance as getShadowFarDistance 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function getShadowFarDistanceSquared -}
{#fun ogre_lgt_getShadowFarDistanceSquared as getShadowFarDistanceSquared 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function setShadowNearClipDistance -}
{#fun ogre_lgt_setShadowNearClipDistance as setShadowNearClipDistance 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' } -> `()'  #}

{- function getShadowNearClipDistance -}
{#fun ogre_lgt_getShadowNearClipDistance as getShadowNearClipDistance 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function setShadowFarClipDistance -}
{#fun ogre_lgt_setShadowFarClipDistance as setShadowFarClipDistance 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' } -> `()'  #}

{- function getShadowFarClipDistance -}
{#fun ogre_lgt_getShadowFarClipDistance as getShadowFarClipDistance 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

