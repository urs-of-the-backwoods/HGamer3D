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


-- ClassMovableObject.chs

-- 

module HGamer3D.Bindings.Ogre.ClassMovableObject where

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

#include "ClassMovableObject.h"
{- function ~MovableObject -}
{#fun ogre_mvo_destruct as delete 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function getName -}
{#fun ogre_mvo_getName as getName 
{ withHG3DClass* `HG3DClass' ,
 alloc64k- `String' peekCString*} -> `()'  #}

{- function getMovableType -}
{#fun ogre_mvo_getMovableType as getMovableType 
{ withHG3DClass* `HG3DClass' ,
 alloc64k- `String' peekCString*} -> `()'  #}

{- function getParentNode -}
{#fun ogre_mvo_getParentNode as getParentNode 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getParentSceneNode -}
{#fun ogre_mvo_getParentSceneNode as getParentSceneNode 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function isParentTagPoint -}
{#fun ogre_mvo_isParentTagPoint as isParentTagPoint 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isAttached -}
{#fun ogre_mvo_isAttached as isAttached 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function detachFromParent -}
{#fun ogre_mvo_detachFromParent as detachFromParent 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function isInScene -}
{#fun ogre_mvo_isInScene as isInScene 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function getBoundingRadius -}
{#fun ogre_mvo_getBoundingRadius as getBoundingRadius 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function setVisible -}
{#fun ogre_mvo_setVisible as setVisible 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function getVisible -}
{#fun ogre_mvo_getVisible as getVisible 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isVisible -}
{#fun ogre_mvo_isVisible as isVisible 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function setRenderingDistance -}
{#fun ogre_mvo_setRenderingDistance as setRenderingDistance 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' } -> `()'  #}

{- function getRenderingDistance -}
{#fun ogre_mvo_getRenderingDistance as getRenderingDistance 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function setRenderingMinPixelSize -}
{#fun ogre_mvo_setRenderingMinPixelSize as setRenderingMinPixelSize 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' } -> `()'  #}

{- function getRenderingMinPixelSize -}
{#fun ogre_mvo_getRenderingMinPixelSize as getRenderingMinPixelSize 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function setQueryFlags -}
{#fun ogre_mvo_setQueryFlags as setQueryFlags 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' } -> `()'  #}

{- function addQueryFlags -}
{#fun ogre_mvo_addQueryFlags as addQueryFlags 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' } -> `()'  #}

{- function removeQueryFlags -}
{#fun ogre_mvo_removeQueryFlags as removeQueryFlags 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' } -> `()'  #}

{- function getQueryFlags -}
{#fun ogre_mvo_getQueryFlags as getQueryFlags 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function setVisibilityFlags -}
{#fun ogre_mvo_setVisibilityFlags as setVisibilityFlags 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' } -> `()'  #}

{- function addVisibilityFlags -}
{#fun ogre_mvo_addVisibilityFlags as addVisibilityFlags 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' } -> `()'  #}

{- function removeVisibilityFlags -}
{#fun ogre_mvo_removeVisibilityFlags as removeVisibilityFlags 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' } -> `()'  #}

{- function getVisibilityFlags -}
{#fun ogre_mvo_getVisibilityFlags as getVisibilityFlags 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function getLightMask -}
{#fun ogre_mvo_getLightMask as getLightMask 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function setLightMask -}
{#fun ogre_mvo_setLightMask as setLightMask 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' } -> `()'  #}

{- function hasEdgeList -}
{#fun ogre_mvo_hasEdgeList as hasEdgeList 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function setCastShadows -}
{#fun ogre_mvo_setCastShadows as setCastShadows 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function getCastShadows -}
{#fun ogre_mvo_getCastShadows as getCastShadows 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function getReceivesShadows -}
{#fun ogre_mvo_getReceivesShadows as getReceivesShadows 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function getPointExtrusionDistance -}
{#fun ogre_mvo_getPointExtrusionDistance as getPointExtrusionDistance 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function getTypeFlags -}
{#fun ogre_mvo_getTypeFlags as getTypeFlags 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function setDebugDisplayEnabled -}
{#fun ogre_mvo_setDebugDisplayEnabled as setDebugDisplayEnabled 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function isDebugDisplayEnabled -}
{#fun ogre_mvo_isDebugDisplayEnabled as isDebugDisplayEnabled 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function setDefaultQueryFlags -}
{#fun ogre_mvo_setDefaultQueryFlags as setDefaultQueryFlags 
{ fromIntegral `Int' } -> `()'  #}

{- function getDefaultQueryFlags -}
{#fun ogre_mvo_getDefaultQueryFlags as getDefaultQueryFlags 
{ alloca- `Int' peekIntConv*} -> `()'  #}

{- function setDefaultVisibilityFlags -}
{#fun ogre_mvo_setDefaultVisibilityFlags as setDefaultVisibilityFlags 
{ fromIntegral `Int' } -> `()'  #}

{- function getDefaultVisibilityFlags -}
{#fun ogre_mvo_getDefaultVisibilityFlags as getDefaultVisibilityFlags 
{ alloca- `Int' peekIntConv*} -> `()'  #}

