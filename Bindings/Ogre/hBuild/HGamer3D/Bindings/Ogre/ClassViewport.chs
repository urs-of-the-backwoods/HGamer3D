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


-- ClassViewport.chs

-- 

module HGamer3D.Bindings.Ogre.ClassViewport where

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
{# import HGamer3D.Bindings.Ogre.StructColour #}
{# import HGamer3D.Bindings.Ogre.EnumOrientationMode #}
{# import HGamer3D.Bindings.Ogre.StructVec2 #}

#include "ClassViewport.h"
{- function Viewport -}
{#fun ogre_vprt_construct as new 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' ,
 realToFrac `Float' ,
 realToFrac `Float' ,
 realToFrac `Float' ,
 realToFrac `Float' ,
 fromIntegral `Int' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function ~Viewport -}
{#fun ogre_vprt_destruct as delete 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function update -}
{#fun ogre_vprt_update as update 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function clear -}
{#fun ogre_vprt_clear as clear 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 withColour* `Colour' ,
 realToFrac `Float' ,
 fromIntegral `Int' } -> `()'  #}

{- function getTarget -}
{#fun ogre_vprt_getTarget as getTarget 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getCamera -}
{#fun ogre_vprt_getCamera as getCamera 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function setCamera -}
{#fun ogre_vprt_setCamera as setCamera 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function getZOrder -}
{#fun ogre_vprt_getZOrder as getZOrder 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function getLeft -}
{#fun ogre_vprt_getLeft as getLeft 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function getTop -}
{#fun ogre_vprt_getTop as getTop 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function getWidth -}
{#fun ogre_vprt_getWidth as getWidth 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function getHeight -}
{#fun ogre_vprt_getHeight as getHeight 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function getActualLeft -}
{#fun ogre_vprt_getActualLeft as getActualLeft 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function getActualTop -}
{#fun ogre_vprt_getActualTop as getActualTop 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function getActualWidth -}
{#fun ogre_vprt_getActualWidth as getActualWidth 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function getActualHeight -}
{#fun ogre_vprt_getActualHeight as getActualHeight 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function setDimensions -}
{#fun ogre_vprt_setDimensions as setDimensions 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' ,
 realToFrac `Float' ,
 realToFrac `Float' ,
 realToFrac `Float' } -> `()'  #}

{- function setOrientationMode -}
{#fun ogre_vprt_setOrientationMode as setOrientationMode 
{ withHG3DClass* `HG3DClass' ,
 cIntFromEnum `EnumOrientationMode' ,
 fromBool `Bool' } -> `()'  #}

{- function getOrientationMode -}
{#fun ogre_vprt_getOrientationMode as getOrientationMode 
{ withHG3DClass* `HG3DClass' ,
 alloca- `EnumOrientationMode' peekEnumUtil*} -> `()'  #}

{- function setBackgroundColour -}
{#fun ogre_vprt_setBackgroundColour as setBackgroundColour 
{ withHG3DClass* `HG3DClass' ,
 withColour* `Colour' } -> `()'  #}

{- function getBackgroundColour -}
{#fun ogre_vprt_getBackgroundColour as getBackgroundColour 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Colour' peekColour*} -> `()'  #}

{- function setDepthClear -}
{#fun ogre_vprt_setDepthClear as setDepthClear 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' } -> `()'  #}

{- function getDepthClear -}
{#fun ogre_vprt_getDepthClear as getDepthClear 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function setClearEveryFrame -}
{#fun ogre_vprt_setClearEveryFrame as setClearEveryFrame 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' ,
 fromIntegral `Int' } -> `()'  #}

{- function getClearEveryFrame -}
{#fun ogre_vprt_getClearEveryFrame as getClearEveryFrame 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function getClearBuffers -}
{#fun ogre_vprt_getClearBuffers as getClearBuffers 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function setAutoUpdated -}
{#fun ogre_vprt_setAutoUpdated as setAutoUpdated 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function isAutoUpdated -}
{#fun ogre_vprt_isAutoUpdated as isAutoUpdated 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function setMaterialScheme -}
{#fun ogre_vprt_setMaterialScheme as setMaterialScheme 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' } -> `()'  #}

{- function getMaterialScheme -}
{#fun ogre_vprt_getMaterialScheme as getMaterialScheme 
{ withHG3DClass* `HG3DClass' ,
 alloc64k- `String' peekCString*} -> `()'  #}

{- function getActualDimensions -}
{#fun ogre_vprt_getActualDimensions as getActualDimensions 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*,
 alloca- `Int' peekIntConv*,
 alloca- `Int' peekIntConv*,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function setOverlaysEnabled -}
{#fun ogre_vprt_setOverlaysEnabled as setOverlaysEnabled 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function getOverlaysEnabled -}
{#fun ogre_vprt_getOverlaysEnabled as getOverlaysEnabled 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function setSkiesEnabled -}
{#fun ogre_vprt_setSkiesEnabled as setSkiesEnabled 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function getSkiesEnabled -}
{#fun ogre_vprt_getSkiesEnabled as getSkiesEnabled 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function setShadowsEnabled -}
{#fun ogre_vprt_setShadowsEnabled as setShadowsEnabled 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function getShadowsEnabled -}
{#fun ogre_vprt_getShadowsEnabled as getShadowsEnabled 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function setVisibilityMask -}
{#fun ogre_vprt_setVisibilityMask as setVisibilityMask 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' } -> `()'  #}

{- function getVisibilityMask -}
{#fun ogre_vprt_getVisibilityMask as getVisibilityMask 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function setRenderQueueInvocationSequenceName -}
{#fun ogre_vprt_setRenderQueueInvocationSequenceName as setRenderQueueInvocationSequenceName 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' } -> `()'  #}

{- function getRenderQueueInvocationSequenceName -}
{#fun ogre_vprt_getRenderQueueInvocationSequenceName as getRenderQueueInvocationSequenceName 
{ withHG3DClass* `HG3DClass' ,
 alloc64k- `String' peekCString*} -> `()'  #}

{- function pointOrientedToScreen -}
{#fun ogre_vprt_pointOrientedToScreen as pointOrientedToScreen 
{ withHG3DClass* `HG3DClass' ,
 withVec2* `Vec2' ,
 fromIntegral `Int' ,
 alloca- `Vec2' peekVec2*} -> `()'  #}

{- function pointOrientedToScreen2 -}
{#fun ogre_vprt_pointOrientedToScreen2 as pointOrientedToScreen2 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' ,
 realToFrac `Float' ,
 fromIntegral `Int' ,
 alloca- `Float' peekFloatConv*,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function setDefaultOrientationMode -}
{#fun ogre_vprt_setDefaultOrientationMode as setDefaultOrientationMode 
{ cIntFromEnum `EnumOrientationMode' } -> `()'  #}

{- function getDefaultOrientationMode -}
{#fun ogre_vprt_getDefaultOrientationMode as getDefaultOrientationMode 
{ alloca- `EnumOrientationMode' peekEnumUtil*} -> `()'  #}

