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


-- ClassRoot.chs

-- 

module HGamer3D.Bindings.Ogre.ClassRoot where

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

#include "ClassRoot.h"
{- function Root -}
{#fun ogre_rt_construct as new 
{ withCString* `String' ,
 withCString* `String' ,
 withCString* `String' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function ~Root -}
{#fun ogre_rt_destruct as delete 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function saveConfig -}
{#fun ogre_rt_saveConfig as saveConfig 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function restoreConfig -}
{#fun ogre_rt_restoreConfig as restoreConfig 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function showConfigDialog -}
{#fun ogre_rt_showConfigDialog as showConfigDialog 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function addRenderSystem -}
{#fun ogre_rt_addRenderSystem as addRenderSystem 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function getRenderSystemByName -}
{#fun ogre_rt_getRenderSystemByName as getRenderSystemByName 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function setRenderSystem -}
{#fun ogre_rt_setRenderSystem as setRenderSystem 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function getRenderSystem -}
{#fun ogre_rt_getRenderSystem as getRenderSystem 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function initialise -}
{#fun ogre_rt_initialise as initialise 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' ,
 withCString* `String' ,
 withCString* `String' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function isInitialised -}
{#fun ogre_rt_isInitialised as isInitialised 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function getRemoveRenderQueueStructuresOnClear -}
{#fun ogre_rt_getRemoveRenderQueueStructuresOnClear as getRemoveRenderQueueStructuresOnClear 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function setRemoveRenderQueueStructuresOnClear -}
{#fun ogre_rt_setRemoveRenderQueueStructuresOnClear as setRemoveRenderQueueStructuresOnClear 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function addSceneManagerFactory -}
{#fun ogre_rt_addSceneManagerFactory as addSceneManagerFactory 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function removeSceneManagerFactory -}
{#fun ogre_rt_removeSceneManagerFactory as removeSceneManagerFactory 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function createSceneManager -}
{#fun ogre_rt_createSceneManager as createSceneManager 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 withCString* `String' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function destroySceneManager -}
{#fun ogre_rt_destroySceneManager as destroySceneManager 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function getSceneManager -}
{#fun ogre_rt_getSceneManager as getSceneManager 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function hasSceneManager -}
{#fun ogre_rt_hasSceneManager as hasSceneManager 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function getTextureManager -}
{#fun ogre_rt_getTextureManager as getTextureManager 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getMeshManager -}
{#fun ogre_rt_getMeshManager as getMeshManager 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getErrorDescription -}
{#fun ogre_rt_getErrorDescription as getErrorDescription 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 alloc64k- `String' peekCString*} -> `()'  #}

{- function queueEndRendering -}
{#fun ogre_rt_queueEndRendering as queueEndRendering 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function startRendering -}
{#fun ogre_rt_startRendering as startRendering 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function renderOneFrame -}
{#fun ogre_rt_renderOneFrame as renderOneFrame 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function renderOneFrame2 -}
{#fun ogre_rt_renderOneFrame2 as renderOneFrame2 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function shutdown -}
{#fun ogre_rt_shutdown as shutdown 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function addResourceLocation -}
{#fun ogre_rt_addResourceLocation as addResourceLocation 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 withCString* `String' ,
 withCString* `String' ,
 fromBool `Bool' } -> `()'  #}

{- function removeResourceLocation -}
{#fun ogre_rt_removeResourceLocation as removeResourceLocation 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 withCString* `String' } -> `()'  #}

{- function convertColourValue -}
{#fun ogre_rt_convertColourValue as convertColourValue 
{ withHG3DClass* `HG3DClass' ,
 withColour* `Colour' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function getAutoCreatedWindow -}
{#fun ogre_rt_getAutoCreatedWindow as getAutoCreatedWindow 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function detachRenderTarget -}
{#fun ogre_rt_detachRenderTarget as detachRenderTarget 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function detachRenderTarget2 -}
{#fun ogre_rt_detachRenderTarget2 as detachRenderTarget2 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function destroyRenderTarget -}
{#fun ogre_rt_destroyRenderTarget as destroyRenderTarget 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function destroyRenderTarget2 -}
{#fun ogre_rt_destroyRenderTarget2 as destroyRenderTarget2 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' } -> `()'  #}

{- function getRenderTarget -}
{#fun ogre_rt_getRenderTarget as getRenderTarget 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function loadPlugin -}
{#fun ogre_rt_loadPlugin as loadPlugin 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' } -> `()'  #}

{- function unloadPlugin -}
{#fun ogre_rt_unloadPlugin as unloadPlugin 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' } -> `()'  #}

{- function destroyRenderQueueInvocationSequence -}
{#fun ogre_rt_destroyRenderQueueInvocationSequence as destroyRenderQueueInvocationSequence 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' } -> `()'  #}

{- function destroyAllRenderQueueInvocationSequences -}
{#fun ogre_rt_destroyAllRenderQueueInvocationSequences as destroyAllRenderQueueInvocationSequences 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function clearEventTimes -}
{#fun ogre_rt_clearEventTimes as clearEventTimes 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function setFrameSmoothingPeriod -}
{#fun ogre_rt_setFrameSmoothingPeriod as setFrameSmoothingPeriod 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' } -> `()'  #}

{- function getFrameSmoothingPeriod -}
{#fun ogre_rt_getFrameSmoothingPeriod as getFrameSmoothingPeriod 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function addMovableObjectFactory -}
{#fun ogre_rt_addMovableObjectFactory as addMovableObjectFactory 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function removeMovableObjectFactory -}
{#fun ogre_rt_removeMovableObjectFactory as removeMovableObjectFactory 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function hasMovableObjectFactory -}
{#fun ogre_rt_hasMovableObjectFactory as hasMovableObjectFactory 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function getMovableObjectFactory -}
{#fun ogre_rt_getMovableObjectFactory as getMovableObjectFactory 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getDisplayMonitorCount -}
{#fun ogre_rt_getDisplayMonitorCount as getDisplayMonitorCount 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function setBlendIndicesGpuRedundant -}
{#fun ogre_rt_setBlendIndicesGpuRedundant as setBlendIndicesGpuRedundant 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function isBlendIndicesGpuRedundant -}
{#fun ogre_rt_isBlendIndicesGpuRedundant as isBlendIndicesGpuRedundant 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function setBlendWeightsGpuRedundant -}
{#fun ogre_rt_setBlendWeightsGpuRedundant as setBlendWeightsGpuRedundant 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function isBlendWeightsGpuRedundant -}
{#fun ogre_rt_isBlendWeightsGpuRedundant as isBlendWeightsGpuRedundant 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function setDefaultMinPixelSize -}
{#fun ogre_rt_setDefaultMinPixelSize as setDefaultMinPixelSize 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' } -> `()'  #}

{- function getDefaultMinPixelSize -}
{#fun ogre_rt_getDefaultMinPixelSize as getDefaultMinPixelSize 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function getSingleton -}
{#fun ogre_rt_getSingleton as getSingleton 
{ alloca- `HG3DClass' peek*} -> `()'  #}

{- function getSingletonPtr -}
{#fun ogre_rt_getSingletonPtr as getSingletonPtr 
{ alloca- `HG3DClass' peek*} -> `()'  #}

