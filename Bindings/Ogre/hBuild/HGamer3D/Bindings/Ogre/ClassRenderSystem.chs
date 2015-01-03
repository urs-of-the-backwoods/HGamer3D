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


-- ClassRenderSystem.chs

-- 

module HGamer3D.Bindings.Ogre.ClassRenderSystem where

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
{# import HGamer3D.Bindings.Ogre.EnumGpuProgramType #}

#include "ClassRenderSystem.h"
{- function ~RenderSystem -}
{#fun ogre_rds_destruct as delete 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function getName -}
{#fun ogre_rds_getName as getName 
{ withHG3DClass* `HG3DClass' ,
 alloc64k- `String' peekCString*} -> `()'  #}

{- function setConfigOption -}
{#fun ogre_rds_setConfigOption as setConfigOption 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 withCString* `String' } -> `()'  #}

{- function validateConfigOptions -}
{#fun ogre_rds_validateConfigOptions as validateConfigOptions 
{ withHG3DClass* `HG3DClass' ,
 alloc64k- `String' peekCString*} -> `()'  #}

{- function reinitialise -}
{#fun ogre_rds_reinitialise as reinitialise 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function shutdown -}
{#fun ogre_rds_shutdown as shutdown 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function setAmbientLight -}
{#fun ogre_rds_setAmbientLight as setAmbientLight 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' ,
 realToFrac `Float' ,
 realToFrac `Float' } -> `()'  #}

{- function setLightingEnabled -}
{#fun ogre_rds_setLightingEnabled as setLightingEnabled 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function setWBufferEnabled -}
{#fun ogre_rds_setWBufferEnabled as setWBufferEnabled 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function getWBufferEnabled -}
{#fun ogre_rds_getWBufferEnabled as getWBufferEnabled 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function createMultiRenderTarget -}
{#fun ogre_rds_createMultiRenderTarget as createMultiRenderTarget 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function destroyRenderWindow -}
{#fun ogre_rds_destroyRenderWindow as destroyRenderWindow 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' } -> `()'  #}

{- function destroyRenderTexture -}
{#fun ogre_rds_destroyRenderTexture as destroyRenderTexture 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' } -> `()'  #}

{- function destroyRenderTarget -}
{#fun ogre_rds_destroyRenderTarget as destroyRenderTarget 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' } -> `()'  #}

{- function attachRenderTarget -}
{#fun ogre_rds_attachRenderTarget as attachRenderTarget 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function getRenderTarget -}
{#fun ogre_rds_getRenderTarget as getRenderTarget 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function detachRenderTarget -}
{#fun ogre_rds_detachRenderTarget as detachRenderTarget 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getErrorDescription -}
{#fun ogre_rds_getErrorDescription as getErrorDescription 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 alloc64k- `String' peekCString*} -> `()'  #}

{- function setWaitForVerticalBlank -}
{#fun ogre_rds_setWaitForVerticalBlank as setWaitForVerticalBlank 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function getWaitForVerticalBlank -}
{#fun ogre_rds_getWaitForVerticalBlank as getWaitForVerticalBlank 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function getGlobalNumberOfInstances -}
{#fun ogre_rds_getGlobalNumberOfInstances as getGlobalNumberOfInstances 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function setGlobalNumberOfInstances -}
{#fun ogre_rds_setGlobalNumberOfInstances as setGlobalNumberOfInstances 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' } -> `()'  #}

{- function setFixedPipelineEnabled -}
{#fun ogre_rds_setFixedPipelineEnabled as setFixedPipelineEnabled 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function getFixedPipelineEnabled -}
{#fun ogre_rds_getFixedPipelineEnabled as getFixedPipelineEnabled 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function setDepthBufferFor -}
{#fun ogre_rds_setDepthBufferFor as setDepthBufferFor 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function areFixedFunctionLightsInViewSpace -}
{#fun ogre_rds_areFixedFunctionLightsInViewSpace as areFixedFunctionLightsInViewSpace 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function convertColourValue -}
{#fun ogre_rds_convertColourValue as convertColourValue 
{ withHG3DClass* `HG3DClass' ,
 withColour* `Colour' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function setStencilCheckEnabled -}
{#fun ogre_rds_setStencilCheckEnabled as setStencilCheckEnabled 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function setNormaliseNormals -}
{#fun ogre_rds_setNormaliseNormals as setNormaliseNormals 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function bindGpuProgramPassIterationParameters -}
{#fun ogre_rds_bindGpuProgramPassIterationParameters as bindGpuProgramPassIterationParameters 
{ withHG3DClass* `HG3DClass' ,
 cIntFromEnum `EnumGpuProgramType' } -> `()'  #}

{- function unbindGpuProgram -}
{#fun ogre_rds_unbindGpuProgram as unbindGpuProgram 
{ withHG3DClass* `HG3DClass' ,
 cIntFromEnum `EnumGpuProgramType' } -> `()'  #}

{- function isGpuProgramBound -}
{#fun ogre_rds_isGpuProgramBound as isGpuProgramBound 
{ withHG3DClass* `HG3DClass' ,
 cIntFromEnum `EnumGpuProgramType' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function addClipPlane2 -}
{#fun ogre_rds_addClipPlane2 as addClipPlane2 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' ,
 realToFrac `Float' ,
 realToFrac `Float' ,
 realToFrac `Float' } -> `()'  #}

{- function resetClipPlanes -}
{#fun ogre_rds_resetClipPlanes as resetClipPlanes 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function setInvertVertexWinding -}
{#fun ogre_rds_setInvertVertexWinding as setInvertVertexWinding 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function getInvertVertexWinding -}
{#fun ogre_rds_getInvertVertexWinding as getInvertVertexWinding 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function setScissorTest -}
{#fun ogre_rds_setScissorTest as setScissorTest 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' ,
 fromIntegral `Int' ,
 fromIntegral `Int' ,
 fromIntegral `Int' ,
 fromIntegral `Int' } -> `()'  #}

{- function clearFrameBuffer -}
{#fun ogre_rds_clearFrameBuffer as clearFrameBuffer 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 withColour* `Colour' ,
 realToFrac `Float' ,
 fromIntegral `Int' } -> `()'  #}

{- function getHorizontalTexelOffset -}
{#fun ogre_rds_getHorizontalTexelOffset as getHorizontalTexelOffset 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function getVerticalTexelOffset -}
{#fun ogre_rds_getVerticalTexelOffset as getVerticalTexelOffset 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function getMinimumDepthInputValue -}
{#fun ogre_rds_getMinimumDepthInputValue as getMinimumDepthInputValue 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function getMaximumDepthInputValue -}
{#fun ogre_rds_getMaximumDepthInputValue as getMaximumDepthInputValue 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function setCurrentPassIterationCount -}
{#fun ogre_rds_setCurrentPassIterationCount as setCurrentPassIterationCount 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' } -> `()'  #}

{- function setDeriveDepthBias -}
{#fun ogre_rds_setDeriveDepthBias as setDeriveDepthBias 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' ,
 realToFrac `Float' ,
 realToFrac `Float' ,
 realToFrac `Float' } -> `()'  #}

{- function preExtraThreadsStarted -}
{#fun ogre_rds_preExtraThreadsStarted as preExtraThreadsStarted 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function postExtraThreadsStarted -}
{#fun ogre_rds_postExtraThreadsStarted as postExtraThreadsStarted 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function registerThread -}
{#fun ogre_rds_registerThread as registerThread 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function unregisterThread -}
{#fun ogre_rds_unregisterThread as unregisterThread 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function getDisplayMonitorCount -}
{#fun ogre_rds_getDisplayMonitorCount as getDisplayMonitorCount 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function beginProfileEvent -}
{#fun ogre_rds_beginProfileEvent as beginProfileEvent 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' } -> `()'  #}

{- function endProfileEvent -}
{#fun ogre_rds_endProfileEvent as endProfileEvent 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function markProfileEvent -}
{#fun ogre_rds_markProfileEvent as markProfileEvent 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' } -> `()'  #}

