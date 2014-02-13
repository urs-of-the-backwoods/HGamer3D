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


-- ClassRenderTarget.chs

-- 

module HGamer3D.Bindings.Ogre.ClassRenderTarget where

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

#include "ClassRenderTarget.h"
{- function ~RenderTarget -}
{#fun ogre_rtgt_destruct as delete 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function getName -}
{#fun ogre_rtgt_getName as getName 
{ withHG3DClass* `HG3DClass' ,
 alloc64k- `String' peekCString*} -> `()'  #}

{- function getMetrics -}
{#fun ogre_rtgt_getMetrics as getMetrics 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*,
 alloca- `Int' peekIntConv*,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function getWidth -}
{#fun ogre_rtgt_getWidth as getWidth 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function getHeight -}
{#fun ogre_rtgt_getHeight as getHeight 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function getColourDepth -}
{#fun ogre_rtgt_getColourDepth as getColourDepth 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function setDepthBufferPool -}
{#fun ogre_rtgt_setDepthBufferPool as setDepthBufferPool 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' } -> `()'  #}

{- function getDepthBufferPool -}
{#fun ogre_rtgt_getDepthBufferPool as getDepthBufferPool 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function detachDepthBuffer -}
{#fun ogre_rtgt_detachDepthBuffer as detachDepthBuffer 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function update -}
{#fun ogre_rtgt_update as update 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function swapBuffers -}
{#fun ogre_rtgt_swapBuffers as swapBuffers 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function addViewport -}
{#fun ogre_rtgt_addViewport as addViewport 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 realToFrac `Float' ,
 realToFrac `Float' ,
 realToFrac `Float' ,
 realToFrac `Float' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getNumViewports -}
{#fun ogre_rtgt_getNumViewports as getNumViewports 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function getViewport -}
{#fun ogre_rtgt_getViewport as getViewport 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getViewportByZOrder -}
{#fun ogre_rtgt_getViewportByZOrder as getViewportByZOrder 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function hasViewportWithZOrder -}
{#fun ogre_rtgt_hasViewportWithZOrder as hasViewportWithZOrder 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function removeViewport -}
{#fun ogre_rtgt_removeViewport as removeViewport 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' } -> `()'  #}

{- function removeAllViewports -}
{#fun ogre_rtgt_removeAllViewports as removeAllViewports 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function getStatistics -}
{#fun ogre_rtgt_getStatistics as getStatistics 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*,
 alloca- `Float' peekFloatConv*,
 alloca- `Float' peekFloatConv*,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function getLastFPS -}
{#fun ogre_rtgt_getLastFPS as getLastFPS 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function getAverageFPS -}
{#fun ogre_rtgt_getAverageFPS as getAverageFPS 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function getBestFPS -}
{#fun ogre_rtgt_getBestFPS as getBestFPS 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function getWorstFPS -}
{#fun ogre_rtgt_getWorstFPS as getWorstFPS 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function getBestFrameTime -}
{#fun ogre_rtgt_getBestFrameTime as getBestFrameTime 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function getWorstFrameTime -}
{#fun ogre_rtgt_getWorstFrameTime as getWorstFrameTime 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function resetStatistics -}
{#fun ogre_rtgt_resetStatistics as resetStatistics 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function removeAllListeners -}
{#fun ogre_rtgt_removeAllListeners as removeAllListeners 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function isActive -}
{#fun ogre_rtgt_isActive as isActive 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function setActive -}
{#fun ogre_rtgt_setActive as setActive 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function setAutoUpdated -}
{#fun ogre_rtgt_setAutoUpdated as setAutoUpdated 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function isAutoUpdated -}
{#fun ogre_rtgt_isAutoUpdated as isAutoUpdated 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function writeContentsToFile -}
{#fun ogre_rtgt_writeContentsToFile as writeContentsToFile 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' } -> `()'  #}

{- function writeContentsToTimestampedFile -}
{#fun ogre_rtgt_writeContentsToTimestampedFile as writeContentsToTimestampedFile 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 withCString* `String' ,
 alloc64k- `String' peekCString*} -> `()'  #}

{- function requiresTextureFlipping -}
{#fun ogre_rtgt_requiresTextureFlipping as requiresTextureFlipping 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function getTriangleCount -}
{#fun ogre_rtgt_getTriangleCount as getTriangleCount 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function getBatchCount -}
{#fun ogre_rtgt_getBatchCount as getBatchCount 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function isPrimary -}
{#fun ogre_rtgt_isPrimary as isPrimary 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isHardwareGammaEnabled -}
{#fun ogre_rtgt_isHardwareGammaEnabled as isHardwareGammaEnabled 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function getFSAA -}
{#fun ogre_rtgt_getFSAA as getFSAA 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function getFSAAHint -}
{#fun ogre_rtgt_getFSAAHint as getFSAAHint 
{ withHG3DClass* `HG3DClass' ,
 alloc64k- `String' peekCString*} -> `()'  #}

