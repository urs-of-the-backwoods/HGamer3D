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


-- ClassOgreRenderer.chs

-- 

module HGamer3D.Bindings.CEGUI.ClassOgreRenderer where

import Foreign
import Foreign.Ptr
import Foreign.C

import HGamer3D.Data.HG3DClass
import HGamer3D.Data.Vector
import HGamer3D.Data.Colour
import HGamer3D.Data.Angle

{# import HGamer3D.Bindings.CEGUI.Utils #}
{# import HGamer3D.Bindings.CEGUI.ClassPtr #}
{# import HGamer3D.Bindings.CEGUI.StructHG3DClass #}
{# import HGamer3D.Bindings.CEGUI.EnumBlendMode #}

#include "ClassOgreRenderer.h"
{- function bootstrapSystem -}
{#fun cegui_ogrrndr_bootstrapSystem as bootstrapSystem 
{ alloca- `HG3DClass' peek*} -> `()'  #}

{- function destroySystem -}
{#fun cegui_ogrrndr_destroySystem as destroySystem 
{ } -> `()'  #}

{- function create -}
{#fun cegui_ogrrndr_create as create 
{ alloca- `HG3DClass' peek*} -> `()'  #}

{- function destroy -}
{#fun cegui_ogrrndr_destroy as destroy 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function createOgreResourceProvider -}
{#fun cegui_ogrrndr_createOgreResourceProvider as createOgreResourceProvider 
{ alloca- `HG3DClass' peek*} -> `()'  #}

{- function destroyOgreResourceProvider -}
{#fun cegui_ogrrndr_destroyOgreResourceProvider as destroyOgreResourceProvider 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function setRenderingEnabled -}
{#fun cegui_ogrrndr_setRenderingEnabled as setRenderingEnabled 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function isRenderingEnabled -}
{#fun cegui_ogrrndr_isRenderingEnabled as isRenderingEnabled 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function setupRenderingBlendMode -}
{#fun cegui_ogrrndr_setupRenderingBlendMode as setupRenderingBlendMode 
{ withHG3DClass* `HG3DClass' ,
 cIntFromEnum `EnumBlendMode' ,
 fromBool `Bool' } -> `()'  #}

{- function setFrameControlExecutionEnabled -}
{#fun cegui_ogrrndr_setFrameControlExecutionEnabled as setFrameControlExecutionEnabled 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function isFrameControlExecutionEnabled -}
{#fun cegui_ogrrndr_isFrameControlExecutionEnabled as isFrameControlExecutionEnabled 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function initialiseRenderStateSettings -}
{#fun cegui_ogrrndr_initialiseRenderStateSettings as initialiseRenderStateSettings 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function destroyAllGeometryBuffers -}
{#fun cegui_ogrrndr_destroyAllGeometryBuffers as destroyAllGeometryBuffers 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function destroyAllTextureTargets -}
{#fun cegui_ogrrndr_destroyAllTextureTargets as destroyAllTextureTargets 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function destroyAllTextures -}
{#fun cegui_ogrrndr_destroyAllTextures as destroyAllTextures 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function beginRendering -}
{#fun cegui_ogrrndr_beginRendering as beginRendering 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function endRendering -}
{#fun cegui_ogrrndr_endRendering as endRendering 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function getMaxTextureSize -}
{#fun cegui_ogrrndr_getMaxTextureSize as getMaxTextureSize 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function getIdentifierString -}
{#fun cegui_ogrrndr_getIdentifierString as getIdentifierString 
{ withHG3DClass* `HG3DClass' ,
 alloc64k- `String' peekCString*} -> `()'  #}

