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


-- ClassSystemHG3D.chs

-- 

module HGamer3D.Bindings.CEGUI.ClassSystemHG3D where

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

#include "ClassSystemHG3D.h"
{- function createNoLogger -}
{#fun cegui_sstm_hg3d_createNoLogger as createNoLogger 
{ alloca- `HG3DClass' peek*} -> `()'  #}

{- function getDefaultResourceProvider -}
{#fun cegui_sstm_hg3d_getDefaultResourceProvider as getDefaultResourceProvider 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getSchemeManagerSingleton -}
{#fun cegui_sstm_hg3d_getSchemeManagerSingleton as getSchemeManagerSingleton 
{ alloca- `HG3DClass' peek*} -> `()'  #}

{- function schemeManagerCreate -}
{#fun cegui_sstm_hg3d_schemeManagerCreate as schemeManagerCreate 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' } -> `()'  #}

{- function fontManagerCreate -}
{#fun cegui_sstm_hg3d_fontManagerCreate as fontManagerCreate 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' } -> `()'  #}

{- function getFontManagerSingleton -}
{#fun cegui_sstm_hg3d_getFontManagerSingleton as getFontManagerSingleton 
{ alloca- `HG3DClass' peek*} -> `()'  #}

{- function getLoggerSingleton -}
{#fun cegui_sstm_hg3d_getLoggerSingleton as getLoggerSingleton 
{ alloca- `HG3DClass' peek*} -> `()'  #}

