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


-- ClassWidgetLookManager.chs

-- 

module HGamer3D.Bindings.CEGUI.ClassWidgetLookManager where

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

#include "ClassWidgetLookManager.h"
{- function WidgetLookManager -}
{#fun cegui_wdgtlmgr_construct as new 
{ alloca- `HG3DClass' peek*} -> `()'  #}

{- function ~WidgetLookManager -}
{#fun cegui_wdgtlmgr_destruct as delete 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function parseLookNFeelSpecification -}
{#fun cegui_wdgtlmgr_parseLookNFeelSpecification as parseLookNFeelSpecification 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 withCString* `String' } -> `()'  #}

{- function isWidgetLookAvailable -}
{#fun cegui_wdgtlmgr_isWidgetLookAvailable as isWidgetLookAvailable 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function eraseWidgetLook -}
{#fun cegui_wdgtlmgr_eraseWidgetLook as eraseWidgetLook 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' } -> `()'  #}

{- function getSingleton -}
{#fun cegui_wdgtlmgr_getSingleton as getSingleton 
{ alloca- `HG3DClass' peek*} -> `()'  #}

{- function getSingletonPtr -}
{#fun cegui_wdgtlmgr_getSingletonPtr as getSingletonPtr 
{ alloca- `HG3DClass' peek*} -> `()'  #}

{- function getDefaultResourceGroup -}
{#fun cegui_wdgtlmgr_getDefaultResourceGroup as getDefaultResourceGroup 
{ alloc64k- `String' peekCString*} -> `()'  #}

{- function setDefaultResourceGroup -}
{#fun cegui_wdgtlmgr_setDefaultResourceGroup as setDefaultResourceGroup 
{ withCString* `String' } -> `()'  #}

