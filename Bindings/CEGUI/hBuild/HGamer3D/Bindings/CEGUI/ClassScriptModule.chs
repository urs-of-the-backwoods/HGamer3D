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


-- ClassScriptModule.chs

-- 

module HGamer3D.Bindings.CEGUI.ClassScriptModule where

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

#include "ClassScriptModule.h"
{- function ~ScriptModule -}
{#fun cegui_scrmd_destruct as delete 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function executeScriptFile -}
{#fun cegui_scrmd_executeScriptFile as executeScriptFile 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 withCString* `String' } -> `()'  #}

{- function executeScriptGlobal -}
{#fun cegui_scrmd_executeScriptGlobal as executeScriptGlobal 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function executeScriptedEventHandler -}
{#fun cegui_scrmd_executeScriptedEventHandler as executeScriptedEventHandler 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function executeString -}
{#fun cegui_scrmd_executeString as executeString 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' } -> `()'  #}

{- function createBindings -}
{#fun cegui_scrmd_createBindings as createBindings 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function destroyBindings -}
{#fun cegui_scrmd_destroyBindings as destroyBindings 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function getIdentifierString -}
{#fun cegui_scrmd_getIdentifierString as getIdentifierString 
{ withHG3DClass* `HG3DClass' ,
 alloc64k- `String' peekCString*} -> `()'  #}

{- function setDefaultResourceGroup -}
{#fun cegui_scrmd_setDefaultResourceGroup as setDefaultResourceGroup 
{ withCString* `String' } -> `()'  #}

{- function getDefaultResourceGroup -}
{#fun cegui_scrmd_getDefaultResourceGroup as getDefaultResourceGroup 
{ alloc64k- `String' peekCString*} -> `()'  #}

