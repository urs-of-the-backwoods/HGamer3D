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


-- ClassConfigFile.chs

-- 

module HGamer3D.Bindings.Ogre.ClassConfigFile where

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

#include "ClassConfigFile.h"
{- function ConfigFile -}
{#fun ogre_cf_construct as new 
{ alloca- `HG3DClass' peek*} -> `()'  #}

{- function ~ConfigFile -}
{#fun ogre_cf_destruct as delete 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function load -}
{#fun ogre_cf_load as load 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 withCString* `String' ,
 fromBool `Bool' } -> `()'  #}

{- function load2 -}
{#fun ogre_cf_load2 as load2 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 withCString* `String' ,
 withCString* `String' ,
 fromBool `Bool' } -> `()'  #}

{- function loadDirect -}
{#fun ogre_cf_loadDirect as loadDirect 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 withCString* `String' ,
 fromBool `Bool' } -> `()'  #}

{- function loadFromResourceSystem -}
{#fun ogre_cf_loadFromResourceSystem as loadFromResourceSystem 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 withCString* `String' ,
 withCString* `String' ,
 fromBool `Bool' } -> `()'  #}

{- function getSetting -}
{#fun ogre_cf_getSetting as getSetting 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 withCString* `String' ,
 withCString* `String' ,
 alloc64k- `String' peekCString*} -> `()'  #}

{- function clear -}
{#fun ogre_cf_clear as clear 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

