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


-- ClassMaterialManager.chs

-- 

module HGamer3D.Bindings.Ogre.ClassMaterialManager where

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
{# import HGamer3D.Bindings.Ogre.StructSharedPtr #}

#include "ClassMaterialManager.h"
{- function MaterialManager -}
{#fun ogre_mtrlmgr_construct as new 
{ alloca- `HG3DClass' peek*} -> `()'  #}

{- function ~MaterialManager -}
{#fun ogre_mtrlmgr_destruct as delete 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function initialise -}
{#fun ogre_mtrlmgr_initialise as initialise 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function setDefaultAnisotropy -}
{#fun ogre_mtrlmgr_setDefaultAnisotropy as setDefaultAnisotropy 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' } -> `()'  #}

{- function getDefaultAnisotropy -}
{#fun ogre_mtrlmgr_getDefaultAnisotropy as getDefaultAnisotropy 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function getDefaultSettings -}
{#fun ogre_mtrlmgr_getDefaultSettings as getDefaultSettings 
{ withHG3DClass* `HG3DClass' ,
 alloca- `SharedPtr' peekSharedPtr*} -> `()'  #}

{- function getActiveScheme -}
{#fun ogre_mtrlmgr_getActiveScheme as getActiveScheme 
{ withHG3DClass* `HG3DClass' ,
 alloc64k- `String' peekCString*} -> `()'  #}

{- function setActiveScheme -}
{#fun ogre_mtrlmgr_setActiveScheme as setActiveScheme 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' } -> `()'  #}

{- function getSingleton -}
{#fun ogre_mtrlmgr_getSingleton as getSingleton 
{ alloca- `HG3DClass' peek*} -> `()'  #}

{- function getSingletonPtr -}
{#fun ogre_mtrlmgr_getSingletonPtr as getSingletonPtr 
{ alloca- `HG3DClass' peek*} -> `()'  #}

