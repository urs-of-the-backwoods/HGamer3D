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


-- ClassLogManager.chs

-- 

module HGamer3D.Bindings.Ogre.ClassLogManager where

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
{# import HGamer3D.Bindings.Ogre.EnumLogMessageLevel #}
{# import HGamer3D.Bindings.Ogre.EnumLoggingLevel #}

#include "ClassLogManager.h"
{- function ~LogManager -}
{#fun ogre_lmgr_destruct as delete 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function createLog -}
{#fun ogre_lmgr_createLog as createLog 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 fromBool `Bool' ,
 fromBool `Bool' ,
 fromBool `Bool' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getLog -}
{#fun ogre_lmgr_getLog as getLog 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getDefaultLog -}
{#fun ogre_lmgr_getDefaultLog as getDefaultLog 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function destroyLog -}
{#fun ogre_lmgr_destroyLog as destroyLog 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' } -> `()'  #}

{- function destroyLog2 -}
{#fun ogre_lmgr_destroyLog2 as destroyLog2 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function setDefaultLog -}
{#fun ogre_lmgr_setDefaultLog as setDefaultLog 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function logMessage -}
{#fun ogre_lmgr_logMessage as logMessage 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 cIntFromEnum `EnumLogMessageLevel' ,
 fromBool `Bool' } -> `()'  #}

{- function logMessage2 -}
{#fun ogre_lmgr_logMessage2 as logMessage2 
{ withHG3DClass* `HG3DClass' ,
 cIntFromEnum `EnumLogMessageLevel' ,
 withCString* `String' ,
 fromBool `Bool' } -> `()'  #}

{- function setLogDetail -}
{#fun ogre_lmgr_setLogDetail as setLogDetail 
{ withHG3DClass* `HG3DClass' ,
 cIntFromEnum `EnumLoggingLevel' } -> `()'  #}

{- function getSingleton -}
{#fun ogre_lmgr_getSingleton as getSingleton 
{ alloca- `HG3DClass' peek*} -> `()'  #}

{- function getSingletonPtr -}
{#fun ogre_lmgr_getSingletonPtr as getSingletonPtr 
{ alloca- `HG3DClass' peek*} -> `()'  #}

