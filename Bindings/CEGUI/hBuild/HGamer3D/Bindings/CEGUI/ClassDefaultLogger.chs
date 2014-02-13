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


-- ClassDefaultLogger.chs

-- 

module HGamer3D.Bindings.CEGUI.ClassDefaultLogger where

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
{# import HGamer3D.Bindings.CEGUI.EnumLoggingLevel #}

#include "ClassDefaultLogger.h"
{- function DefaultLogger -}
{#fun cegui_dfltlgr_construct as new 
{ alloca- `HG3DClass' peek*} -> `()'  #}

{- function ~DefaultLogger -}
{#fun cegui_dfltlgr_destruct as delete 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function logEvent -}
{#fun cegui_dfltlgr_logEvent as logEvent 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 cIntFromEnum `EnumLoggingLevel' } -> `()'  #}

{- function setLogFilename -}
{#fun cegui_dfltlgr_setLogFilename as setLogFilename 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 fromBool `Bool' } -> `()'  #}

