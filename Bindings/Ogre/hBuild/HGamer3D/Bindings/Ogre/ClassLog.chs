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


-- ClassLog.chs

-- 

module HGamer3D.Bindings.Ogre.ClassLog where

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

#include "ClassLog.h"
{- function ~Log -}
{#fun ogre_lg_destruct as delete 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function getName -}
{#fun ogre_lg_getName as getName 
{ withHG3DClass* `HG3DClass' ,
 alloc64k- `String' peekCString*} -> `()'  #}

{- function isDebugOutputEnabled -}
{#fun ogre_lg_isDebugOutputEnabled as isDebugOutputEnabled 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isFileOutputSuppressed -}
{#fun ogre_lg_isFileOutputSuppressed as isFileOutputSuppressed 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isTimeStampEnabled -}
{#fun ogre_lg_isTimeStampEnabled as isTimeStampEnabled 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function logMessage -}
{#fun ogre_lg_logMessage as logMessage 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 cIntFromEnum `EnumLogMessageLevel' ,
 fromBool `Bool' } -> `()'  #}

{- function setDebugOutputEnabled -}
{#fun ogre_lg_setDebugOutputEnabled as setDebugOutputEnabled 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function setLogDetail -}
{#fun ogre_lg_setLogDetail as setLogDetail 
{ withHG3DClass* `HG3DClass' ,
 cIntFromEnum `EnumLoggingLevel' } -> `()'  #}

{- function setTimeStampEnabled -}
{#fun ogre_lg_setTimeStampEnabled as setTimeStampEnabled 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function getLogDetail -}
{#fun ogre_lg_getLogDetail as getLogDetail 
{ withHG3DClass* `HG3DClass' ,
 alloca- `EnumLoggingLevel' peekEnumUtil*} -> `()'  #}

