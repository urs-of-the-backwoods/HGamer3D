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


-- ClassWindowManager.chs

-- 

module HGamer3D.Bindings.CEGUI.ClassWindowManager where

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

#include "ClassWindowManager.h"
{- function WindowManager -}
{#fun cegui_wmgr_construct as new 
{ alloca- `HG3DClass' peek*} -> `()'  #}

{- function ~WindowManager -}
{#fun cegui_wmgr_destruct as delete 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function createWindow -}
{#fun cegui_wmgr_createWindow as createWindow 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 withCString* `String' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function destroyWindow -}
{#fun cegui_wmgr_destroyWindow as destroyWindow 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function destroyWindow2 -}
{#fun cegui_wmgr_destroyWindow2 as destroyWindow2 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' } -> `()'  #}

{- function getWindow -}
{#fun cegui_wmgr_getWindow as getWindow 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function isWindowPresent -}
{#fun cegui_wmgr_isWindowPresent as isWindowPresent 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function destroyAllWindows -}
{#fun cegui_wmgr_destroyAllWindows as destroyAllWindows 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function isDeadPoolEmpty -}
{#fun cegui_wmgr_isDeadPoolEmpty as isDeadPoolEmpty 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function cleanDeadPool -}
{#fun cegui_wmgr_cleanDeadPool as cleanDeadPool 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function saveWindowLayout -}
{#fun cegui_wmgr_saveWindowLayout as saveWindowLayout 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 withCString* `String' ,
 fromBool `Bool' } -> `()'  #}

{- function saveWindowLayout2 -}
{#fun cegui_wmgr_saveWindowLayout2 as saveWindowLayout2 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 fromBool `Bool' } -> `()'  #}

{- function renameWindow -}
{#fun cegui_wmgr_renameWindow as renameWindow 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 withCString* `String' } -> `()'  #}

{- function renameWindow2 -}
{#fun cegui_wmgr_renameWindow2 as renameWindow2 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' ,
 withCString* `String' } -> `()'  #}

{- function lock -}
{#fun cegui_wmgr_lock as lock 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function unlock -}
{#fun cegui_wmgr_unlock as unlock 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function isLocked -}
{#fun cegui_wmgr_isLocked as isLocked 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function getDefaultResourceGroup -}
{#fun cegui_wmgr_getDefaultResourceGroup as getDefaultResourceGroup 
{ alloc64k- `String' peekCString*} -> `()'  #}

{- function setDefaultResourceGroup -}
{#fun cegui_wmgr_setDefaultResourceGroup as setDefaultResourceGroup 
{ withCString* `String' } -> `()'  #}

