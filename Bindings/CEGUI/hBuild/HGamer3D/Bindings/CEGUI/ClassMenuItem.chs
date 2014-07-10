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


-- ClassMenuItem.chs

-- 

module HGamer3D.Bindings.CEGUI.ClassMenuItem where

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

#include "ClassMenuItem.h"
{- function isHovering -}
{#fun cegui_mnitm_isHovering as isHovering 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isPushed -}
{#fun cegui_mnitm_isPushed as isPushed 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isOpened -}
{#fun cegui_mnitm_isOpened as isOpened 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isPopupClosing -}
{#fun cegui_mnitm_isPopupClosing as isPopupClosing 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function hasAutoPopup -}
{#fun cegui_mnitm_hasAutoPopup as hasAutoPopup 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function getAutoPopupTimeout -}
{#fun cegui_mnitm_getAutoPopupTimeout as getAutoPopupTimeout 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function setAutoPopupTimeout -}
{#fun cegui_mnitm_setAutoPopupTimeout as setAutoPopupTimeout 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' } -> `()'  #}

{- function getPopupOffset -}
{#fun cegui_mnitm_getPopupOffset as getPopupOffset 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function setPopupOffset -}
{#fun cegui_mnitm_setPopupOffset as setPopupOffset 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function openPopupMenu -}
{#fun cegui_mnitm_openPopupMenu as openPopupMenu 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function closePopupMenu -}
{#fun cegui_mnitm_closePopupMenu as closePopupMenu 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function togglePopupMenu -}
{#fun cegui_mnitm_togglePopupMenu as togglePopupMenu 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function startPopupClosing -}
{#fun cegui_mnitm_startPopupClosing as startPopupClosing 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function startPopupOpening -}
{#fun cegui_mnitm_startPopupOpening as startPopupOpening 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function MenuItem -}
{#fun cegui_mnitm_construct as new 
{ withCString* `String' ,
 withCString* `String' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function ~MenuItem -}
{#fun cegui_mnitm_destruct as delete 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

