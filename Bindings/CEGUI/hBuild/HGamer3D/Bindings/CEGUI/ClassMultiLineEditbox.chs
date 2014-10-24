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


-- ClassMultiLineEditbox.chs

-- 

module HGamer3D.Bindings.CEGUI.ClassMultiLineEditbox where

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

#include "ClassMultiLineEditbox.h"
{- function hasInputFocus -}
{#fun cegui_mltlnedtbx_hasInputFocus as hasInputFocus 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isReadOnly -}
{#fun cegui_mltlnedtbx_isReadOnly as isReadOnly 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function getCaratIndex -}
{#fun cegui_mltlnedtbx_getCaratIndex as getCaratIndex 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function getSelectionStartIndex -}
{#fun cegui_mltlnedtbx_getSelectionStartIndex as getSelectionStartIndex 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function getSelectionEndIndex -}
{#fun cegui_mltlnedtbx_getSelectionEndIndex as getSelectionEndIndex 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function getSelectionLength -}
{#fun cegui_mltlnedtbx_getSelectionLength as getSelectionLength 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function getMaxTextLength -}
{#fun cegui_mltlnedtbx_getMaxTextLength as getMaxTextLength 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function isWordWrapped -}
{#fun cegui_mltlnedtbx_isWordWrapped as isWordWrapped 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function getVertScrollbar -}
{#fun cegui_mltlnedtbx_getVertScrollbar as getVertScrollbar 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function isVertScrollbarAlwaysShown -}
{#fun cegui_mltlnedtbx_isVertScrollbarAlwaysShown as isVertScrollbarAlwaysShown 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function getHorzScrollbar -}
{#fun cegui_mltlnedtbx_getHorzScrollbar as getHorzScrollbar 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getLineNumberFromIndex -}
{#fun cegui_mltlnedtbx_getLineNumberFromIndex as getLineNumberFromIndex 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function initialiseComponents -}
{#fun cegui_mltlnedtbx_initialiseComponents as initialiseComponents 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function setReadOnly -}
{#fun cegui_mltlnedtbx_setReadOnly as setReadOnly 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function setCaratIndex -}
{#fun cegui_mltlnedtbx_setCaratIndex as setCaratIndex 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' } -> `()'  #}

{- function setSelection -}
{#fun cegui_mltlnedtbx_setSelection as setSelection 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 fromIntegral `Int' } -> `()'  #}

{- function setMaxTextLength -}
{#fun cegui_mltlnedtbx_setMaxTextLength as setMaxTextLength 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' } -> `()'  #}

{- function ensureCaratIsVisible -}
{#fun cegui_mltlnedtbx_ensureCaratIsVisible as ensureCaratIsVisible 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function setWordWrapping -}
{#fun cegui_mltlnedtbx_setWordWrapping as setWordWrapping 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function setShowVertScrollbar -}
{#fun cegui_mltlnedtbx_setShowVertScrollbar as setShowVertScrollbar 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function MultiLineEditbox -}
{#fun cegui_mltlnedtbx_construct as new 
{ withCString* `String' ,
 withCString* `String' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function ~MultiLineEditbox -}
{#fun cegui_mltlnedtbx_destruct as delete 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

