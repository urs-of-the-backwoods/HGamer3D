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


-- ClassEditbox.chs

-- 

module HGamer3D.Bindings.CEGUI.ClassEditbox where

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

#include "ClassEditbox.h"
{- function hasInputFocus -}
{#fun cegui_edtbx_hasInputFocus as hasInputFocus 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isReadOnly -}
{#fun cegui_edtbx_isReadOnly as isReadOnly 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isTextMasked -}
{#fun cegui_edtbx_isTextMasked as isTextMasked 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isTextValid -}
{#fun cegui_edtbx_isTextValid as isTextValid 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function getValidationString -}
{#fun cegui_edtbx_getValidationString as getValidationString 
{ withHG3DClass* `HG3DClass' ,
 alloc64k- `String' peekCString*} -> `()'  #}

{- function getCaratIndex -}
{#fun cegui_edtbx_getCaratIndex as getCaratIndex 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function getSelectionStartIndex -}
{#fun cegui_edtbx_getSelectionStartIndex as getSelectionStartIndex 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function getSelectionEndIndex -}
{#fun cegui_edtbx_getSelectionEndIndex as getSelectionEndIndex 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function getSelectionLength -}
{#fun cegui_edtbx_getSelectionLength as getSelectionLength 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function getMaskCodePoint -}
{#fun cegui_edtbx_getMaskCodePoint as getMaskCodePoint 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function getMaxTextLength -}
{#fun cegui_edtbx_getMaxTextLength as getMaxTextLength 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function setReadOnly -}
{#fun cegui_edtbx_setReadOnly as setReadOnly 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function setTextMasked -}
{#fun cegui_edtbx_setTextMasked as setTextMasked 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function setValidationString -}
{#fun cegui_edtbx_setValidationString as setValidationString 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' } -> `()'  #}

{- function setCaratIndex -}
{#fun cegui_edtbx_setCaratIndex as setCaratIndex 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' } -> `()'  #}

{- function setSelection -}
{#fun cegui_edtbx_setSelection as setSelection 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 fromIntegral `Int' } -> `()'  #}

{- function setMaskCodePoint -}
{#fun cegui_edtbx_setMaskCodePoint as setMaskCodePoint 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' } -> `()'  #}

{- function setMaxTextLength -}
{#fun cegui_edtbx_setMaxTextLength as setMaxTextLength 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' } -> `()'  #}

{- function Editbox -}
{#fun cegui_edtbx_construct as new 
{ withCString* `String' ,
 withCString* `String' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function ~Editbox -}
{#fun cegui_edtbx_destruct as delete 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

