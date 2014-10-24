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


-- ClassSpinner.chs

-- 

module HGamer3D.Bindings.CEGUI.ClassSpinner where

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
{# import HGamer3D.Bindings.CEGUI.EnumTextInputMode #}

#include "ClassSpinner.h"
{- function Spinner -}
{#fun cegui_spnnr_construct as new 
{ withCString* `String' ,
 withCString* `String' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function ~Spinner -}
{#fun cegui_spnnr_destruct as delete 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function initialiseComponents -}
{#fun cegui_spnnr_initialiseComponents as initialiseComponents 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function getCurrentValue -}
{#fun cegui_spnnr_getCurrentValue as getCurrentValue 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Double' peekFloatConv*} -> `()'  #}

{- function getStepSize -}
{#fun cegui_spnnr_getStepSize as getStepSize 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Double' peekFloatConv*} -> `()'  #}

{- function getMaximumValue -}
{#fun cegui_spnnr_getMaximumValue as getMaximumValue 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Double' peekFloatConv*} -> `()'  #}

{- function getMinimumValue -}
{#fun cegui_spnnr_getMinimumValue as getMinimumValue 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Double' peekFloatConv*} -> `()'  #}

{- function getTextInputMode -}
{#fun cegui_spnnr_getTextInputMode as getTextInputMode 
{ withHG3DClass* `HG3DClass' ,
 alloca- `EnumTextInputMode' peekEnumUtil*} -> `()'  #}

{- function setCurrentValue -}
{#fun cegui_spnnr_setCurrentValue as setCurrentValue 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Double' } -> `()'  #}

{- function setStepSize -}
{#fun cegui_spnnr_setStepSize as setStepSize 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Double' } -> `()'  #}

{- function setMaximumValue -}
{#fun cegui_spnnr_setMaximumValue as setMaximumValue 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Double' } -> `()'  #}

{- function setMinimumValue -}
{#fun cegui_spnnr_setMinimumValue as setMinimumValue 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Double' } -> `()'  #}

{- function setTextInputMode -}
{#fun cegui_spnnr_setTextInputMode as setTextInputMode 
{ withHG3DClass* `HG3DClass' ,
 cIntFromEnum `EnumTextInputMode' } -> `()'  #}

