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


-- ClassScrollbar.chs

-- 

module HGamer3D.Bindings.CEGUI.ClassScrollbar where

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

#include "ClassScrollbar.h"
{- function getDocumentSize -}
{#fun cegui_scrlbr_getDocumentSize as getDocumentSize 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function getPageSize -}
{#fun cegui_scrlbr_getPageSize as getPageSize 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function getStepSize -}
{#fun cegui_scrlbr_getStepSize as getStepSize 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function getOverlapSize -}
{#fun cegui_scrlbr_getOverlapSize as getOverlapSize 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function getScrollPosition -}
{#fun cegui_scrlbr_getScrollPosition as getScrollPosition 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function getIncreaseButton -}
{#fun cegui_scrlbr_getIncreaseButton as getIncreaseButton 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getDecreaseButton -}
{#fun cegui_scrlbr_getDecreaseButton as getDecreaseButton 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getThumb -}
{#fun cegui_scrlbr_getThumb as getThumb 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function initialiseComponents -}
{#fun cegui_scrlbr_initialiseComponents as initialiseComponents 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function setDocumentSize -}
{#fun cegui_scrlbr_setDocumentSize as setDocumentSize 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' } -> `()'  #}

{- function setPageSize -}
{#fun cegui_scrlbr_setPageSize as setPageSize 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' } -> `()'  #}

{- function setStepSize -}
{#fun cegui_scrlbr_setStepSize as setStepSize 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' } -> `()'  #}

{- function setOverlapSize -}
{#fun cegui_scrlbr_setOverlapSize as setOverlapSize 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' } -> `()'  #}

{- function setScrollPosition -}
{#fun cegui_scrlbr_setScrollPosition as setScrollPosition 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' } -> `()'  #}

{- function setEndLockEnabled -}
{#fun cegui_scrlbr_setEndLockEnabled as setEndLockEnabled 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function isEndLockEnabled -}
{#fun cegui_scrlbr_isEndLockEnabled as isEndLockEnabled 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function Scrollbar -}
{#fun cegui_scrlbr_construct as new 
{ withCString* `String' ,
 withCString* `String' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function ~Scrollbar -}
{#fun cegui_scrlbr_destruct as delete 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

