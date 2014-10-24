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


-- ClassDragContainer.chs

-- 

module HGamer3D.Bindings.CEGUI.ClassDragContainer where

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

#include "ClassDragContainer.h"
{- function DragContainer -}
{#fun cegui_drgcnt_construct as new 
{ withCString* `String' ,
 withCString* `String' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function ~DragContainer -}
{#fun cegui_drgcnt_destruct as delete 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function isDraggingEnabled -}
{#fun cegui_drgcnt_isDraggingEnabled as isDraggingEnabled 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function setDraggingEnabled -}
{#fun cegui_drgcnt_setDraggingEnabled as setDraggingEnabled 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function isBeingDragged -}
{#fun cegui_drgcnt_isBeingDragged as isBeingDragged 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function getPixelDragThreshold -}
{#fun cegui_drgcnt_getPixelDragThreshold as getPixelDragThreshold 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function setPixelDragThreshold -}
{#fun cegui_drgcnt_setPixelDragThreshold as setPixelDragThreshold 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' } -> `()'  #}

{- function getDragAlpha -}
{#fun cegui_drgcnt_getDragAlpha as getDragAlpha 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function setDragAlpha -}
{#fun cegui_drgcnt_setDragAlpha as setDragAlpha 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' } -> `()'  #}

{- function setDragCursorImage3 -}
{#fun cegui_drgcnt_setDragCursorImage3 as setDragCursorImage3 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 withCString* `String' } -> `()'  #}

{- function getCurrentDropTarget -}
{#fun cegui_drgcnt_getCurrentDropTarget as getCurrentDropTarget 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function isStickyModeEnabled -}
{#fun cegui_drgcnt_isStickyModeEnabled as isStickyModeEnabled 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function setStickyModeEnabled -}
{#fun cegui_drgcnt_setStickyModeEnabled as setStickyModeEnabled 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function pickUp -}
{#fun cegui_drgcnt_pickUp as pickUp 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function setFixedDragOffset -}
{#fun cegui_drgcnt_setFixedDragOffset as setFixedDragOffset 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function getFixedDragOffset -}
{#fun cegui_drgcnt_getFixedDragOffset as getFixedDragOffset 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function setUsingFixedDragOffset -}
{#fun cegui_drgcnt_setUsingFixedDragOffset as setUsingFixedDragOffset 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function isUsingFixedDragOffset -}
{#fun cegui_drgcnt_isUsingFixedDragOffset as isUsingFixedDragOffset 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

