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


-- ClassListHeaderSegment.chs

-- 

module HGamer3D.Bindings.CEGUI.ClassListHeaderSegment where

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
{# import HGamer3D.Bindings.CEGUI.EnumSortDirection #}

#include "ClassListHeaderSegment.h"
{- function isSizingEnabled -}
{#fun cegui_lsthdrsgm_isSizingEnabled as isSizingEnabled 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function getSortDirection -}
{#fun cegui_lsthdrsgm_getSortDirection as getSortDirection 
{ withHG3DClass* `HG3DClass' ,
 alloca- `EnumSortDirection' peekEnumUtil*} -> `()'  #}

{- function isDragMovingEnabled -}
{#fun cegui_lsthdrsgm_isDragMovingEnabled as isDragMovingEnabled 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isClickable -}
{#fun cegui_lsthdrsgm_isClickable as isClickable 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isSegmentHovering -}
{#fun cegui_lsthdrsgm_isSegmentHovering as isSegmentHovering 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isSegmentPushed -}
{#fun cegui_lsthdrsgm_isSegmentPushed as isSegmentPushed 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isSplitterHovering -}
{#fun cegui_lsthdrsgm_isSplitterHovering as isSplitterHovering 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isBeingDragMoved -}
{#fun cegui_lsthdrsgm_isBeingDragMoved as isBeingDragMoved 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isBeingDragSized -}
{#fun cegui_lsthdrsgm_isBeingDragSized as isBeingDragSized 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function setSizingEnabled -}
{#fun cegui_lsthdrsgm_setSizingEnabled as setSizingEnabled 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function setSortDirection -}
{#fun cegui_lsthdrsgm_setSortDirection as setSortDirection 
{ withHG3DClass* `HG3DClass' ,
 cIntFromEnum `EnumSortDirection' } -> `()'  #}

{- function setDragMovingEnabled -}
{#fun cegui_lsthdrsgm_setDragMovingEnabled as setDragMovingEnabled 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function setClickable -}
{#fun cegui_lsthdrsgm_setClickable as setClickable 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function setSizingCursorImage2 -}
{#fun cegui_lsthdrsgm_setSizingCursorImage2 as setSizingCursorImage2 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 withCString* `String' } -> `()'  #}

{- function setMovingCursorImage2 -}
{#fun cegui_lsthdrsgm_setMovingCursorImage2 as setMovingCursorImage2 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 withCString* `String' } -> `()'  #}

{- function ListHeaderSegment -}
{#fun cegui_lsthdrsgm_construct as new 
{ withCString* `String' ,
 withCString* `String' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function ~ListHeaderSegment -}
{#fun cegui_lsthdrsgm_destruct as delete 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

