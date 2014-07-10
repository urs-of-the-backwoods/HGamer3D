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


-- ClassListHeader.chs

-- 

module HGamer3D.Bindings.CEGUI.ClassListHeader where

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

#include "ClassListHeader.h"
{- function getColumnCount -}
{#fun cegui_lsthdr_getColumnCount as getColumnCount 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function getSegmentFromColumn -}
{#fun cegui_lsthdr_getSegmentFromColumn as getSegmentFromColumn 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getSegmentFromID -}
{#fun cegui_lsthdr_getSegmentFromID as getSegmentFromID 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getSortSegment -}
{#fun cegui_lsthdr_getSortSegment as getSortSegment 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getColumnFromSegment -}
{#fun cegui_lsthdr_getColumnFromSegment as getColumnFromSegment 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function getColumnFromID -}
{#fun cegui_lsthdr_getColumnFromID as getColumnFromID 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function getSortColumn -}
{#fun cegui_lsthdr_getSortColumn as getSortColumn 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function getColumnWithText -}
{#fun cegui_lsthdr_getColumnWithText as getColumnWithText 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function getPixelOffsetToSegment -}
{#fun cegui_lsthdr_getPixelOffsetToSegment as getPixelOffsetToSegment 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function getPixelOffsetToColumn -}
{#fun cegui_lsthdr_getPixelOffsetToColumn as getPixelOffsetToColumn 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function getTotalSegmentsPixelExtent -}
{#fun cegui_lsthdr_getTotalSegmentsPixelExtent as getTotalSegmentsPixelExtent 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function getSortDirection -}
{#fun cegui_lsthdr_getSortDirection as getSortDirection 
{ withHG3DClass* `HG3DClass' ,
 alloca- `EnumSortDirection' peekEnumUtil*} -> `()'  #}

{- function isSortingEnabled -}
{#fun cegui_lsthdr_isSortingEnabled as isSortingEnabled 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isColumnSizingEnabled -}
{#fun cegui_lsthdr_isColumnSizingEnabled as isColumnSizingEnabled 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isColumnDraggingEnabled -}
{#fun cegui_lsthdr_isColumnDraggingEnabled as isColumnDraggingEnabled 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function getSegmentOffset -}
{#fun cegui_lsthdr_getSegmentOffset as getSegmentOffset 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function setSortingEnabled -}
{#fun cegui_lsthdr_setSortingEnabled as setSortingEnabled 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function setSortDirection -}
{#fun cegui_lsthdr_setSortDirection as setSortDirection 
{ withHG3DClass* `HG3DClass' ,
 cIntFromEnum `EnumSortDirection' } -> `()'  #}

{- function setSortSegment -}
{#fun cegui_lsthdr_setSortSegment as setSortSegment 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function setSortColumn -}
{#fun cegui_lsthdr_setSortColumn as setSortColumn 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' } -> `()'  #}

{- function setSortColumnFromID -}
{#fun cegui_lsthdr_setSortColumnFromID as setSortColumnFromID 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' } -> `()'  #}

{- function setColumnSizingEnabled -}
{#fun cegui_lsthdr_setColumnSizingEnabled as setColumnSizingEnabled 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function setColumnDraggingEnabled -}
{#fun cegui_lsthdr_setColumnDraggingEnabled as setColumnDraggingEnabled 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function addColumn -}
{#fun cegui_lsthdr_addColumn as addColumn 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 fromIntegral `Int' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function insertColumn -}
{#fun cegui_lsthdr_insertColumn as insertColumn 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 fromIntegral `Int' ,
 withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' } -> `()'  #}

{- function insertColumn2 -}
{#fun cegui_lsthdr_insertColumn2 as insertColumn2 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 fromIntegral `Int' ,
 withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function removeColumn -}
{#fun cegui_lsthdr_removeColumn as removeColumn 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' } -> `()'  #}

{- function removeSegment -}
{#fun cegui_lsthdr_removeSegment as removeSegment 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function moveColumn -}
{#fun cegui_lsthdr_moveColumn as moveColumn 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 fromIntegral `Int' } -> `()'  #}

{- function moveColumn2 -}
{#fun cegui_lsthdr_moveColumn2 as moveColumn2 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function moveSegment -}
{#fun cegui_lsthdr_moveSegment as moveSegment 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' } -> `()'  #}

{- function moveSegment2 -}
{#fun cegui_lsthdr_moveSegment2 as moveSegment2 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function setSegmentOffset -}
{#fun cegui_lsthdr_setSegmentOffset as setSegmentOffset 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' } -> `()'  #}

{- function setColumnWidth -}
{#fun cegui_lsthdr_setColumnWidth as setColumnWidth 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function ListHeader -}
{#fun cegui_lsthdr_construct as new 
{ withCString* `String' ,
 withCString* `String' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function ~ListHeader -}
{#fun cegui_lsthdr_destruct as delete 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

