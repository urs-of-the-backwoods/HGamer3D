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


-- ClassMultiColumnList.chs

-- 

module HGamer3D.Bindings.CEGUI.ClassMultiColumnList where

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
{# import HGamer3D.Bindings.CEGUI.EnumSelectionMode #}

#include "ClassMultiColumnList.h"
{- function isUserSortControlEnabled -}
{#fun cegui_mltclmlst_isUserSortControlEnabled as isUserSortControlEnabled 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isUserColumnSizingEnabled -}
{#fun cegui_mltclmlst_isUserColumnSizingEnabled as isUserColumnSizingEnabled 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isUserColumnDraggingEnabled -}
{#fun cegui_mltclmlst_isUserColumnDraggingEnabled as isUserColumnDraggingEnabled 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function getColumnCount -}
{#fun cegui_mltclmlst_getColumnCount as getColumnCount 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function getRowCount -}
{#fun cegui_mltclmlst_getRowCount as getRowCount 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function getSortColumn -}
{#fun cegui_mltclmlst_getSortColumn as getSortColumn 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function getColumnWithID -}
{#fun cegui_mltclmlst_getColumnWithID as getColumnWithID 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function getColumnWithHeaderText -}
{#fun cegui_mltclmlst_getColumnWithHeaderText as getColumnWithHeaderText 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function getSortDirection -}
{#fun cegui_mltclmlst_getSortDirection as getSortDirection 
{ withHG3DClass* `HG3DClass' ,
 alloca- `EnumSortDirection' peekEnumUtil*} -> `()'  #}

{- function getHeaderSegmentForColumn -}
{#fun cegui_mltclmlst_getHeaderSegmentForColumn as getHeaderSegmentForColumn 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getItemRowIndex -}
{#fun cegui_mltclmlst_getItemRowIndex as getItemRowIndex 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function getItemColumnIndex -}
{#fun cegui_mltclmlst_getItemColumnIndex as getItemColumnIndex 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function isListboxItemInColumn -}
{#fun cegui_mltclmlst_isListboxItemInColumn as isListboxItemInColumn 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isListboxItemInRow -}
{#fun cegui_mltclmlst_isListboxItemInRow as isListboxItemInRow 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isListboxItemInList -}
{#fun cegui_mltclmlst_isListboxItemInList as isListboxItemInList 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function findColumnItemWithText -}
{#fun cegui_mltclmlst_findColumnItemWithText as findColumnItemWithText 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 fromIntegral `Int' ,
 withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function findRowItemWithText -}
{#fun cegui_mltclmlst_findRowItemWithText as findRowItemWithText 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 fromIntegral `Int' ,
 withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function findListItemWithText -}
{#fun cegui_mltclmlst_findListItemWithText as findListItemWithText 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getFirstSelectedItem -}
{#fun cegui_mltclmlst_getFirstSelectedItem as getFirstSelectedItem 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getNextSelected -}
{#fun cegui_mltclmlst_getNextSelected as getNextSelected 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getSelectedCount -}
{#fun cegui_mltclmlst_getSelectedCount as getSelectedCount 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function getNominatedSelectionColumnID -}
{#fun cegui_mltclmlst_getNominatedSelectionColumnID as getNominatedSelectionColumnID 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function getNominatedSelectionColumn -}
{#fun cegui_mltclmlst_getNominatedSelectionColumn as getNominatedSelectionColumn 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function getNominatedSelectionRow -}
{#fun cegui_mltclmlst_getNominatedSelectionRow as getNominatedSelectionRow 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function getSelectionMode -}
{#fun cegui_mltclmlst_getSelectionMode as getSelectionMode 
{ withHG3DClass* `HG3DClass' ,
 alloca- `EnumSelectionMode' peekEnumUtil*} -> `()'  #}

{- function isVertScrollbarAlwaysShown -}
{#fun cegui_mltclmlst_isVertScrollbarAlwaysShown as isVertScrollbarAlwaysShown 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isHorzScrollbarAlwaysShown -}
{#fun cegui_mltclmlst_isHorzScrollbarAlwaysShown as isHorzScrollbarAlwaysShown 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function getColumnID -}
{#fun cegui_mltclmlst_getColumnID as getColumnID 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function getRowID -}
{#fun cegui_mltclmlst_getRowID as getRowID 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function getRowWithID -}
{#fun cegui_mltclmlst_getRowWithID as getRowWithID 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function getVertScrollbar -}
{#fun cegui_mltclmlst_getVertScrollbar as getVertScrollbar 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getHorzScrollbar -}
{#fun cegui_mltclmlst_getHorzScrollbar as getHorzScrollbar 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getListHeader -}
{#fun cegui_mltclmlst_getListHeader as getListHeader 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getTotalRowsHeight -}
{#fun cegui_mltclmlst_getTotalRowsHeight as getTotalRowsHeight 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function getWidestColumnItemWidth -}
{#fun cegui_mltclmlst_getWidestColumnItemWidth as getWidestColumnItemWidth 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function getHighestRowItemHeight -}
{#fun cegui_mltclmlst_getHighestRowItemHeight as getHighestRowItemHeight 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function initialiseComponents -}
{#fun cegui_mltclmlst_initialiseComponents as initialiseComponents 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function resetList -}
{#fun cegui_mltclmlst_resetList as resetList 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function addColumn -}
{#fun cegui_mltclmlst_addColumn as addColumn 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 fromIntegral `Int' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function insertColumn -}
{#fun cegui_mltclmlst_insertColumn as insertColumn 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 fromIntegral `Int' ,
 withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' } -> `()'  #}

{- function removeColumn -}
{#fun cegui_mltclmlst_removeColumn as removeColumn 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' } -> `()'  #}

{- function removeColumnWithID -}
{#fun cegui_mltclmlst_removeColumnWithID as removeColumnWithID 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' } -> `()'  #}

{- function moveColumn -}
{#fun cegui_mltclmlst_moveColumn as moveColumn 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 fromIntegral `Int' } -> `()'  #}

{- function moveColumnWithID -}
{#fun cegui_mltclmlst_moveColumnWithID as moveColumnWithID 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 fromIntegral `Int' } -> `()'  #}

{- function addRow -}
{#fun cegui_mltclmlst_addRow as addRow 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function addRow2 -}
{#fun cegui_mltclmlst_addRow2 as addRow2 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 fromIntegral `Int' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function insertRow -}
{#fun cegui_mltclmlst_insertRow as insertRow 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 fromIntegral `Int' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function insertRow2 -}
{#fun cegui_mltclmlst_insertRow2 as insertRow2 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 fromIntegral `Int' ,
 fromIntegral `Int' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function removeRow -}
{#fun cegui_mltclmlst_removeRow as removeRow 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' } -> `()'  #}

{- function setItem2 -}
{#fun cegui_mltclmlst_setItem2 as setItem2 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 fromIntegral `Int' } -> `()'  #}

{- function setSelectionMode -}
{#fun cegui_mltclmlst_setSelectionMode as setSelectionMode 
{ withHG3DClass* `HG3DClass' ,
 cIntFromEnum `EnumSelectionMode' } -> `()'  #}

{- function setNominatedSelectionColumnID -}
{#fun cegui_mltclmlst_setNominatedSelectionColumnID as setNominatedSelectionColumnID 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' } -> `()'  #}

{- function setNominatedSelectionColumn -}
{#fun cegui_mltclmlst_setNominatedSelectionColumn as setNominatedSelectionColumn 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' } -> `()'  #}

{- function setNominatedSelectionRow -}
{#fun cegui_mltclmlst_setNominatedSelectionRow as setNominatedSelectionRow 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' } -> `()'  #}

{- function setSortDirection -}
{#fun cegui_mltclmlst_setSortDirection as setSortDirection 
{ withHG3DClass* `HG3DClass' ,
 cIntFromEnum `EnumSortDirection' } -> `()'  #}

{- function setSortColumn -}
{#fun cegui_mltclmlst_setSortColumn as setSortColumn 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' } -> `()'  #}

{- function setSortColumnByID -}
{#fun cegui_mltclmlst_setSortColumnByID as setSortColumnByID 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' } -> `()'  #}

{- function setShowVertScrollbar -}
{#fun cegui_mltclmlst_setShowVertScrollbar as setShowVertScrollbar 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function setShowHorzScrollbar -}
{#fun cegui_mltclmlst_setShowHorzScrollbar as setShowHorzScrollbar 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function clearAllSelections -}
{#fun cegui_mltclmlst_clearAllSelections as clearAllSelections 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function setItemSelectState -}
{#fun cegui_mltclmlst_setItemSelectState as setItemSelectState 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function handleUpdatedItemData -}
{#fun cegui_mltclmlst_handleUpdatedItemData as handleUpdatedItemData 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function setColumnHeaderWidth -}
{#fun cegui_mltclmlst_setColumnHeaderWidth as setColumnHeaderWidth 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function setUserSortControlEnabled -}
{#fun cegui_mltclmlst_setUserSortControlEnabled as setUserSortControlEnabled 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function setUserColumnSizingEnabled -}
{#fun cegui_mltclmlst_setUserColumnSizingEnabled as setUserColumnSizingEnabled 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function setUserColumnDraggingEnabled -}
{#fun cegui_mltclmlst_setUserColumnDraggingEnabled as setUserColumnDraggingEnabled 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function autoSizeColumnHeader -}
{#fun cegui_mltclmlst_autoSizeColumnHeader as autoSizeColumnHeader 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' } -> `()'  #}

{- function setRowID -}
{#fun cegui_mltclmlst_setRowID as setRowID 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 fromIntegral `Int' } -> `()'  #}

{- function MultiColumnList -}
{#fun cegui_mltclmlst_construct as new 
{ withCString* `String' ,
 withCString* `String' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function ~MultiColumnList -}
{#fun cegui_mltclmlst_destruct as delete 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

