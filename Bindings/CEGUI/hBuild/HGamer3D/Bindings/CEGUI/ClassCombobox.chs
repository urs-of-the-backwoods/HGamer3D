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


-- ClassCombobox.chs

-- 

module HGamer3D.Bindings.CEGUI.ClassCombobox where

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

#include "ClassCombobox.h"
{- function getSingleClickEnabled -}
{#fun cegui_cmbbx_getSingleClickEnabled as getSingleClickEnabled 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isDropDownListVisible -}
{#fun cegui_cmbbx_isDropDownListVisible as isDropDownListVisible 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function getEditbox -}
{#fun cegui_cmbbx_getEditbox as getEditbox 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getPushButton -}
{#fun cegui_cmbbx_getPushButton as getPushButton 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getDropList -}
{#fun cegui_cmbbx_getDropList as getDropList 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function hasInputFocus -}
{#fun cegui_cmbbx_hasInputFocus as hasInputFocus 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isReadOnly -}
{#fun cegui_cmbbx_isReadOnly as isReadOnly 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isTextValid -}
{#fun cegui_cmbbx_isTextValid as isTextValid 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function getValidationString -}
{#fun cegui_cmbbx_getValidationString as getValidationString 
{ withHG3DClass* `HG3DClass' ,
 alloc64k- `String' peekCString*} -> `()'  #}

{- function getCaratIndex -}
{#fun cegui_cmbbx_getCaratIndex as getCaratIndex 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function getSelectionStartIndex -}
{#fun cegui_cmbbx_getSelectionStartIndex as getSelectionStartIndex 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function getSelectionEndIndex -}
{#fun cegui_cmbbx_getSelectionEndIndex as getSelectionEndIndex 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function getSelectionLength -}
{#fun cegui_cmbbx_getSelectionLength as getSelectionLength 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function getMaxTextLength -}
{#fun cegui_cmbbx_getMaxTextLength as getMaxTextLength 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function getItemCount -}
{#fun cegui_cmbbx_getItemCount as getItemCount 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function getSelectedItem -}
{#fun cegui_cmbbx_getSelectedItem as getSelectedItem 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getListboxItemFromIndex -}
{#fun cegui_cmbbx_getListboxItemFromIndex as getListboxItemFromIndex 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getItemIndex -}
{#fun cegui_cmbbx_getItemIndex as getItemIndex 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function isSortEnabled -}
{#fun cegui_cmbbx_isSortEnabled as isSortEnabled 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isItemSelected -}
{#fun cegui_cmbbx_isItemSelected as isItemSelected 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function findItemWithText -}
{#fun cegui_cmbbx_findItemWithText as findItemWithText 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function isListboxItemInList -}
{#fun cegui_cmbbx_isListboxItemInList as isListboxItemInList 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isVertScrollbarAlwaysShown -}
{#fun cegui_cmbbx_isVertScrollbarAlwaysShown as isVertScrollbarAlwaysShown 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isHorzScrollbarAlwaysShown -}
{#fun cegui_cmbbx_isHorzScrollbarAlwaysShown as isHorzScrollbarAlwaysShown 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function initialiseComponents -}
{#fun cegui_cmbbx_initialiseComponents as initialiseComponents 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function showDropList -}
{#fun cegui_cmbbx_showDropList as showDropList 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function hideDropList -}
{#fun cegui_cmbbx_hideDropList as hideDropList 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function setSingleClickEnabled -}
{#fun cegui_cmbbx_setSingleClickEnabled as setSingleClickEnabled 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function setReadOnly -}
{#fun cegui_cmbbx_setReadOnly as setReadOnly 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function setValidationString -}
{#fun cegui_cmbbx_setValidationString as setValidationString 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' } -> `()'  #}

{- function setCaratIndex -}
{#fun cegui_cmbbx_setCaratIndex as setCaratIndex 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' } -> `()'  #}

{- function setSelection -}
{#fun cegui_cmbbx_setSelection as setSelection 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 fromIntegral `Int' } -> `()'  #}

{- function setMaxTextLength -}
{#fun cegui_cmbbx_setMaxTextLength as setMaxTextLength 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' } -> `()'  #}

{- function activateEditbox -}
{#fun cegui_cmbbx_activateEditbox as activateEditbox 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function resetList -}
{#fun cegui_cmbbx_resetList as resetList 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function addItem -}
{#fun cegui_cmbbx_addItem as addItem 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function insertItem -}
{#fun cegui_cmbbx_insertItem as insertItem 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function removeItem -}
{#fun cegui_cmbbx_removeItem as removeItem 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function clearAllSelections -}
{#fun cegui_cmbbx_clearAllSelections as clearAllSelections 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function setSortingEnabled -}
{#fun cegui_cmbbx_setSortingEnabled as setSortingEnabled 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function setShowVertScrollbar -}
{#fun cegui_cmbbx_setShowVertScrollbar as setShowVertScrollbar 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function setShowHorzScrollbar -}
{#fun cegui_cmbbx_setShowHorzScrollbar as setShowHorzScrollbar 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function setItemSelectState -}
{#fun cegui_cmbbx_setItemSelectState as setItemSelectState 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function setItemSelectState2 -}
{#fun cegui_cmbbx_setItemSelectState2 as setItemSelectState2 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 fromBool `Bool' } -> `()'  #}

{- function handleUpdatedListItemData -}
{#fun cegui_cmbbx_handleUpdatedListItemData as handleUpdatedListItemData 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function Combobox -}
{#fun cegui_cmbbx_construct as new 
{ withCString* `String' ,
 withCString* `String' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function ~Combobox -}
{#fun cegui_cmbbx_destruct as delete 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

