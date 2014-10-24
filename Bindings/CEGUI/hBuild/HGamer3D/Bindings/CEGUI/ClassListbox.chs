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


-- ClassListbox.chs

-- 

module HGamer3D.Bindings.CEGUI.ClassListbox where

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

#include "ClassListbox.h"
{- function getItemCount -}
{#fun cegui_lstbx_getItemCount as getItemCount 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function getSelectedCount -}
{#fun cegui_lstbx_getSelectedCount as getSelectedCount 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function getFirstSelectedItem -}
{#fun cegui_lstbx_getFirstSelectedItem as getFirstSelectedItem 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getNextSelected -}
{#fun cegui_lstbx_getNextSelected as getNextSelected 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getListboxItemFromIndex -}
{#fun cegui_lstbx_getListboxItemFromIndex as getListboxItemFromIndex 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getItemIndex -}
{#fun cegui_lstbx_getItemIndex as getItemIndex 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function isSortEnabled -}
{#fun cegui_lstbx_isSortEnabled as isSortEnabled 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isMultiselectEnabled -}
{#fun cegui_lstbx_isMultiselectEnabled as isMultiselectEnabled 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isItemTooltipsEnabled -}
{#fun cegui_lstbx_isItemTooltipsEnabled as isItemTooltipsEnabled 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isItemSelected -}
{#fun cegui_lstbx_isItemSelected as isItemSelected 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function findItemWithText -}
{#fun cegui_lstbx_findItemWithText as findItemWithText 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function isListboxItemInList -}
{#fun cegui_lstbx_isListboxItemInList as isListboxItemInList 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isVertScrollbarAlwaysShown -}
{#fun cegui_lstbx_isVertScrollbarAlwaysShown as isVertScrollbarAlwaysShown 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isHorzScrollbarAlwaysShown -}
{#fun cegui_lstbx_isHorzScrollbarAlwaysShown as isHorzScrollbarAlwaysShown 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function initialiseComponents -}
{#fun cegui_lstbx_initialiseComponents as initialiseComponents 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function resetList -}
{#fun cegui_lstbx_resetList as resetList 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function addItem -}
{#fun cegui_lstbx_addItem as addItem 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function insertItem -}
{#fun cegui_lstbx_insertItem as insertItem 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function removeItem -}
{#fun cegui_lstbx_removeItem as removeItem 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function clearAllSelections -}
{#fun cegui_lstbx_clearAllSelections as clearAllSelections 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function setSortingEnabled -}
{#fun cegui_lstbx_setSortingEnabled as setSortingEnabled 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function setMultiselectEnabled -}
{#fun cegui_lstbx_setMultiselectEnabled as setMultiselectEnabled 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function setShowVertScrollbar -}
{#fun cegui_lstbx_setShowVertScrollbar as setShowVertScrollbar 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function setShowHorzScrollbar -}
{#fun cegui_lstbx_setShowHorzScrollbar as setShowHorzScrollbar 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function setItemTooltipsEnabled -}
{#fun cegui_lstbx_setItemTooltipsEnabled as setItemTooltipsEnabled 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function setItemSelectState -}
{#fun cegui_lstbx_setItemSelectState as setItemSelectState 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function setItemSelectState2 -}
{#fun cegui_lstbx_setItemSelectState2 as setItemSelectState2 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 fromBool `Bool' } -> `()'  #}

{- function handleUpdatedItemData -}
{#fun cegui_lstbx_handleUpdatedItemData as handleUpdatedItemData 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function ensureItemIsVisible -}
{#fun cegui_lstbx_ensureItemIsVisible as ensureItemIsVisible 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' } -> `()'  #}

{- function ensureItemIsVisible2 -}
{#fun cegui_lstbx_ensureItemIsVisible2 as ensureItemIsVisible2 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function getVertScrollbar -}
{#fun cegui_lstbx_getVertScrollbar as getVertScrollbar 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getHorzScrollbar -}
{#fun cegui_lstbx_getHorzScrollbar as getHorzScrollbar 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getTotalItemsHeight -}
{#fun cegui_lstbx_getTotalItemsHeight as getTotalItemsHeight 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function getWidestItemWidth -}
{#fun cegui_lstbx_getWidestItemWidth as getWidestItemWidth 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function Listbox -}
{#fun cegui_lstbx_construct as new 
{ withCString* `String' ,
 withCString* `String' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function ~Listbox -}
{#fun cegui_lstbx_destruct as delete 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

