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


-- ClassItemListbox.chs

-- 

module HGamer3D.Bindings.CEGUI.ClassItemListbox where

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

#include "ClassItemListbox.h"
{- function getSelectedCount -}
{#fun cegui_itmlstbx_getSelectedCount as getSelectedCount 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function getLastSelectedItem -}
{#fun cegui_itmlstbx_getLastSelectedItem as getLastSelectedItem 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getFirstSelectedItem -}
{#fun cegui_itmlstbx_getFirstSelectedItem as getFirstSelectedItem 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getNextSelectedItem -}
{#fun cegui_itmlstbx_getNextSelectedItem as getNextSelectedItem 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getNextSelectedItemAfter -}
{#fun cegui_itmlstbx_getNextSelectedItemAfter as getNextSelectedItemAfter 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function isMultiSelectEnabled -}
{#fun cegui_itmlstbx_isMultiSelectEnabled as isMultiSelectEnabled 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isItemSelected -}
{#fun cegui_itmlstbx_isItemSelected as isItemSelected 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function initialiseComponents -}
{#fun cegui_itmlstbx_initialiseComponents as initialiseComponents 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function setMultiSelectEnabled -}
{#fun cegui_itmlstbx_setMultiSelectEnabled as setMultiSelectEnabled 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function clearAllSelections -}
{#fun cegui_itmlstbx_clearAllSelections as clearAllSelections 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function selectRange -}
{#fun cegui_itmlstbx_selectRange as selectRange 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 fromIntegral `Int' } -> `()'  #}

{- function selectAllItems -}
{#fun cegui_itmlstbx_selectAllItems as selectAllItems 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function ItemListbox -}
{#fun cegui_itmlstbx_construct as new 
{ withCString* `String' ,
 withCString* `String' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function ~ItemListbox -}
{#fun cegui_itmlstbx_destruct as delete 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function layoutItemWidgets -}
{#fun cegui_itmlstbx_layoutItemWidgets as layoutItemWidgets 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function testClassName_impl -}
{#fun cegui_itmlstbx_testClassName_impl as testClassNameImpl 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function notifyItemClicked -}
{#fun cegui_itmlstbx_notifyItemClicked as notifyItemClicked 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function notifyItemSelectState -}
{#fun cegui_itmlstbx_notifyItemSelectState as notifyItemSelectState 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

