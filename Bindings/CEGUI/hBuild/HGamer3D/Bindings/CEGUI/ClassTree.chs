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


-- ClassTree.chs

-- 

module HGamer3D.Bindings.CEGUI.ClassTree where

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

#include "ClassTree.h"
{- function doTreeRender -}
{#fun cegui_tree_doTreeRender as doTreeRender 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function doScrollbars -}
{#fun cegui_tree_doScrollbars as doScrollbars 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function getItemCount -}
{#fun cegui_tree_getItemCount as getItemCount 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function getSelectedCount -}
{#fun cegui_tree_getSelectedCount as getSelectedCount 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function isSortEnabled -}
{#fun cegui_tree_isSortEnabled as isSortEnabled 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function getVertScrollbar -}
{#fun cegui_tree_getVertScrollbar as getVertScrollbar 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getHorzScrollbar -}
{#fun cegui_tree_getHorzScrollbar as getHorzScrollbar 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function isMultiselectEnabled -}
{#fun cegui_tree_isMultiselectEnabled as isMultiselectEnabled 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isItemTooltipsEnabled -}
{#fun cegui_tree_isItemTooltipsEnabled as isItemTooltipsEnabled 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isVertScrollbarAlwaysShown -}
{#fun cegui_tree_isVertScrollbarAlwaysShown as isVertScrollbarAlwaysShown 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isHorzScrollbarAlwaysShown -}
{#fun cegui_tree_isHorzScrollbarAlwaysShown as isHorzScrollbarAlwaysShown 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function initialise -}
{#fun cegui_tree_initialise as initialise 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function resetList -}
{#fun cegui_tree_resetList as resetList 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function clearAllSelections -}
{#fun cegui_tree_clearAllSelections as clearAllSelections 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function setSortingEnabled -}
{#fun cegui_tree_setSortingEnabled as setSortingEnabled 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function setMultiselectEnabled -}
{#fun cegui_tree_setMultiselectEnabled as setMultiselectEnabled 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function setShowVertScrollbar -}
{#fun cegui_tree_setShowVertScrollbar as setShowVertScrollbar 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function setShowHorzScrollbar -}
{#fun cegui_tree_setShowHorzScrollbar as setShowHorzScrollbar 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function setItemTooltipsEnabled -}
{#fun cegui_tree_setItemTooltipsEnabled as setItemTooltipsEnabled 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function setItemSelectState2 -}
{#fun cegui_tree_setItemSelectState2 as setItemSelectState2 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 fromBool `Bool' } -> `()'  #}

{- function setLookNFeel -}
{#fun cegui_tree_setLookNFeel as setLookNFeel 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' } -> `()'  #}

{- function handleUpdatedItemData -}
{#fun cegui_tree_handleUpdatedItemData as handleUpdatedItemData 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function Tree -}
{#fun cegui_tree_construct as new 
{ withCString* `String' ,
 withCString* `String' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function ~Tree -}
{#fun cegui_tree_destruct as delete 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

