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


-- ClassListboxItem.chs

-- 

module HGamer3D.Bindings.CEGUI.ClassListboxItem where

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

#include "ClassListboxItem.h"
{- function ~ListboxItem -}
{#fun cegui_lstbxitm_destruct as delete 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function getTooltipText -}
{#fun cegui_lstbxitm_getTooltipText as getTooltipText 
{ withHG3DClass* `HG3DClass' ,
 alloc64k- `String' peekCString*} -> `()'  #}

{- function getText -}
{#fun cegui_lstbxitm_getText as getText 
{ withHG3DClass* `HG3DClass' ,
 alloc64k- `String' peekCString*} -> `()'  #}

{- function getTextVisual -}
{#fun cegui_lstbxitm_getTextVisual as getTextVisual 
{ withHG3DClass* `HG3DClass' ,
 alloc64k- `String' peekCString*} -> `()'  #}

{- function getID -}
{#fun cegui_lstbxitm_getID as getID 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function isSelected -}
{#fun cegui_lstbxitm_isSelected as isSelected 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isDisabled -}
{#fun cegui_lstbxitm_isDisabled as isDisabled 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isAutoDeleted -}
{#fun cegui_lstbxitm_isAutoDeleted as isAutoDeleted 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function getOwnerWindow -}
{#fun cegui_lstbxitm_getOwnerWindow as getOwnerWindow 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function setText -}
{#fun cegui_lstbxitm_setText as setText 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' } -> `()'  #}

{- function setTooltipText -}
{#fun cegui_lstbxitm_setTooltipText as setTooltipText 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' } -> `()'  #}

{- function setID -}
{#fun cegui_lstbxitm_setID as setID 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' } -> `()'  #}

{- function setSelected -}
{#fun cegui_lstbxitm_setSelected as setSelected 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function setDisabled -}
{#fun cegui_lstbxitm_setDisabled as setDisabled 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function setAutoDeleted -}
{#fun cegui_lstbxitm_setAutoDeleted as setAutoDeleted 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function setOwnerWindow -}
{#fun cegui_lstbxitm_setOwnerWindow as setOwnerWindow 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function setSelectionBrushImage2 -}
{#fun cegui_lstbxitm_setSelectionBrushImage2 as setSelectionBrushImage2 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 withCString* `String' } -> `()'  #}

