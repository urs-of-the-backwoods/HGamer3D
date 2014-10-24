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


-- ClassScrolledItemListBase.chs

-- abstrakte Klasse!

module HGamer3D.Bindings.CEGUI.ClassScrolledItemListBase where

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

#include "ClassScrolledItemListBase.h"
{- function isVertScrollbarAlwaysShown -}
{#fun cegui_scrlitmlstbs_isVertScrollbarAlwaysShown as isVertScrollbarAlwaysShown 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isHorzScrollbarAlwaysShown -}
{#fun cegui_scrlitmlstbs_isHorzScrollbarAlwaysShown as isHorzScrollbarAlwaysShown 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function getVertScrollbar -}
{#fun cegui_scrlitmlstbs_getVertScrollbar as getVertScrollbar 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getHorzScrollbar -}
{#fun cegui_scrlitmlstbs_getHorzScrollbar as getHorzScrollbar 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function setShowVertScrollbar -}
{#fun cegui_scrlitmlstbs_setShowVertScrollbar as setShowVertScrollbar 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function setShowHorzScrollbar -}
{#fun cegui_scrlitmlstbs_setShowHorzScrollbar as setShowHorzScrollbar 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function ensureItemIsVisibleVert -}
{#fun cegui_scrlitmlstbs_ensureItemIsVisibleVert as ensureItemIsVisibleVert 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function ensureItemIsVisibleHorz -}
{#fun cegui_scrlitmlstbs_ensureItemIsVisibleHorz as ensureItemIsVisibleHorz 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function initialiseComponents -}
{#fun cegui_scrlitmlstbs_initialiseComponents as initialiseComponents 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

