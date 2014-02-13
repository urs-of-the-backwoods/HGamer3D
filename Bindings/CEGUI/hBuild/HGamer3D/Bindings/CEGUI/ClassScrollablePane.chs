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


-- ClassScrollablePane.chs

-- 

module HGamer3D.Bindings.CEGUI.ClassScrollablePane where

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

#include "ClassScrollablePane.h"
{- function ScrollablePane -}
{#fun cegui_scrlpn_construct as new 
{ withCString* `String' ,
 withCString* `String' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function ~ScrollablePane -}
{#fun cegui_scrlpn_destruct as delete 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function getContentPane -}
{#fun cegui_scrlpn_getContentPane as getContentPane 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function isVertScrollbarAlwaysShown -}
{#fun cegui_scrlpn_isVertScrollbarAlwaysShown as isVertScrollbarAlwaysShown 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function setShowVertScrollbar -}
{#fun cegui_scrlpn_setShowVertScrollbar as setShowVertScrollbar 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function isHorzScrollbarAlwaysShown -}
{#fun cegui_scrlpn_isHorzScrollbarAlwaysShown as isHorzScrollbarAlwaysShown 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function setShowHorzScrollbar -}
{#fun cegui_scrlpn_setShowHorzScrollbar as setShowHorzScrollbar 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function isContentPaneAutoSized -}
{#fun cegui_scrlpn_isContentPaneAutoSized as isContentPaneAutoSized 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function setContentPaneAutoSized -}
{#fun cegui_scrlpn_setContentPaneAutoSized as setContentPaneAutoSized 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function getHorizontalStepSize -}
{#fun cegui_scrlpn_getHorizontalStepSize as getHorizontalStepSize 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function setHorizontalStepSize -}
{#fun cegui_scrlpn_setHorizontalStepSize as setHorizontalStepSize 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' } -> `()'  #}

{- function getHorizontalOverlapSize -}
{#fun cegui_scrlpn_getHorizontalOverlapSize as getHorizontalOverlapSize 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function setHorizontalOverlapSize -}
{#fun cegui_scrlpn_setHorizontalOverlapSize as setHorizontalOverlapSize 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' } -> `()'  #}

{- function getHorizontalScrollPosition -}
{#fun cegui_scrlpn_getHorizontalScrollPosition as getHorizontalScrollPosition 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function setHorizontalScrollPosition -}
{#fun cegui_scrlpn_setHorizontalScrollPosition as setHorizontalScrollPosition 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' } -> `()'  #}

{- function getVerticalStepSize -}
{#fun cegui_scrlpn_getVerticalStepSize as getVerticalStepSize 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function setVerticalStepSize -}
{#fun cegui_scrlpn_setVerticalStepSize as setVerticalStepSize 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' } -> `()'  #}

{- function getVerticalOverlapSize -}
{#fun cegui_scrlpn_getVerticalOverlapSize as getVerticalOverlapSize 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function setVerticalOverlapSize -}
{#fun cegui_scrlpn_setVerticalOverlapSize as setVerticalOverlapSize 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' } -> `()'  #}

{- function getVerticalScrollPosition -}
{#fun cegui_scrlpn_getVerticalScrollPosition as getVerticalScrollPosition 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function setVerticalScrollPosition -}
{#fun cegui_scrlpn_setVerticalScrollPosition as setVerticalScrollPosition 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' } -> `()'  #}

{- function getVertScrollbar -}
{#fun cegui_scrlpn_getVertScrollbar as getVertScrollbar 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getHorzScrollbar -}
{#fun cegui_scrlpn_getHorzScrollbar as getHorzScrollbar 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function initialiseComponents -}
{#fun cegui_scrlpn_initialiseComponents as initialiseComponents 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function destroy -}
{#fun cegui_scrlpn_destroy as destroy 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

