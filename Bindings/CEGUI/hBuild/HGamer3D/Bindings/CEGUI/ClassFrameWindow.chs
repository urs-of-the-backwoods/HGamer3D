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


-- ClassFrameWindow.chs

-- 

module HGamer3D.Bindings.CEGUI.ClassFrameWindow where

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

#include "ClassFrameWindow.h"
{- function initialiseComponents -}
{#fun cegui_frmwndw_initialiseComponents as initialiseComponents 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function isSizingEnabled -}
{#fun cegui_frmwndw_isSizingEnabled as isSizingEnabled 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isFrameEnabled -}
{#fun cegui_frmwndw_isFrameEnabled as isFrameEnabled 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isTitleBarEnabled -}
{#fun cegui_frmwndw_isTitleBarEnabled as isTitleBarEnabled 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isCloseButtonEnabled -}
{#fun cegui_frmwndw_isCloseButtonEnabled as isCloseButtonEnabled 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isRollupEnabled -}
{#fun cegui_frmwndw_isRollupEnabled as isRollupEnabled 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isRolledup -}
{#fun cegui_frmwndw_isRolledup as isRolledup 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function getSizingBorderThickness -}
{#fun cegui_frmwndw_getSizingBorderThickness as getSizingBorderThickness 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function setSizingEnabled -}
{#fun cegui_frmwndw_setSizingEnabled as setSizingEnabled 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function setFrameEnabled -}
{#fun cegui_frmwndw_setFrameEnabled as setFrameEnabled 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function setTitleBarEnabled -}
{#fun cegui_frmwndw_setTitleBarEnabled as setTitleBarEnabled 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function setCloseButtonEnabled -}
{#fun cegui_frmwndw_setCloseButtonEnabled as setCloseButtonEnabled 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function setRollupEnabled -}
{#fun cegui_frmwndw_setRollupEnabled as setRollupEnabled 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function toggleRollup -}
{#fun cegui_frmwndw_toggleRollup as toggleRollup 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function setSizingBorderThickness -}
{#fun cegui_frmwndw_setSizingBorderThickness as setSizingBorderThickness 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' } -> `()'  #}

{- function isDragMovingEnabled -}
{#fun cegui_frmwndw_isDragMovingEnabled as isDragMovingEnabled 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function setDragMovingEnabled -}
{#fun cegui_frmwndw_setDragMovingEnabled as setDragMovingEnabled 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function setNSSizingCursorImage2 -}
{#fun cegui_frmwndw_setNSSizingCursorImage2 as setNSSizingCursorImage2 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 withCString* `String' } -> `()'  #}

{- function setEWSizingCursorImage2 -}
{#fun cegui_frmwndw_setEWSizingCursorImage2 as setEWSizingCursorImage2 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 withCString* `String' } -> `()'  #}

{- function setNWSESizingCursorImage2 -}
{#fun cegui_frmwndw_setNWSESizingCursorImage2 as setNWSESizingCursorImage2 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 withCString* `String' } -> `()'  #}

{- function setNESWSizingCursorImage2 -}
{#fun cegui_frmwndw_setNESWSizingCursorImage2 as setNESWSizingCursorImage2 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 withCString* `String' } -> `()'  #}

{- function getCloseButton -}
{#fun cegui_frmwndw_getCloseButton as getCloseButton 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function FrameWindow -}
{#fun cegui_frmwndw_construct as new 
{ withCString* `String' ,
 withCString* `String' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function ~FrameWindow -}
{#fun cegui_frmwndw_destruct as delete 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

