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


-- ClassHG3DWindowStaticFunctions.chs

-- 

module HGamer3D.Bindings.CEGUI.ClassHG3DWindowStaticFunctions where

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

#include "ClassHG3DWindowStaticFunctions.h"
{- function castWindowToPushButton -}
{#fun cegui_hg3dwsfs_castWindowToPushButton as castWindowToPushButton 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function castWindowToListbox -}
{#fun cegui_hg3dwsfs_castWindowToListbox as castWindowToListbox 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function castWindowToCombobox -}
{#fun cegui_hg3dwsfs_castWindowToCombobox as castWindowToCombobox 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function castWindowToRadioButton -}
{#fun cegui_hg3dwsfs_castWindowToRadioButton as castWindowToRadioButton 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function castWindowToEditbox -}
{#fun cegui_hg3dwsfs_castWindowToEditbox as castWindowToEditbox 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function castWindowToMultiLineEditbox -}
{#fun cegui_hg3dwsfs_castWindowToMultiLineEditbox as castWindowToMultiLineEditbox 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function castWindowToFrameWindow -}
{#fun cegui_hg3dwsfs_castWindowToFrameWindow as castWindowToFrameWindow 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function castWindowToProgressBar -}
{#fun cegui_hg3dwsfs_castWindowToProgressBar as castWindowToProgressBar 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function castWindowToSlider -}
{#fun cegui_hg3dwsfs_castWindowToSlider as castWindowToSlider 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function castWindowToSpinner -}
{#fun cegui_hg3dwsfs_castWindowToSpinner as castWindowToSpinner 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function castWindowToMultiColumnList -}
{#fun cegui_hg3dwsfs_castWindowToMultiColumnList as castWindowToMultiColumnList 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function udScale -}
{#fun cegui_hg3dwsfs_udScale as udScale 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function udOffset -}
{#fun cegui_hg3dwsfs_udOffset as udOffset 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function v2X -}
{#fun cegui_hg3dwsfs_v2X as v2X 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function v2Y -}
{#fun cegui_hg3dwsfs_v2Y as v2Y 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getWindowWidth -}
{#fun cegui_hg3dwsfs_getWindowWidth as getWindowWidth 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getWindowHeight -}
{#fun cegui_hg3dwsfs_getWindowHeight as getWindowHeight 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function setNewWindowSize -}
{#fun cegui_hg3dwsfs_setNewWindowSize as setNewWindowSize 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' ,
 realToFrac `Float' } -> `()'  #}

{- function getWindowMargin -}
{#fun cegui_hg3dwsfs_getWindowMargin as getWindowMargin 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function setWindowMargin -}
{#fun cegui_hg3dwsfs_setWindowMargin as setWindowMargin 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

