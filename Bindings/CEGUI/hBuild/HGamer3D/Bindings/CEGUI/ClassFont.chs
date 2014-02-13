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


-- ClassFont.chs

-- 

module HGamer3D.Bindings.CEGUI.ClassFont where

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

#include "ClassFont.h"
{- function ~Font -}
{#fun cegui_fnt_destruct as delete 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function getName -}
{#fun cegui_fnt_getName as getName 
{ withHG3DClass* `HG3DClass' ,
 alloc64k- `String' peekCString*} -> `()'  #}

{- function getTypeName -}
{#fun cegui_fnt_getTypeName as getTypeName 
{ withHG3DClass* `HG3DClass' ,
 alloc64k- `String' peekCString*} -> `()'  #}

{- function isCodepointAvailable -}
{#fun cegui_fnt_isCodepointAvailable as isCodepointAvailable 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function setAutoScaled -}
{#fun cegui_fnt_setAutoScaled as setAutoScaled 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function isAutoScaled -}
{#fun cegui_fnt_isAutoScaled as isAutoScaled 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function getLineSpacing -}
{#fun cegui_fnt_getLineSpacing as getLineSpacing 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function getFontHeight -}
{#fun cegui_fnt_getFontHeight as getFontHeight 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function getBaseline -}
{#fun cegui_fnt_getBaseline as getBaseline 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function getTextExtent -}
{#fun cegui_fnt_getTextExtent as getTextExtent 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 realToFrac `Float' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function getCharAtPixel -}
{#fun cegui_fnt_getCharAtPixel as getCharAtPixel 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 realToFrac `Float' ,
 realToFrac `Float' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function getCharAtPixel2 -}
{#fun cegui_fnt_getCharAtPixel2 as getCharAtPixel2 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 fromIntegral `Int' ,
 realToFrac `Float' ,
 realToFrac `Float' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function setDefaultResourceGroup -}
{#fun cegui_fnt_setDefaultResourceGroup as setDefaultResourceGroup 
{ withCString* `String' } -> `()'  #}

{- function getDefaultResourceGroup -}
{#fun cegui_fnt_getDefaultResourceGroup as getDefaultResourceGroup 
{ alloc64k- `String' peekCString*} -> `()'  #}

