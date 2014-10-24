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


-- ClassImageset.chs

-- 

module HGamer3D.Bindings.CEGUI.ClassImageset where

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

#include "ClassImageset.h"
{- function Imageset2 -}
{#fun cegui_imgst_construct as new 
{ withCString* `String' ,
 withCString* `String' ,
 withCString* `String' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function ~Imageset -}
{#fun cegui_imgst_destruct as delete 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function getName -}
{#fun cegui_imgst_getName as getName 
{ withHG3DClass* `HG3DClass' ,
 alloc64k- `String' peekCString*} -> `()'  #}

{- function getImageCount -}
{#fun cegui_imgst_getImageCount as getImageCount 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function isImageDefined -}
{#fun cegui_imgst_isImageDefined as isImageDefined 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function undefineImage -}
{#fun cegui_imgst_undefineImage as undefineImage 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' } -> `()'  #}

{- function undefineAllImages -}
{#fun cegui_imgst_undefineAllImages as undefineAllImages 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function getImageWidth -}
{#fun cegui_imgst_getImageWidth as getImageWidth 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function getImageHeight -}
{#fun cegui_imgst_getImageHeight as getImageHeight 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function getImageOffsetX -}
{#fun cegui_imgst_getImageOffsetX as getImageOffsetX 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function getImageOffsetY -}
{#fun cegui_imgst_getImageOffsetY as getImageOffsetY 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function isAutoScaled -}
{#fun cegui_imgst_isAutoScaled as isAutoScaled 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function setAutoScalingEnabled -}
{#fun cegui_imgst_setAutoScalingEnabled as setAutoScalingEnabled 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function setDefaultResourceGroup -}
{#fun cegui_imgst_setDefaultResourceGroup as setDefaultResourceGroup 
{ withCString* `String' } -> `()'  #}

{- function getDefaultResourceGroup -}
{#fun cegui_imgst_getDefaultResourceGroup as getDefaultResourceGroup 
{ alloc64k- `String' peekCString*} -> `()'  #}

