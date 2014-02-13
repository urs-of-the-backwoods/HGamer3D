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


-- ClassProgressBar.chs

-- 

module HGamer3D.Bindings.CEGUI.ClassProgressBar where

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

#include "ClassProgressBar.h"
{- function getProgress -}
{#fun cegui_prgbr_getProgress as getProgress 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function getStep -}
{#fun cegui_prgbr_getStep as getStep 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function setProgress -}
{#fun cegui_prgbr_setProgress as setProgress 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' } -> `()'  #}

{- function setStepSize -}
{#fun cegui_prgbr_setStepSize as setStepSize 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' } -> `()'  #}

{- function step -}
{#fun cegui_prgbr_step as step 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function adjustProgress -}
{#fun cegui_prgbr_adjustProgress as adjustProgress 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' } -> `()'  #}

{- function ProgressBar -}
{#fun cegui_prgbr_construct as new 
{ withCString* `String' ,
 withCString* `String' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function ~ProgressBar -}
{#fun cegui_prgbr_destruct as delete 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

