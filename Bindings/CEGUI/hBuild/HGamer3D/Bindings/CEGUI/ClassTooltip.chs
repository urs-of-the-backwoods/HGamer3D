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


-- ClassTooltip.chs

-- 

module HGamer3D.Bindings.CEGUI.ClassTooltip where

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

#include "ClassTooltip.h"
{- function Tooltip -}
{#fun cegui_tltp_construct as new 
{ withCString* `String' ,
 withCString* `String' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function ~Tooltip -}
{#fun cegui_tltp_destruct as delete 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function setTargetWindow -}
{#fun cegui_tltp_setTargetWindow as setTargetWindow 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function getTargetWindow -}
{#fun cegui_tltp_getTargetWindow as getTargetWindow 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function resetTimer -}
{#fun cegui_tltp_resetTimer as resetTimer 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function getHoverTime -}
{#fun cegui_tltp_getHoverTime as getHoverTime 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function setDisplayTime -}
{#fun cegui_tltp_setDisplayTime as setDisplayTime 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' } -> `()'  #}

{- function getFadeTime -}
{#fun cegui_tltp_getFadeTime as getFadeTime 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function setHoverTime -}
{#fun cegui_tltp_setHoverTime as setHoverTime 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' } -> `()'  #}

{- function getDisplayTime -}
{#fun cegui_tltp_getDisplayTime as getDisplayTime 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function setFadeTime -}
{#fun cegui_tltp_setFadeTime as setFadeTime 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' } -> `()'  #}

{- function positionSelf -}
{#fun cegui_tltp_positionSelf as positionSelf 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function sizeSelf -}
{#fun cegui_tltp_sizeSelf as sizeSelf 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

