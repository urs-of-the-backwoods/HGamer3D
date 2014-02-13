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


-- ClassJoystick.chs

-- 

module HGamer3D.Bindings.SFML.ClassJoystick where

import Foreign
import Foreign.Ptr
import Foreign.C

import HGamer3D.Data.HG3DClass
import HGamer3D.Data.Vector
import HGamer3D.Data.Colour
import HGamer3D.Data.Angle

{# import HGamer3D.Bindings.SFML.Utils #}
{# import HGamer3D.Bindings.SFML.ClassPtr #}
{# import HGamer3D.Bindings.SFML.StructHG3DClass #}
{# import HGamer3D.Bindings.SFML.EnumJoystickAxis #}

#include "ClassJoystick.h"
{- function isConnected -}
{#fun sfml_jst_isConnected as isConnected 
{ fromIntegral `Int' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function getButtonCount -}
{#fun sfml_jst_getButtonCount as getButtonCount 
{ fromIntegral `Int' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function hasAxis -}
{#fun sfml_jst_hasAxis as hasAxis 
{ fromIntegral `Int' ,
 cIntFromEnum `EnumJoystickAxis' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isButtonPressed -}
{#fun sfml_jst_isButtonPressed as isButtonPressed 
{ fromIntegral `Int' ,
 fromIntegral `Int' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function getAxisPosition -}
{#fun sfml_jst_getAxisPosition as getAxisPosition 
{ fromIntegral `Int' ,
 cIntFromEnum `EnumJoystickAxis' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function update -}
{#fun sfml_jst_update as update 
{ } -> `()'  #}

