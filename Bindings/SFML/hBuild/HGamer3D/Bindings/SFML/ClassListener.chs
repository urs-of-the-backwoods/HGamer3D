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


-- ClassListener.chs

-- 

module HGamer3D.Bindings.SFML.ClassListener where

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
{# import HGamer3D.Bindings.SFML.StructVec3 #}

#include "ClassListener.h"
{- function setGlobalVolume -}
{#fun sfml_lst_setGlobalVolume as setGlobalVolume 
{ realToFrac `Float' } -> `()'  #}

{- function getGlobalVolume -}
{#fun sfml_lst_getGlobalVolume as getGlobalVolume 
{ alloca- `Float' peekFloatConv*} -> `()'  #}

{- function setPosition -}
{#fun sfml_lst_setPosition as setPosition 
{ realToFrac `Float' ,
 realToFrac `Float' ,
 realToFrac `Float' } -> `()'  #}

{- function getPosition -}
{#fun sfml_lst_getPosition as getPosition 
{ alloca- `Vec3' peekVec3*} -> `()'  #}

{- function setDirection -}
{#fun sfml_lst_setDirection as setDirection 
{ realToFrac `Float' ,
 realToFrac `Float' ,
 realToFrac `Float' } -> `()'  #}

{- function getDirection -}
{#fun sfml_lst_getDirection as getDirection 
{ alloca- `Vec3' peekVec3*} -> `()'  #}

