{-# LANGUAGE EmptyDataDecls #-}
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


-- ClassPtr.chs

-- Class Ptr Utilities

module HGamer3D.Bindings.SFML.ClassPtr where

import Foreign
import Foreign.Ptr
import Foreign.C

import HGamer3D.Data.HG3DClass
import HGamer3D.Data.Vector
import HGamer3D.Data.Colour
import HGamer3D.Data.Angle

{# import HGamer3D.Bindings.SFML.Utils #}

#include "ClassPtr.h"
{- class ClassMouseHG3D -}
{#pointer *ClassMouseHG3D as ClassMouseHG3D#}
{- class ClassJoystick -}
{#pointer *ClassJoystick as ClassJoystick#}
{- class ClassKeyboard -}
{#pointer *ClassKeyboard as ClassKeyboard#}
{- class ClassListener -}
{#pointer *ClassListener as ClassListener#}
{- class ClassMouse -}
{#pointer *ClassMouse as ClassMouse#}
{- class ClassMusic -}
{#pointer *ClassMusic as ClassMusic#}
{- class ClassSound -}
{#pointer *ClassSound as ClassSound#}
{- class ClassSoundBuffer -}
{#pointer *ClassSoundBuffer as ClassSoundBuffer#}
{- class ClassSoundSource -}
{#pointer *ClassSoundSource as ClassSoundSource#}
{- class ClassSoundStream -}
{#pointer *ClassSoundStream as ClassSoundStream#}
