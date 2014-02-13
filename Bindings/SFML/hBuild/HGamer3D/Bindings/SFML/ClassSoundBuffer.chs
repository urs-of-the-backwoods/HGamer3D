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


-- ClassSoundBuffer.chs

-- 

module HGamer3D.Bindings.SFML.ClassSoundBuffer where

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

#include "ClassSoundBuffer.h"
{- function SoundBuffer -}
{#fun sfml_snbf_construct as new 
{ alloca- `HG3DClass' peek*} -> `()'  #}

{- function ~SoundBuffer -}
{#fun sfml_snbf_destruct as delete 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function loadFromFile -}
{#fun sfml_snbf_loadFromFile as loadFromFile 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function saveToFile -}
{#fun sfml_snbf_saveToFile as saveToFile 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function getSampleRate -}
{#fun sfml_snbf_getSampleRate as getSampleRate 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function getChannelCount -}
{#fun sfml_snbf_getChannelCount as getChannelCount 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

