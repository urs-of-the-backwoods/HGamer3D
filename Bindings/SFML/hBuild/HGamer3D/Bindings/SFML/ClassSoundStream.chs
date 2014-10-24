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


-- ClassSoundStream.chs

-- 

module HGamer3D.Bindings.SFML.ClassSoundStream where

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

#include "ClassSoundStream.h"
{- function ~SoundStream -}
{#fun sfml_snst_destruct as delete 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function play -}
{#fun sfml_snst_play as play 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function pause -}
{#fun sfml_snst_pause as pause 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function stop -}
{#fun sfml_snst_stop as stop 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function getChannelCount -}
{#fun sfml_snst_getChannelCount as getChannelCount 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function getSampleRate -}
{#fun sfml_snst_getSampleRate as getSampleRate 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function setLoop -}
{#fun sfml_snst_setLoop as setLoop 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function getLoop -}
{#fun sfml_snst_getLoop as getLoop 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

