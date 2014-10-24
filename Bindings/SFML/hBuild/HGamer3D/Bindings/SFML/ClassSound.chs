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


-- ClassSound.chs

-- 

module HGamer3D.Bindings.SFML.ClassSound where

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

#include "ClassSound.h"
{- function Sound -}
{#fun sfml_snd_construct as new 
{ alloca- `HG3DClass' peek*} -> `()'  #}

{- function ~Sound -}
{#fun sfml_snd_destruct as delete 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function play -}
{#fun sfml_snd_play as play 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function pause -}
{#fun sfml_snd_pause as pause 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function stop -}
{#fun sfml_snd_stop as stop 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function setBuffer -}
{#fun sfml_snd_setBuffer as setBuffer 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function setLoop -}
{#fun sfml_snd_setLoop as setLoop 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function getLoop -}
{#fun sfml_snd_getLoop as getLoop 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function resetBuffer -}
{#fun sfml_snd_resetBuffer as resetBuffer 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

