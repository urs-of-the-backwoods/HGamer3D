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


-- ClassSoundSource.chs

-- 

module HGamer3D.Bindings.SFML.ClassSoundSource where

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

#include "ClassSoundSource.h"
{- function ~SoundSource -}
{#fun sfml_snsr_destruct as delete 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function setPitch -}
{#fun sfml_snsr_setPitch as setPitch 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' } -> `()'  #}

{- function setVolume -}
{#fun sfml_snsr_setVolume as setVolume 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' } -> `()'  #}

{- function setPosition -}
{#fun sfml_snsr_setPosition as setPosition 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' ,
 realToFrac `Float' ,
 realToFrac `Float' } -> `()'  #}

{- function setRelativeToListener -}
{#fun sfml_snsr_setRelativeToListener as setRelativeToListener 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function setMinDistance -}
{#fun sfml_snsr_setMinDistance as setMinDistance 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' } -> `()'  #}

{- function setAttenuation -}
{#fun sfml_snsr_setAttenuation as setAttenuation 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' } -> `()'  #}

{- function getPitch -}
{#fun sfml_snsr_getPitch as getPitch 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function getVolume -}
{#fun sfml_snsr_getVolume as getVolume 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function getPosition -}
{#fun sfml_snsr_getPosition as getPosition 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Vec3' peekVec3*} -> `()'  #}

{- function isRelativeToListener -}
{#fun sfml_snsr_isRelativeToListener as isRelativeToListener 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function getMinDistance -}
{#fun sfml_snsr_getMinDistance as getMinDistance 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function getAttenuation -}
{#fun sfml_snsr_getAttenuation as getAttenuation 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

