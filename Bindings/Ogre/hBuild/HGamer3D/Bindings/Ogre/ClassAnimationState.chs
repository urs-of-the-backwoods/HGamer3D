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


-- ClassAnimationState.chs

-- 

module HGamer3D.Bindings.Ogre.ClassAnimationState where

import Foreign
import Foreign.Ptr
import Foreign.C

import HGamer3D.Data.HG3DClass
import HGamer3D.Data.Vector
import HGamer3D.Data.Colour
import HGamer3D.Data.Angle

{# import HGamer3D.Bindings.Ogre.Utils #}
{# import HGamer3D.Bindings.Ogre.ClassPtr #}
{# import HGamer3D.Bindings.Ogre.StructHG3DClass #}

#include "ClassAnimationState.h"
{- function AnimationState -}
{#fun ogre_anms_construct as new 
{ withCString* `String' ,
 withHG3DClass* `HG3DClass' ,
 realToFrac `Float' ,
 realToFrac `Float' ,
 realToFrac `Float' ,
 fromBool `Bool' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function ~AnimationState -}
{#fun ogre_anms_destruct as delete 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function getAnimationName -}
{#fun ogre_anms_getAnimationName as getAnimationName 
{ withHG3DClass* `HG3DClass' ,
 alloc64k- `String' peekCString*} -> `()'  #}

{- function getTimePosition -}
{#fun ogre_anms_getTimePosition as getTimePosition 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function setTimePosition -}
{#fun ogre_anms_setTimePosition as setTimePosition 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' } -> `()'  #}

{- function getLength -}
{#fun ogre_anms_getLength as getLength 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function setLength -}
{#fun ogre_anms_setLength as setLength 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' } -> `()'  #}

{- function getWeight -}
{#fun ogre_anms_getWeight as getWeight 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function setWeight -}
{#fun ogre_anms_setWeight as setWeight 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' } -> `()'  #}

{- function addTime -}
{#fun ogre_anms_addTime as addTime 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' } -> `()'  #}

{- function hasEnded -}
{#fun ogre_anms_hasEnded as hasEnded 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function getEnabled -}
{#fun ogre_anms_getEnabled as getEnabled 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function setEnabled -}
{#fun ogre_anms_setEnabled as setEnabled 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function setLoop -}
{#fun ogre_anms_setLoop as setLoop 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function getLoop -}
{#fun ogre_anms_getLoop as getLoop 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function copyStateFrom -}
{#fun ogre_anms_copyStateFrom as copyStateFrom 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function getParent -}
{#fun ogre_anms_getParent as getParent 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function createBlendMask -}
{#fun ogre_anms_createBlendMask as createBlendMask 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 realToFrac `Float' } -> `()'  #}

{- function destroyBlendMask -}
{#fun ogre_anms_destroyBlendMask as destroyBlendMask 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function hasBlendMask -}
{#fun ogre_anms_hasBlendMask as hasBlendMask 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function setBlendMaskEntry -}
{#fun ogre_anms_setBlendMaskEntry as setBlendMaskEntry 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 realToFrac `Float' } -> `()'  #}

{- function getBlendMaskEntry -}
{#fun ogre_anms_getBlendMaskEntry as getBlendMaskEntry 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

