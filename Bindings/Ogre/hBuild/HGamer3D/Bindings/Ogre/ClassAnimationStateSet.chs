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


-- ClassAnimationStateSet.chs

-- 

module HGamer3D.Bindings.Ogre.ClassAnimationStateSet where

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

#include "ClassAnimationStateSet.h"
{- function ~AnimationStateSet -}
{#fun ogre_ass_destruct as delete 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function createAnimationState -}
{#fun ogre_ass_createAnimationState as createAnimationState 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 realToFrac `Float' ,
 realToFrac `Float' ,
 realToFrac `Float' ,
 fromBool `Bool' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getAnimationState -}
{#fun ogre_ass_getAnimationState as getAnimationState 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function hasAnimationState -}
{#fun ogre_ass_hasAnimationState as hasAnimationState 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function removeAnimationState -}
{#fun ogre_ass_removeAnimationState as removeAnimationState 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' } -> `()'  #}

{- function removeAllAnimationStates -}
{#fun ogre_ass_removeAllAnimationStates as removeAllAnimationStates 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function copyMatchingState -}
{#fun ogre_ass_copyMatchingState as copyMatchingState 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function hasEnabledAnimationState -}
{#fun ogre_ass_hasEnabledAnimationState as hasEnabledAnimationState 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

