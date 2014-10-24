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


-- ClassBone.chs

-- 

module HGamer3D.Bindings.Ogre.ClassBone where

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
{# import HGamer3D.Bindings.Ogre.StructVec3 #}
{# import HGamer3D.Bindings.Ogre.StructQuaternion #}

#include "ClassBone.h"
{- function ~Bone -}
{#fun ogre_bn_destruct as delete 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function createChild -}
{#fun ogre_bn_createChild as createChild 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 withVec3* `Vec3' ,
 withQuaternion* `Quaternion' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getHandle -}
{#fun ogre_bn_getHandle as getHandle 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function setBindingPose -}
{#fun ogre_bn_setBindingPose as setBindingPose 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function reset -}
{#fun ogre_bn_reset as reset 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function setManuallyControlled -}
{#fun ogre_bn_setManuallyControlled as setManuallyControlled 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function isManuallyControlled -}
{#fun ogre_bn_isManuallyControlled as isManuallyControlled 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function needUpdate -}
{#fun ogre_bn_needUpdate as needUpdate 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

