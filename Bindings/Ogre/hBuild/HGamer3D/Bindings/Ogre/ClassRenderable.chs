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


-- ClassRenderable.chs

-- 

module HGamer3D.Bindings.Ogre.ClassRenderable where

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
{# import HGamer3D.Bindings.Ogre.StructSharedPtr #}

#include "ClassRenderable.h"
{- function ~Renderable -}
{#fun ogre_rndl_destruct as delete 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function getMaterial -}
{#fun ogre_rndl_getMaterial as getMaterial 
{ withHG3DClass* `HG3DClass' ,
 alloca- `SharedPtr' peekSharedPtr*} -> `()'  #}

{- function preRender -}
{#fun ogre_rndl_preRender as preRender 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function postRender -}
{#fun ogre_rndl_postRender as postRender 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function getNumWorldTransforms -}
{#fun ogre_rndl_getNumWorldTransforms as getNumWorldTransforms 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function setUseIdentityProjection -}
{#fun ogre_rndl_setUseIdentityProjection as setUseIdentityProjection 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function getUseIdentityProjection -}
{#fun ogre_rndl_getUseIdentityProjection as getUseIdentityProjection 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function setUseIdentityView -}
{#fun ogre_rndl_setUseIdentityView as setUseIdentityView 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function getUseIdentityView -}
{#fun ogre_rndl_getUseIdentityView as getUseIdentityView 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function getSquaredViewDepth -}
{#fun ogre_rndl_getSquaredViewDepth as getSquaredViewDepth 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function getCastsShadows -}
{#fun ogre_rndl_getCastsShadows as getCastsShadows 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function removeCustomParameter -}
{#fun ogre_rndl_removeCustomParameter as removeCustomParameter 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' } -> `()'  #}

{- function hasCustomParameter -}
{#fun ogre_rndl_hasCustomParameter as hasCustomParameter 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function setPolygonModeOverrideable -}
{#fun ogre_rndl_setPolygonModeOverrideable as setPolygonModeOverrideable 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function getPolygonModeOverrideable -}
{#fun ogre_rndl_getPolygonModeOverrideable as getPolygonModeOverrideable 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

