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


-- ClassControllerManager.chs

-- 

module HGamer3D.Bindings.Ogre.ClassControllerManager where

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

#include "ClassControllerManager.h"
{- function ControllerManager -}
{#fun ogre_cmgr_construct as new 
{ alloca- `HG3DClass' peek*} -> `()'  #}

{- function ~ControllerManager -}
{#fun ogre_cmgr_destruct as delete 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function clearControllers -}
{#fun ogre_cmgr_clearControllers as clearControllers 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function updateAllControllers -}
{#fun ogre_cmgr_updateAllControllers as updateAllControllers 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function getTimeFactor -}
{#fun ogre_cmgr_getTimeFactor as getTimeFactor 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function setTimeFactor -}
{#fun ogre_cmgr_setTimeFactor as setTimeFactor 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' } -> `()'  #}

{- function getFrameDelay -}
{#fun ogre_cmgr_getFrameDelay as getFrameDelay 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function setFrameDelay -}
{#fun ogre_cmgr_setFrameDelay as setFrameDelay 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' } -> `()'  #}

{- function getElapsedTime -}
{#fun ogre_cmgr_getElapsedTime as getElapsedTime 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function setElapsedTime -}
{#fun ogre_cmgr_setElapsedTime as setElapsedTime 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' } -> `()'  #}

{- function getSingleton -}
{#fun ogre_cmgr_getSingleton as getSingleton 
{ alloca- `HG3DClass' peek*} -> `()'  #}

{- function getSingletonPtr -}
{#fun ogre_cmgr_getSingletonPtr as getSingletonPtr 
{ alloca- `HG3DClass' peek*} -> `()'  #}

