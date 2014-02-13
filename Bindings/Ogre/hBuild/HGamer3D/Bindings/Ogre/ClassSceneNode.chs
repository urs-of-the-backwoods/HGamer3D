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


-- ClassSceneNode.chs

-- 

module HGamer3D.Bindings.Ogre.ClassSceneNode where

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

#include "ClassSceneNode.h"
{- function ~SceneNode -}
{#fun ogre_sn_destruct as delete 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function attachObject -}
{#fun ogre_sn_attachObject as attachObject 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function numAttachedObjects -}
{#fun ogre_sn_numAttachedObjects as numAttachedObjects 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function getAttachedObject -}
{#fun ogre_sn_getAttachedObject as getAttachedObject 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getAttachedObject2 -}
{#fun ogre_sn_getAttachedObject2 as getAttachedObject2 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function detachObject -}
{#fun ogre_sn_detachObject as detachObject 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function detachObject2 -}
{#fun ogre_sn_detachObject2 as detachObject2 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function detachObject3 -}
{#fun ogre_sn_detachObject3 as detachObject3 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function detachAllObjects -}
{#fun ogre_sn_detachAllObjects as detachAllObjects 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function isInSceneGraph -}
{#fun ogre_sn_isInSceneGraph as isInSceneGraph 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function getCreator -}
{#fun ogre_sn_getCreator as getCreator 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function removeAndDestroyChild -}
{#fun ogre_sn_removeAndDestroyChild as removeAndDestroyChild 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' } -> `()'  #}

{- function removeAndDestroyChild2 -}
{#fun ogre_sn_removeAndDestroyChild2 as removeAndDestroyChild2 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' } -> `()'  #}

{- function removeAndDestroyAllChildren -}
{#fun ogre_sn_removeAndDestroyAllChildren as removeAndDestroyAllChildren 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function showBoundingBox -}
{#fun ogre_sn_showBoundingBox as showBoundingBox 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function hideBoundingBox -}
{#fun ogre_sn_hideBoundingBox as hideBoundingBox 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function getShowBoundingBox -}
{#fun ogre_sn_getShowBoundingBox as getShowBoundingBox 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function createChildSceneNode -}
{#fun ogre_sn_createChildSceneNode as createChildSceneNode 
{ withHG3DClass* `HG3DClass' ,
 withVec3* `Vec3' ,
 withQuaternion* `Quaternion' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function createChildSceneNode2 -}
{#fun ogre_sn_createChildSceneNode2 as createChildSceneNode2 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 withVec3* `Vec3' ,
 withQuaternion* `Quaternion' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function setFixedYawAxis -}
{#fun ogre_sn_setFixedYawAxis as setFixedYawAxis 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' ,
 withVec3* `Vec3' } -> `()'  #}

{- function getAutoTrackTarget -}
{#fun ogre_sn_getAutoTrackTarget as getAutoTrackTarget 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getAutoTrackOffset -}
{#fun ogre_sn_getAutoTrackOffset as getAutoTrackOffset 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Vec3' peekVec3*} -> `()'  #}

{- function getAutoTrackLocalDirection -}
{#fun ogre_sn_getAutoTrackLocalDirection as getAutoTrackLocalDirection 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Vec3' peekVec3*} -> `()'  #}

{- function getParentSceneNode -}
{#fun ogre_sn_getParentSceneNode as getParentSceneNode 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function setVisible -}
{#fun ogre_sn_setVisible as setVisible 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' ,
 fromBool `Bool' } -> `()'  #}

{- function flipVisibility -}
{#fun ogre_sn_flipVisibility as flipVisibility 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function setDebugDisplayEnabled -}
{#fun ogre_sn_setDebugDisplayEnabled as setDebugDisplayEnabled 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' ,
 fromBool `Bool' } -> `()'  #}

