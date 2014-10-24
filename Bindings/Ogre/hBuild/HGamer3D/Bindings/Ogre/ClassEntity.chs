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


-- ClassEntity.chs

-- 

module HGamer3D.Bindings.Ogre.ClassEntity where

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
{# import HGamer3D.Bindings.Ogre.EnumEntityVertexDataBindChoice #}

#include "ClassEntity.h"
{- function ~Entity -}
{#fun ogre_ent_destruct as delete 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function getMesh -}
{#fun ogre_ent_getMesh as getMesh 
{ withHG3DClass* `HG3DClass' ,
 alloca- `SharedPtr' peekSharedPtr*} -> `()'  #}

{- function getNumSubEntities -}
{#fun ogre_ent_getNumSubEntities as getNumSubEntities 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function clone -}
{#fun ogre_ent_clone as clone 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function setMaterialName -}
{#fun ogre_ent_setMaterialName as setMaterialName 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 withCString* `String' } -> `()'  #}

{- function setMaterial -}
{#fun ogre_ent_setMaterial as setMaterial 
{ withHG3DClass* `HG3DClass' ,
 withSharedPtr* `SharedPtr' } -> `()'  #}

{- function getMovableType -}
{#fun ogre_ent_getMovableType as getMovableType 
{ withHG3DClass* `HG3DClass' ,
 alloc64k- `String' peekCString*} -> `()'  #}

{- function getAnimationState -}
{#fun ogre_ent_getAnimationState as getAnimationState 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function hasAnimationState -}
{#fun ogre_ent_hasAnimationState as hasAnimationState 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function getAllAnimationStates -}
{#fun ogre_ent_getAllAnimationStates as getAllAnimationStates 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function setDisplaySkeleton -}
{#fun ogre_ent_setDisplaySkeleton as setDisplaySkeleton 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function getDisplaySkeleton -}
{#fun ogre_ent_getDisplaySkeleton as getDisplaySkeleton 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function getManualLodLevel -}
{#fun ogre_ent_getManualLodLevel as getManualLodLevel 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getNumManualLodLevels -}
{#fun ogre_ent_getNumManualLodLevels as getNumManualLodLevels 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function setPolygonModeOverrideable -}
{#fun ogre_ent_setPolygonModeOverrideable as setPolygonModeOverrideable 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function detachObjectFromBone -}
{#fun ogre_ent_detachObjectFromBone as detachObjectFromBone 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function detachObjectFromBone2 -}
{#fun ogre_ent_detachObjectFromBone2 as detachObjectFromBone2 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function detachAllObjectsFromBone -}
{#fun ogre_ent_detachAllObjectsFromBone as detachAllObjectsFromBone 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function getBoundingRadius -}
{#fun ogre_ent_getBoundingRadius as getBoundingRadius 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function hasEdgeList -}
{#fun ogre_ent_hasEdgeList as hasEdgeList 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function hasSkeleton -}
{#fun ogre_ent_hasSkeleton as hasSkeleton 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isHardwareAnimationEnabled -}
{#fun ogre_ent_isHardwareAnimationEnabled as isHardwareAnimationEnabled 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function getSoftwareAnimationRequests -}
{#fun ogre_ent_getSoftwareAnimationRequests as getSoftwareAnimationRequests 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function getSoftwareAnimationNormalsRequests -}
{#fun ogre_ent_getSoftwareAnimationNormalsRequests as getSoftwareAnimationNormalsRequests 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function addSoftwareAnimationRequest -}
{#fun ogre_ent_addSoftwareAnimationRequest as addSoftwareAnimationRequest 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function removeSoftwareAnimationRequest -}
{#fun ogre_ent_removeSoftwareAnimationRequest as removeSoftwareAnimationRequest 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function shareSkeletonInstanceWith -}
{#fun ogre_ent_shareSkeletonInstanceWith as shareSkeletonInstanceWith 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function hasVertexAnimation -}
{#fun ogre_ent_hasVertexAnimation as hasVertexAnimation 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function stopSharingSkeletonInstance -}
{#fun ogre_ent_stopSharingSkeletonInstance as stopSharingSkeletonInstance 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function sharesSkeletonInstance -}
{#fun ogre_ent_sharesSkeletonInstance as sharesSkeletonInstance 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function refreshAvailableAnimationState -}
{#fun ogre_ent_refreshAvailableAnimationState as refreshAvailableAnimationState 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function chooseVertexDataForBinding -}
{#fun ogre_ent_chooseVertexDataForBinding as chooseVertexDataForBinding 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' ,
 alloca- `EnumEntityVertexDataBindChoice' peekEnumUtil*} -> `()'  #}

{- function isInitialised -}
{#fun ogre_ent_isInitialised as isInitialised 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function backgroundLoadingComplete -}
{#fun ogre_ent_backgroundLoadingComplete as backgroundLoadingComplete 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function setSkipAnimationStateUpdate -}
{#fun ogre_ent_setSkipAnimationStateUpdate as setSkipAnimationStateUpdate 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function getSkipAnimationStateUpdate -}
{#fun ogre_ent_getSkipAnimationStateUpdate as getSkipAnimationStateUpdate 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function setAlwaysUpdateMainSkeleton -}
{#fun ogre_ent_setAlwaysUpdateMainSkeleton as setAlwaysUpdateMainSkeleton 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function getAlwaysUpdateMainSkeleton -}
{#fun ogre_ent_getAlwaysUpdateMainSkeleton as getAlwaysUpdateMainSkeleton 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

