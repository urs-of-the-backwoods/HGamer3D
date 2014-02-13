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


-- ClassSceneManager.chs

-- 

module HGamer3D.Bindings.Ogre.ClassSceneManager where

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
{# import HGamer3D.Bindings.Ogre.EnumSceneManagerPrefabType #}
{# import HGamer3D.Bindings.Ogre.StructColour #}
{# import HGamer3D.Bindings.Ogre.StructQuaternion #}
{# import HGamer3D.Bindings.Ogre.EnumSceneManagerSpecialCaseRenderQueueMode #}
{# import HGamer3D.Bindings.Ogre.EnumLightType #}

#include "ClassSceneManager.h"
{- function ~SceneManager -}
{#fun ogre_scmgr_destruct as delete 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function getName -}
{#fun ogre_scmgr_getName as getName 
{ withHG3DClass* `HG3DClass' ,
 alloc64k- `String' peekCString*} -> `()'  #}

{- function getTypeName -}
{#fun ogre_scmgr_getTypeName as getTypeName 
{ withHG3DClass* `HG3DClass' ,
 alloc64k- `String' peekCString*} -> `()'  #}

{- function createCamera -}
{#fun ogre_scmgr_createCamera as createCamera 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getCamera -}
{#fun ogre_scmgr_getCamera as getCamera 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function hasCamera -}
{#fun ogre_scmgr_hasCamera as hasCamera 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function destroyCamera -}
{#fun ogre_scmgr_destroyCamera as destroyCamera 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function destroyCamera2 -}
{#fun ogre_scmgr_destroyCamera2 as destroyCamera2 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' } -> `()'  #}

{- function destroyAllCameras -}
{#fun ogre_scmgr_destroyAllCameras as destroyAllCameras 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function createLight -}
{#fun ogre_scmgr_createLight as createLight 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function createLight2 -}
{#fun ogre_scmgr_createLight2 as createLight2 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getLight -}
{#fun ogre_scmgr_getLight as getLight 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function hasLight -}
{#fun ogre_scmgr_hasLight as hasLight 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function destroyLight -}
{#fun ogre_scmgr_destroyLight as destroyLight 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' } -> `()'  #}

{- function destroyLight2 -}
{#fun ogre_scmgr_destroyLight2 as destroyLight2 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function destroyAllLights -}
{#fun ogre_scmgr_destroyAllLights as destroyAllLights 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function createSceneNode -}
{#fun ogre_scmgr_createSceneNode as createSceneNode 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function createSceneNode2 -}
{#fun ogre_scmgr_createSceneNode2 as createSceneNode2 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function destroySceneNode -}
{#fun ogre_scmgr_destroySceneNode as destroySceneNode 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' } -> `()'  #}

{- function destroySceneNode2 -}
{#fun ogre_scmgr_destroySceneNode2 as destroySceneNode2 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function getRootSceneNode -}
{#fun ogre_scmgr_getRootSceneNode as getRootSceneNode 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getSceneNode -}
{#fun ogre_scmgr_getSceneNode as getSceneNode 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function hasSceneNode -}
{#fun ogre_scmgr_hasSceneNode as hasSceneNode 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function createEntity -}
{#fun ogre_scmgr_createEntity as createEntity 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 withCString* `String' ,
 withCString* `String' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function createEntity2 -}
{#fun ogre_scmgr_createEntity2 as createEntity2 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 withSharedPtr* `SharedPtr' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function createEntity3 -}
{#fun ogre_scmgr_createEntity3 as createEntity3 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function createEntity4 -}
{#fun ogre_scmgr_createEntity4 as createEntity4 
{ withHG3DClass* `HG3DClass' ,
 withSharedPtr* `SharedPtr' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function createEntity5 -}
{#fun ogre_scmgr_createEntity5 as createEntity5 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 cIntFromEnum `EnumSceneManagerPrefabType' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function createEntity6 -}
{#fun ogre_scmgr_createEntity6 as createEntity6 
{ withHG3DClass* `HG3DClass' ,
 cIntFromEnum `EnumSceneManagerPrefabType' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getEntity -}
{#fun ogre_scmgr_getEntity as getEntity 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function hasEntity -}
{#fun ogre_scmgr_hasEntity as hasEntity 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function destroyEntity -}
{#fun ogre_scmgr_destroyEntity as destroyEntity 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function destroyEntity2 -}
{#fun ogre_scmgr_destroyEntity2 as destroyEntity2 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' } -> `()'  #}

{- function destroyAllEntities -}
{#fun ogre_scmgr_destroyAllEntities as destroyAllEntities 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function createManualObject -}
{#fun ogre_scmgr_createManualObject as createManualObject 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function createManualObject2 -}
{#fun ogre_scmgr_createManualObject2 as createManualObject2 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getManualObject -}
{#fun ogre_scmgr_getManualObject as getManualObject 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function hasManualObject -}
{#fun ogre_scmgr_hasManualObject as hasManualObject 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function destroyManualObject -}
{#fun ogre_scmgr_destroyManualObject as destroyManualObject 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function destroyManualObject2 -}
{#fun ogre_scmgr_destroyManualObject2 as destroyManualObject2 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' } -> `()'  #}

{- function destroyAllManualObjects -}
{#fun ogre_scmgr_destroyAllManualObjects as destroyAllManualObjects 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function createBillboardChain -}
{#fun ogre_scmgr_createBillboardChain as createBillboardChain 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function createBillboardChain2 -}
{#fun ogre_scmgr_createBillboardChain2 as createBillboardChain2 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getBillboardChain -}
{#fun ogre_scmgr_getBillboardChain as getBillboardChain 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function hasBillboardChain -}
{#fun ogre_scmgr_hasBillboardChain as hasBillboardChain 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function destroyBillboardChain -}
{#fun ogre_scmgr_destroyBillboardChain as destroyBillboardChain 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function destroyBillboardChain2 -}
{#fun ogre_scmgr_destroyBillboardChain2 as destroyBillboardChain2 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' } -> `()'  #}

{- function destroyAllBillboardChains -}
{#fun ogre_scmgr_destroyAllBillboardChains as destroyAllBillboardChains 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function hasRibbonTrail -}
{#fun ogre_scmgr_hasRibbonTrail as hasRibbonTrail 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function destroyRibbonTrail2 -}
{#fun ogre_scmgr_destroyRibbonTrail2 as destroyRibbonTrail2 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' } -> `()'  #}

{- function destroyAllRibbonTrails -}
{#fun ogre_scmgr_destroyAllRibbonTrails as destroyAllRibbonTrails 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function hasParticleSystem -}
{#fun ogre_scmgr_hasParticleSystem as hasParticleSystem 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function destroyParticleSystem2 -}
{#fun ogre_scmgr_destroyParticleSystem2 as destroyParticleSystem2 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' } -> `()'  #}

{- function destroyAllParticleSystems -}
{#fun ogre_scmgr_destroyAllParticleSystems as destroyAllParticleSystems 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function clearScene -}
{#fun ogre_scmgr_clearScene as clearScene 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function setAmbientLight -}
{#fun ogre_scmgr_setAmbientLight as setAmbientLight 
{ withHG3DClass* `HG3DClass' ,
 withColour* `Colour' } -> `()'  #}

{- function getAmbientLight -}
{#fun ogre_scmgr_getAmbientLight as getAmbientLight 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Colour' peekColour*} -> `()'  #}

{- function prepareWorldGeometry -}
{#fun ogre_scmgr_prepareWorldGeometry as prepareWorldGeometry 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' } -> `()'  #}

{- function setWorldGeometry -}
{#fun ogre_scmgr_setWorldGeometry as setWorldGeometry 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' } -> `()'  #}

{- function estimateWorldGeometry -}
{#fun ogre_scmgr_estimateWorldGeometry as estimateWorldGeometry 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function hasOption -}
{#fun ogre_scmgr_hasOption as hasOption 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function setSkyPlaneEnabled -}
{#fun ogre_scmgr_setSkyPlaneEnabled as setSkyPlaneEnabled 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function isSkyPlaneEnabled -}
{#fun ogre_scmgr_isSkyPlaneEnabled as isSkyPlaneEnabled 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function getSkyPlaneNode -}
{#fun ogre_scmgr_getSkyPlaneNode as getSkyPlaneNode 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function setSkyBox -}
{#fun ogre_scmgr_setSkyBox as setSkyBox 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' ,
 withCString* `String' ,
 realToFrac `Float' ,
 fromBool `Bool' ,
 withQuaternion* `Quaternion' ,
 withCString* `String' } -> `()'  #}

{- function setSkyBoxEnabled -}
{#fun ogre_scmgr_setSkyBoxEnabled as setSkyBoxEnabled 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function isSkyBoxEnabled -}
{#fun ogre_scmgr_isSkyBoxEnabled as isSkyBoxEnabled 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function getSkyBoxNode -}
{#fun ogre_scmgr_getSkyBoxNode as getSkyBoxNode 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function setSkyDome -}
{#fun ogre_scmgr_setSkyDome as setSkyDome 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' ,
 withCString* `String' ,
 realToFrac `Float' ,
 realToFrac `Float' ,
 realToFrac `Float' ,
 fromBool `Bool' ,
 withQuaternion* `Quaternion' ,
 fromIntegral `Int' ,
 fromIntegral `Int' ,
 fromIntegral `Int' ,
 withCString* `String' } -> `()'  #}

{- function setSkyDomeEnabled -}
{#fun ogre_scmgr_setSkyDomeEnabled as setSkyDomeEnabled 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function isSkyDomeEnabled -}
{#fun ogre_scmgr_isSkyDomeEnabled as isSkyDomeEnabled 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function getSkyDomeNode -}
{#fun ogre_scmgr_getSkyDomeNode as getSkyDomeNode 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getFogColour -}
{#fun ogre_scmgr_getFogColour as getFogColour 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Colour' peekColour*} -> `()'  #}

{- function getFogStart -}
{#fun ogre_scmgr_getFogStart as getFogStart 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function getFogEnd -}
{#fun ogre_scmgr_getFogEnd as getFogEnd 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function getFogDensity -}
{#fun ogre_scmgr_getFogDensity as getFogDensity 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function createBillboardSet -}
{#fun ogre_scmgr_createBillboardSet as createBillboardSet 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 fromIntegral `Int' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function createBillboardSet2 -}
{#fun ogre_scmgr_createBillboardSet2 as createBillboardSet2 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getBillboardSet -}
{#fun ogre_scmgr_getBillboardSet as getBillboardSet 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function hasBillboardSet -}
{#fun ogre_scmgr_hasBillboardSet as hasBillboardSet 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function destroyBillboardSet -}
{#fun ogre_scmgr_destroyBillboardSet as destroyBillboardSet 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function destroyBillboardSet2 -}
{#fun ogre_scmgr_destroyBillboardSet2 as destroyBillboardSet2 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' } -> `()'  #}

{- function destroyAllBillboardSets -}
{#fun ogre_scmgr_destroyAllBillboardSets as destroyAllBillboardSets 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function setDisplaySceneNodes -}
{#fun ogre_scmgr_setDisplaySceneNodes as setDisplaySceneNodes 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function getDisplaySceneNodes -}
{#fun ogre_scmgr_getDisplaySceneNodes as getDisplaySceneNodes 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function createAnimation -}
{#fun ogre_scmgr_createAnimation as createAnimation 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 realToFrac `Float' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getAnimation -}
{#fun ogre_scmgr_getAnimation as getAnimation 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function hasAnimation -}
{#fun ogre_scmgr_hasAnimation as hasAnimation 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function destroyAnimation -}
{#fun ogre_scmgr_destroyAnimation as destroyAnimation 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' } -> `()'  #}

{- function destroyAllAnimations -}
{#fun ogre_scmgr_destroyAllAnimations as destroyAllAnimations 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function createAnimationState -}
{#fun ogre_scmgr_createAnimationState as createAnimationState 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getAnimationState -}
{#fun ogre_scmgr_getAnimationState as getAnimationState 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function hasAnimationState -}
{#fun ogre_scmgr_hasAnimationState as hasAnimationState 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function destroyAnimationState -}
{#fun ogre_scmgr_destroyAnimationState as destroyAnimationState 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' } -> `()'  #}

{- function destroyAllAnimationStates -}
{#fun ogre_scmgr_destroyAllAnimationStates as destroyAllAnimationStates 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function clearSpecialCaseRenderQueues -}
{#fun ogre_scmgr_clearSpecialCaseRenderQueues as clearSpecialCaseRenderQueues 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function setSpecialCaseRenderQueueMode -}
{#fun ogre_scmgr_setSpecialCaseRenderQueueMode as setSpecialCaseRenderQueueMode 
{ withHG3DClass* `HG3DClass' ,
 cIntFromEnum `EnumSceneManagerSpecialCaseRenderQueueMode' } -> `()'  #}

{- function getSpecialCaseRenderQueueMode -}
{#fun ogre_scmgr_getSpecialCaseRenderQueueMode as getSpecialCaseRenderQueueMode 
{ withHG3DClass* `HG3DClass' ,
 alloca- `EnumSceneManagerSpecialCaseRenderQueueMode' peekEnumUtil*} -> `()'  #}

{- function showBoundingBoxes -}
{#fun ogre_scmgr_showBoundingBoxes as showBoundingBoxes 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function getShowBoundingBoxes -}
{#fun ogre_scmgr_getShowBoundingBoxes as getShowBoundingBoxes 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function setShowDebugShadows -}
{#fun ogre_scmgr_setShowDebugShadows as setShowDebugShadows 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function getShowDebugShadows -}
{#fun ogre_scmgr_getShowDebugShadows as getShowDebugShadows 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function setShadowColour -}
{#fun ogre_scmgr_setShadowColour as setShadowColour 
{ withHG3DClass* `HG3DClass' ,
 withColour* `Colour' } -> `()'  #}

{- function getShadowColour -}
{#fun ogre_scmgr_getShadowColour as getShadowColour 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Colour' peekColour*} -> `()'  #}

{- function setShadowDirectionalLightExtrusionDistance -}
{#fun ogre_scmgr_setShadowDirectionalLightExtrusionDistance as setShadowDirectionalLightExtrusionDistance 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' } -> `()'  #}

{- function getShadowDirectionalLightExtrusionDistance -}
{#fun ogre_scmgr_getShadowDirectionalLightExtrusionDistance as getShadowDirectionalLightExtrusionDistance 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function setShadowFarDistance -}
{#fun ogre_scmgr_setShadowFarDistance as setShadowFarDistance 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' } -> `()'  #}

{- function getShadowFarDistance -}
{#fun ogre_scmgr_getShadowFarDistance as getShadowFarDistance 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function getShadowFarDistanceSquared -}
{#fun ogre_scmgr_getShadowFarDistanceSquared as getShadowFarDistanceSquared 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function setShadowIndexBufferSize -}
{#fun ogre_scmgr_setShadowIndexBufferSize as setShadowIndexBufferSize 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' } -> `()'  #}

{- function getShadowIndexBufferSize -}
{#fun ogre_scmgr_getShadowIndexBufferSize as getShadowIndexBufferSize 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function setShadowTextureSize -}
{#fun ogre_scmgr_setShadowTextureSize as setShadowTextureSize 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' } -> `()'  #}

{- function setShadowTextureFSAA -}
{#fun ogre_scmgr_setShadowTextureFSAA as setShadowTextureFSAA 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' } -> `()'  #}

{- function setShadowTextureCount -}
{#fun ogre_scmgr_setShadowTextureCount as setShadowTextureCount 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' } -> `()'  #}

{- function getShadowTextureCount -}
{#fun ogre_scmgr_getShadowTextureCount as getShadowTextureCount 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function setShadowTextureCountPerLightType -}
{#fun ogre_scmgr_setShadowTextureCountPerLightType as setShadowTextureCountPerLightType 
{ withHG3DClass* `HG3DClass' ,
 cIntFromEnum `EnumLightType' ,
 fromIntegral `Int' } -> `()'  #}

{- function getShadowTextureCountPerLightType -}
{#fun ogre_scmgr_getShadowTextureCountPerLightType as getShadowTextureCountPerLightType 
{ withHG3DClass* `HG3DClass' ,
 cIntFromEnum `EnumLightType' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function getShadowTexture -}
{#fun ogre_scmgr_getShadowTexture as getShadowTexture 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 alloca- `SharedPtr' peekSharedPtr*} -> `()'  #}

{- function setShadowDirLightTextureOffset -}
{#fun ogre_scmgr_setShadowDirLightTextureOffset as setShadowDirLightTextureOffset 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' } -> `()'  #}

{- function getShadowDirLightTextureOffset -}
{#fun ogre_scmgr_getShadowDirLightTextureOffset as getShadowDirLightTextureOffset 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function setShadowTextureFadeStart -}
{#fun ogre_scmgr_setShadowTextureFadeStart as setShadowTextureFadeStart 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' } -> `()'  #}

{- function setShadowTextureFadeEnd -}
{#fun ogre_scmgr_setShadowTextureFadeEnd as setShadowTextureFadeEnd 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' } -> `()'  #}

{- function setShadowTextureSelfShadow -}
{#fun ogre_scmgr_setShadowTextureSelfShadow as setShadowTextureSelfShadow 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function getShadowTextureSelfShadow -}
{#fun ogre_scmgr_getShadowTextureSelfShadow as getShadowTextureSelfShadow 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function setShadowTextureCasterMaterial -}
{#fun ogre_scmgr_setShadowTextureCasterMaterial as setShadowTextureCasterMaterial 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' } -> `()'  #}

{- function setShadowTextureReceiverMaterial -}
{#fun ogre_scmgr_setShadowTextureReceiverMaterial as setShadowTextureReceiverMaterial 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' } -> `()'  #}

{- function setShadowCasterRenderBackFaces -}
{#fun ogre_scmgr_setShadowCasterRenderBackFaces as setShadowCasterRenderBackFaces 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function getShadowCasterRenderBackFaces -}
{#fun ogre_scmgr_getShadowCasterRenderBackFaces as getShadowCasterRenderBackFaces 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function setShadowUseInfiniteFarPlane -}
{#fun ogre_scmgr_setShadowUseInfiniteFarPlane as setShadowUseInfiniteFarPlane 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function isShadowTechniqueStencilBased -}
{#fun ogre_scmgr_isShadowTechniqueStencilBased as isShadowTechniqueStencilBased 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isShadowTechniqueTextureBased -}
{#fun ogre_scmgr_isShadowTechniqueTextureBased as isShadowTechniqueTextureBased 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isShadowTechniqueModulative -}
{#fun ogre_scmgr_isShadowTechniqueModulative as isShadowTechniqueModulative 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isShadowTechniqueAdditive -}
{#fun ogre_scmgr_isShadowTechniqueAdditive as isShadowTechniqueAdditive 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isShadowTechniqueIntegrated -}
{#fun ogre_scmgr_isShadowTechniqueIntegrated as isShadowTechniqueIntegrated 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isShadowTechniqueInUse -}
{#fun ogre_scmgr_isShadowTechniqueInUse as isShadowTechniqueInUse 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function setShadowUseLightClipPlanes -}
{#fun ogre_scmgr_setShadowUseLightClipPlanes as setShadowUseLightClipPlanes 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function getShadowUseLightClipPlanes -}
{#fun ogre_scmgr_getShadowUseLightClipPlanes as getShadowUseLightClipPlanes 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function setLateMaterialResolving -}
{#fun ogre_scmgr_setLateMaterialResolving as setLateMaterialResolving 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function isLateMaterialResolving -}
{#fun ogre_scmgr_isLateMaterialResolving as isLateMaterialResolving 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function hasStaticGeometry -}
{#fun ogre_scmgr_hasStaticGeometry as hasStaticGeometry 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function destroyStaticGeometry2 -}
{#fun ogre_scmgr_destroyStaticGeometry2 as destroyStaticGeometry2 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' } -> `()'  #}

{- function destroyAllStaticGeometry -}
{#fun ogre_scmgr_destroyAllStaticGeometry as destroyAllStaticGeometry 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function destroyInstancedGeometry2 -}
{#fun ogre_scmgr_destroyInstancedGeometry2 as destroyInstancedGeometry2 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' } -> `()'  #}

{- function destroyAllInstancedGeometry -}
{#fun ogre_scmgr_destroyAllInstancedGeometry as destroyAllInstancedGeometry 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function hasInstanceManager -}
{#fun ogre_scmgr_hasInstanceManager as hasInstanceManager 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function destroyInstanceManager -}
{#fun ogre_scmgr_destroyInstanceManager as destroyInstanceManager 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' } -> `()'  #}

{- function destroyAllInstanceManagers -}
{#fun ogre_scmgr_destroyAllInstanceManagers as destroyAllInstanceManagers 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function destroyMovableObject -}
{#fun ogre_scmgr_destroyMovableObject as destroyMovableObject 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 withCString* `String' } -> `()'  #}

{- function destroyMovableObject2 -}
{#fun ogre_scmgr_destroyMovableObject2 as destroyMovableObject2 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function destroyAllMovableObjectsByType -}
{#fun ogre_scmgr_destroyAllMovableObjectsByType as destroyAllMovableObjectsByType 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' } -> `()'  #}

{- function destroyAllMovableObjects -}
{#fun ogre_scmgr_destroyAllMovableObjects as destroyAllMovableObjects 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function getMovableObject -}
{#fun ogre_scmgr_getMovableObject as getMovableObject 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 withCString* `String' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function hasMovableObject -}
{#fun ogre_scmgr_hasMovableObject as hasMovableObject 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 withCString* `String' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function injectMovableObject -}
{#fun ogre_scmgr_injectMovableObject as injectMovableObject 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function extractMovableObject -}
{#fun ogre_scmgr_extractMovableObject as extractMovableObject 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 withCString* `String' } -> `()'  #}

{- function extractMovableObject2 -}
{#fun ogre_scmgr_extractMovableObject2 as extractMovableObject2 
{ withHG3DClass* `HG3DClass' ,
 withHG3DClass* `HG3DClass' } -> `()'  #}

{- function extractAllMovableObjectsByType -}
{#fun ogre_scmgr_extractAllMovableObjectsByType as extractAllMovableObjectsByType 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' } -> `()'  #}

{- function setVisibilityMask -}
{#fun ogre_scmgr_setVisibilityMask as setVisibilityMask 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' } -> `()'  #}

{- function getVisibilityMask -}
{#fun ogre_scmgr_getVisibilityMask as getVisibilityMask 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function setFindVisibleObjects -}
{#fun ogre_scmgr_setFindVisibleObjects as setFindVisibleObjects 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function getFindVisibleObjects -}
{#fun ogre_scmgr_getFindVisibleObjects as getFindVisibleObjects 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function setNormaliseNormalsOnScale -}
{#fun ogre_scmgr_setNormaliseNormalsOnScale as setNormaliseNormalsOnScale 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function getNormaliseNormalsOnScale -}
{#fun ogre_scmgr_getNormaliseNormalsOnScale as getNormaliseNormalsOnScale 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function setFlipCullingOnNegativeScale -}
{#fun ogre_scmgr_setFlipCullingOnNegativeScale as setFlipCullingOnNegativeScale 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function getFlipCullingOnNegativeScale -}
{#fun ogre_scmgr_getFlipCullingOnNegativeScale as getFlipCullingOnNegativeScale 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function getDestinationRenderSystem -}
{#fun ogre_scmgr_getDestinationRenderSystem as getDestinationRenderSystem 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getCurrentViewport -}
{#fun ogre_scmgr_getCurrentViewport as getCurrentViewport 
{ withHG3DClass* `HG3DClass' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function setCameraRelativeRendering -}
{#fun ogre_scmgr_setCameraRelativeRendering as setCameraRelativeRendering 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function getCameraRelativeRendering -}
{#fun ogre_scmgr_getCameraRelativeRendering as getCameraRelativeRendering 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

