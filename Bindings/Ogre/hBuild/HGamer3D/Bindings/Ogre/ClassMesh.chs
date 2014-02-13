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


-- ClassMesh.chs

-- 

module HGamer3D.Bindings.Ogre.ClassMesh where

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
{# import HGamer3D.Bindings.Ogre.EnumVertexAnimationType #}

#include "ClassMesh.h"
{- function ~Mesh -}
{#fun ogre_msh_destruct as delete 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function unnameSubMesh -}
{#fun ogre_msh_unnameSubMesh as unnameSubMesh 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' } -> `()'  #}

{- function getNumSubMeshes -}
{#fun ogre_msh_getNumSubMeshes as getNumSubMeshes 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function destroySubMesh -}
{#fun ogre_msh_destroySubMesh as destroySubMesh 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' } -> `()'  #}

{- function destroySubMesh2 -}
{#fun ogre_msh_destroySubMesh2 as destroySubMesh2 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' } -> `()'  #}

{- function clone -}
{#fun ogre_msh_clone as clone 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 withCString* `String' ,
 alloca- `SharedPtr' peekSharedPtr*} -> `()'  #}

{- function getBoundingSphereRadius -}
{#fun ogre_msh_getBoundingSphereRadius as getBoundingSphereRadius 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Float' peekFloatConv*} -> `()'  #}

{- function setSkeletonName -}
{#fun ogre_msh_setSkeletonName as setSkeletonName 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' } -> `()'  #}

{- function hasSkeleton -}
{#fun ogre_msh_hasSkeleton as hasSkeleton 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function hasVertexAnimation -}
{#fun ogre_msh_hasVertexAnimation as hasVertexAnimation 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function getSkeleton -}
{#fun ogre_msh_getSkeleton as getSkeleton 
{ withHG3DClass* `HG3DClass' ,
 alloca- `SharedPtr' peekSharedPtr*} -> `()'  #}

{- function getSkeletonName -}
{#fun ogre_msh_getSkeletonName as getSkeletonName 
{ withHG3DClass* `HG3DClass' ,
 alloc64k- `String' peekCString*} -> `()'  #}

{- function clearBoneAssignments -}
{#fun ogre_msh_clearBoneAssignments as clearBoneAssignments 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function createManualLodLevel -}
{#fun ogre_msh_createManualLodLevel as createManualLodLevel 
{ withHG3DClass* `HG3DClass' ,
 realToFrac `Float' ,
 withCString* `String' ,
 withCString* `String' } -> `()'  #}

{- function isLodManual -}
{#fun ogre_msh_isLodManual as isLodManual 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function removeLodLevels -}
{#fun ogre_msh_removeLodLevels as removeLodLevels 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function isVertexBufferShadowed -}
{#fun ogre_msh_isVertexBufferShadowed as isVertexBufferShadowed 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isIndexBufferShadowed -}
{#fun ogre_msh_isIndexBufferShadowed as isIndexBufferShadowed 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function buildEdgeList -}
{#fun ogre_msh_buildEdgeList as buildEdgeList 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function freeEdgeList -}
{#fun ogre_msh_freeEdgeList as freeEdgeList 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function prepareForShadowVolume -}
{#fun ogre_msh_prepareForShadowVolume as prepareForShadowVolume 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function isPreparedForShadowVolumes -}
{#fun ogre_msh_isPreparedForShadowVolumes as isPreparedForShadowVolumes 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function isEdgeListBuilt -}
{#fun ogre_msh_isEdgeListBuilt as isEdgeListBuilt 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function setAutoBuildEdgeLists -}
{#fun ogre_msh_setAutoBuildEdgeLists as setAutoBuildEdgeLists 
{ withHG3DClass* `HG3DClass' ,
 fromBool `Bool' } -> `()'  #}

{- function getAutoBuildEdgeLists -}
{#fun ogre_msh_getAutoBuildEdgeLists as getAutoBuildEdgeLists 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function getSharedVertexDataAnimationType -}
{#fun ogre_msh_getSharedVertexDataAnimationType as getSharedVertexDataAnimationType 
{ withHG3DClass* `HG3DClass' ,
 alloca- `EnumVertexAnimationType' peekEnumUtil*} -> `()'  #}

{- function getSharedVertexDataAnimationIncludesNormals -}
{#fun ogre_msh_getSharedVertexDataAnimationIncludesNormals as getSharedVertexDataAnimationIncludesNormals 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function createAnimation -}
{#fun ogre_msh_createAnimation as createAnimation 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 realToFrac `Float' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function getAnimation -}
{#fun ogre_msh_getAnimation as getAnimation 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function hasAnimation -}
{#fun ogre_msh_hasAnimation as hasAnimation 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' ,
 alloca- `Bool' peekBoolUtil*} -> `()'  #}

{- function removeAnimation -}
{#fun ogre_msh_removeAnimation as removeAnimation 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' } -> `()'  #}

{- function getNumAnimations -}
{#fun ogre_msh_getNumAnimations as getNumAnimations 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function getAnimation2 -}
{#fun ogre_msh_getAnimation2 as getAnimation2 
{ withHG3DClass* `HG3DClass' ,
 fromIntegral `Int' ,
 alloca- `HG3DClass' peek*} -> `()'  #}

{- function removeAllAnimations -}
{#fun ogre_msh_removeAllAnimations as removeAllAnimations 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function updateMaterialForAllSubMeshes -}
{#fun ogre_msh_updateMaterialForAllSubMeshes as updateMaterialForAllSubMeshes 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

{- function getPoseCount -}
{#fun ogre_msh_getPoseCount as getPoseCount 
{ withHG3DClass* `HG3DClass' ,
 alloca- `Int' peekIntConv*} -> `()'  #}

{- function removePose2 -}
{#fun ogre_msh_removePose2 as removePose2 
{ withHG3DClass* `HG3DClass' ,
 withCString* `String' } -> `()'  #}

{- function removeAllPoses -}
{#fun ogre_msh_removeAllPoses as removeAllPoses 
{ withHG3DClass* `HG3DClass' } -> `()'  #}

