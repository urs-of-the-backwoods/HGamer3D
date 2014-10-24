{-# LANGUAGE EmptyDataDecls #-}
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


-- ClassPtr.chs

-- Class Ptr Utilities

module HGamer3D.Bindings.Ogre.ClassPtr where

import Foreign
import Foreign.Ptr
import Foreign.C

import HGamer3D.Data.HG3DClass
import HGamer3D.Data.Vector
import HGamer3D.Data.Colour
import HGamer3D.Data.Angle

{# import HGamer3D.Bindings.Ogre.Utils #}

#include "ClassPtr.h"
{- class ClassAnimation -}
{#pointer *ClassAnimation as ClassAnimation#}
{- class ClassAnimationState -}
{#pointer *ClassAnimationState as ClassAnimationState#}
{- class ClassAnimationStateSet -}
{#pointer *ClassAnimationStateSet as ClassAnimationStateSet#}
{- class ClassAnimationTrack -}
{#pointer *ClassAnimationTrack as ClassAnimationTrack#}
{- class ClassArchive -}
{#pointer *ClassArchive as ClassArchive#}
{- class ClassArchiveManager -}
{#pointer *ClassArchiveManager as ClassArchiveManager#}
{- class ClassBillboard -}
{#pointer *ClassBillboard as ClassBillboard#}
{- class ClassBillboardChain -}
{#pointer *ClassBillboardChain as ClassBillboardChain#}
{- class ClassBillboardChainFactory -}
{#pointer *ClassBillboardChainFactory as ClassBillboardChainFactory#}
{- class ClassBillboardSet -}
{#pointer *ClassBillboardSet as ClassBillboardSet#}
{- class ClassBillboardSetFactory -}
{#pointer *ClassBillboardSetFactory as ClassBillboardSetFactory#}
{- class ClassBone -}
{#pointer *ClassBone as ClassBone#}
{- class ClassCamera -}
{#pointer *ClassCamera as ClassCamera#}
{- class ClassConfigFile -}
{#pointer *ClassConfigFile as ClassConfigFile#}
{- class ClassControllerManager -}
{#pointer *ClassControllerManager as ClassControllerManager#}
{- class ClassEntity -}
{#pointer *ClassEntity as ClassEntity#}
{- class ClassEntityFactory -}
{#pointer *ClassEntityFactory as ClassEntityFactory#}
{- class ClassException -}
{#pointer *ClassException as ClassException#}
{- class ClassFrustum -}
{#pointer *ClassFrustum as ClassFrustum#}
{- class ClassLight -}
{#pointer *ClassLight as ClassLight#}
{- class ClassLightFactory -}
{#pointer *ClassLightFactory as ClassLightFactory#}
{- class ClassLog -}
{#pointer *ClassLog as ClassLog#}
{- class ClassLogManager -}
{#pointer *ClassLogManager as ClassLogManager#}
{- class ClassManualObject -}
{#pointer *ClassManualObject as ClassManualObject#}
{- class ClassManualObjectFactory -}
{#pointer *ClassManualObjectFactory as ClassManualObjectFactory#}
{- class ClassManualObjectSection -}
{#pointer *ClassManualObjectSection as ClassManualObjectSection#}
{- class ClassMaterial -}
{#pointer *ClassMaterial as ClassMaterial#}
{- class ClassMaterialManager -}
{#pointer *ClassMaterialManager as ClassMaterialManager#}
{- class ClassMesh -}
{#pointer *ClassMesh as ClassMesh#}
{- class ClassMeshManager -}
{#pointer *ClassMeshManager as ClassMeshManager#}
{- class ClassMovableObject -}
{#pointer *ClassMovableObject as ClassMovableObject#}
{- class ClassMovableObjectFactory -}
{#pointer *ClassMovableObjectFactory as ClassMovableObjectFactory#}
{- class ClassMultiRenderTarget -}
{#pointer *ClassMultiRenderTarget as ClassMultiRenderTarget#}
{- class ClassNode -}
{#pointer *ClassNode as ClassNode#}
{- class ClassNodeAnimationTrack -}
{#pointer *ClassNodeAnimationTrack as ClassNodeAnimationTrack#}
{- class ClassNumericAnimationTrack -}
{#pointer *ClassNumericAnimationTrack as ClassNumericAnimationTrack#}
{- class ClassRenderable -}
{#pointer *ClassRenderable as ClassRenderable#}
{- class ClassRenderSystem -}
{#pointer *ClassRenderSystem as ClassRenderSystem#}
{- class ClassRenderTarget -}
{#pointer *ClassRenderTarget as ClassRenderTarget#}
{- class ClassRenderTexture -}
{#pointer *ClassRenderTexture as ClassRenderTexture#}
{- class ClassRenderWindow -}
{#pointer *ClassRenderWindow as ClassRenderWindow#}
{- class ClassResource -}
{#pointer *ClassResource as ClassResource#}
{- class ClassResourceGroupManager -}
{#pointer *ClassResourceGroupManager as ClassResourceGroupManager#}
{- class ClassResourceManager -}
{#pointer *ClassResourceManager as ClassResourceManager#}
{- class ClassRoot -}
{#pointer *ClassRoot as ClassRoot#}
{- class ClassSceneManager -}
{#pointer *ClassSceneManager as ClassSceneManager#}
{- class ClassSceneManagerFactory -}
{#pointer *ClassSceneManagerFactory as ClassSceneManagerFactory#}
{- class ClassSceneNode -}
{#pointer *ClassSceneNode as ClassSceneNode#}
{- class ClassSkeleton -}
{#pointer *ClassSkeleton as ClassSkeleton#}
{- class ClassSkeletonManager -}
{#pointer *ClassSkeletonManager as ClassSkeletonManager#}
{- class ClassTextureManager -}
{#pointer *ClassTextureManager as ClassTextureManager#}
{- class ClassTimeIndex -}
{#pointer *ClassTimeIndex as ClassTimeIndex#}
{- class ClassVertexAnimationTrack -}
{#pointer *ClassVertexAnimationTrack as ClassVertexAnimationTrack#}
{- class ClassViewport -}
{#pointer *ClassViewport as ClassViewport#}
{- class ClassWindowEventUtilities -}
{#pointer *ClassWindowEventUtilities as ClassWindowEventUtilities#}
{- class ClassHG3DUtilities -}
{#pointer *ClassHG3DUtilities as ClassHG3DUtilities#}
