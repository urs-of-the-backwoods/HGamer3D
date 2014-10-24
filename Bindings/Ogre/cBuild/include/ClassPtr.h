// This source file is part of HGamer3D, a project to enable 3D game development 
// in Haskell. For the latest info, see http://www.hgamer3d.org .
// 
// (c) 2011-2014 Peter Althainz
// 
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
// 
//     http://www.apache.org/licenses/LICENSE-2.0
// 
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
// 

// ClassPtr.h

// Here are the methods defined, which do the class pointer
// marshalling and the casting of subclasses to higher classes

#include "wchar.h"


#ifndef CLASSPTR_INCLUDE_H
#define CLASSPTR_INCLUDE_H

typedef struct hg3dclass_struct {
	void *ptr;
	void *fptr;
} hg3dclass_struct;

void *getHG3DClassPtr(hg3dclass_struct inSt, const char* className);


typedef void ClassAnimation; 
hg3dclass_struct getHG3DClass_Animation(void *ptrIn);

typedef void ClassAnimationState; 
hg3dclass_struct getHG3DClass_AnimationState(void *ptrIn);

typedef void ClassAnimationStateSet; 
hg3dclass_struct getHG3DClass_AnimationStateSet(void *ptrIn);

typedef void ClassAnimationTrack; 
hg3dclass_struct getHG3DClass_AnimationTrack(void *ptrIn);

typedef void ClassArchive; 
hg3dclass_struct getHG3DClass_Archive(void *ptrIn);

typedef void ClassArchiveManager; 
hg3dclass_struct getHG3DClass_ArchiveManager(void *ptrIn);

typedef void ClassBillboard; 
hg3dclass_struct getHG3DClass_Billboard(void *ptrIn);

typedef void ClassBillboardChain; 
hg3dclass_struct getHG3DClass_BillboardChain(void *ptrIn);

typedef void ClassBillboardChainFactory; 
hg3dclass_struct getHG3DClass_BillboardChainFactory(void *ptrIn);

typedef void ClassBillboardSet; 
hg3dclass_struct getHG3DClass_BillboardSet(void *ptrIn);

typedef void ClassBillboardSetFactory; 
hg3dclass_struct getHG3DClass_BillboardSetFactory(void *ptrIn);

typedef void ClassBone; 
hg3dclass_struct getHG3DClass_Bone(void *ptrIn);

typedef void ClassCamera; 
hg3dclass_struct getHG3DClass_Camera(void *ptrIn);

typedef void ClassConfigFile; 
hg3dclass_struct getHG3DClass_ConfigFile(void *ptrIn);

typedef void ClassControllerManager; 
hg3dclass_struct getHG3DClass_ControllerManager(void *ptrIn);

typedef void ClassEntity; 
hg3dclass_struct getHG3DClass_Entity(void *ptrIn);

typedef void ClassEntityFactory; 
hg3dclass_struct getHG3DClass_EntityFactory(void *ptrIn);

typedef void ClassException; 
hg3dclass_struct getHG3DClass_Exception(void *ptrIn);

typedef void ClassFrustum; 
hg3dclass_struct getHG3DClass_Frustum(void *ptrIn);

typedef void ClassLight; 
hg3dclass_struct getHG3DClass_Light(void *ptrIn);

typedef void ClassLightFactory; 
hg3dclass_struct getHG3DClass_LightFactory(void *ptrIn);

typedef void ClassLog; 
hg3dclass_struct getHG3DClass_Log(void *ptrIn);

typedef void ClassLogManager; 
hg3dclass_struct getHG3DClass_LogManager(void *ptrIn);

typedef void ClassManualObject; 
hg3dclass_struct getHG3DClass_ManualObject(void *ptrIn);

typedef void ClassManualObjectFactory; 
hg3dclass_struct getHG3DClass_ManualObjectFactory(void *ptrIn);

typedef void ClassManualObjectSection; 
hg3dclass_struct getHG3DClass_ManualObjectSection(void *ptrIn);

typedef void ClassMaterial; 
hg3dclass_struct getHG3DClass_Material(void *ptrIn);

typedef void ClassMaterialManager; 
hg3dclass_struct getHG3DClass_MaterialManager(void *ptrIn);

typedef void ClassMesh; 
hg3dclass_struct getHG3DClass_Mesh(void *ptrIn);

typedef void ClassMeshManager; 
hg3dclass_struct getHG3DClass_MeshManager(void *ptrIn);

typedef void ClassMovableObject; 
hg3dclass_struct getHG3DClass_MovableObject(void *ptrIn);

typedef void ClassMovableObjectFactory; 
hg3dclass_struct getHG3DClass_MovableObjectFactory(void *ptrIn);

typedef void ClassMultiRenderTarget; 
hg3dclass_struct getHG3DClass_MultiRenderTarget(void *ptrIn);

typedef void ClassNode; 
hg3dclass_struct getHG3DClass_Node(void *ptrIn);

typedef void ClassNodeAnimationTrack; 
hg3dclass_struct getHG3DClass_NodeAnimationTrack(void *ptrIn);

typedef void ClassNumericAnimationTrack; 
hg3dclass_struct getHG3DClass_NumericAnimationTrack(void *ptrIn);

typedef void ClassRenderable; 
hg3dclass_struct getHG3DClass_Renderable(void *ptrIn);

typedef void ClassRenderSystem; 
hg3dclass_struct getHG3DClass_RenderSystem(void *ptrIn);

typedef void ClassRenderTarget; 
hg3dclass_struct getHG3DClass_RenderTarget(void *ptrIn);

typedef void ClassRenderTexture; 
hg3dclass_struct getHG3DClass_RenderTexture(void *ptrIn);

typedef void ClassRenderWindow; 
hg3dclass_struct getHG3DClass_RenderWindow(void *ptrIn);

typedef void ClassResource; 
hg3dclass_struct getHG3DClass_Resource(void *ptrIn);

typedef void ClassResourceGroupManager; 
hg3dclass_struct getHG3DClass_ResourceGroupManager(void *ptrIn);

typedef void ClassResourceManager; 
hg3dclass_struct getHG3DClass_ResourceManager(void *ptrIn);

typedef void ClassRoot; 
hg3dclass_struct getHG3DClass_Root(void *ptrIn);

typedef void ClassSceneManager; 
hg3dclass_struct getHG3DClass_SceneManager(void *ptrIn);

typedef void ClassSceneManagerFactory; 
hg3dclass_struct getHG3DClass_SceneManagerFactory(void *ptrIn);

typedef void ClassSceneNode; 
hg3dclass_struct getHG3DClass_SceneNode(void *ptrIn);

typedef void ClassSkeleton; 
hg3dclass_struct getHG3DClass_Skeleton(void *ptrIn);

typedef void ClassSkeletonManager; 
hg3dclass_struct getHG3DClass_SkeletonManager(void *ptrIn);

typedef void ClassTextureManager; 
hg3dclass_struct getHG3DClass_TextureManager(void *ptrIn);

typedef void ClassTimeIndex; 
hg3dclass_struct getHG3DClass_TimeIndex(void *ptrIn);

typedef void ClassVertexAnimationTrack; 
hg3dclass_struct getHG3DClass_VertexAnimationTrack(void *ptrIn);

typedef void ClassViewport; 
hg3dclass_struct getHG3DClass_Viewport(void *ptrIn);

typedef void ClassWindowEventUtilities; 
hg3dclass_struct getHG3DClass_WindowEventUtilities(void *ptrIn);

typedef void ClassHG3DUtilities; 
hg3dclass_struct getHG3DClass_HG3DUtilities(void *ptrIn);

#endif
