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

// ClassPtr.cpp

// Here are the methods defined, which do the class pointer
// marshalling and the casting of subclasses to higher classes

#include <wchar.h>
#include <string>
#include <iostream>

#include <iostream>
#include <typeinfo>
#include <stdio.h>
#include <cstring>
#include <exception>
#include <OgreDllDefines.h>
#include "./Ogre.h"
#include "./OgreString.h"
#include "./WindowUtilsHG3D.h"
#include "HG3DUtilities.h"



typedef struct hg3dclass_struct {
	void *ptr;
	void *fptr;
} hg3dclass_struct;

void *getHG3DClassPtr(hg3dclass_struct inSt, const char* className)
{
	void *(*ptrcaster)(const char*, void*);
	ptrcaster = (void *(*)(const char*, void*))(inSt.fptr);
	return ((*ptrcaster)(className, inSt.ptr));
}

//
// Ogre::Animation
//

// Ptr Caster
void *internalHG3D_Animation_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "Ogre::Animation") == 0) {
		return ptrIn;
	};
	printf("PtrCaster not successful, Class: Ogre::Animation is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_Animation(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_Animation_PtrCaster);
	return st;
};

//
// Ogre::AnimationState
//

// Ptr Caster
void *internalHG3D_AnimationState_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "Ogre::AnimationState") == 0) {
		return ptrIn;
	};
	printf("PtrCaster not successful, Class: Ogre::AnimationState is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_AnimationState(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_AnimationState_PtrCaster);
	return st;
};

//
// Ogre::AnimationStateSet
//

// Ptr Caster
void *internalHG3D_AnimationStateSet_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "Ogre::AnimationStateSet") == 0) {
		return ptrIn;
	};
	printf("PtrCaster not successful, Class: Ogre::AnimationStateSet is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_AnimationStateSet(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_AnimationStateSet_PtrCaster);
	return st;
};

//
// Ogre::AnimationTrack
//

// Ptr Caster
void *internalHG3D_AnimationTrack_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "Ogre::AnimationTrack") == 0) {
		return ptrIn;
	};
	printf("PtrCaster not successful, Class: Ogre::AnimationTrack is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_AnimationTrack(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_AnimationTrack_PtrCaster);
	return st;
};

//
// Ogre::Archive
//

// Ptr Caster
void *internalHG3D_Archive_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "Ogre::Archive") == 0) {
		return ptrIn;
	};
	printf("PtrCaster not successful, Class: Ogre::Archive is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_Archive(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_Archive_PtrCaster);
	return st;
};

//
// Ogre::ArchiveManager
//

// Ptr Caster
void *internalHG3D_ArchiveManager_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "Ogre::ArchiveManager") == 0) {
		return ptrIn;
	};
	printf("PtrCaster not successful, Class: Ogre::ArchiveManager is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_ArchiveManager(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_ArchiveManager_PtrCaster);
	return st;
};

//
// Ogre::Billboard
//

// Ptr Caster
void *internalHG3D_Billboard_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "Ogre::Billboard") == 0) {
		return ptrIn;
	};
	printf("PtrCaster not successful, Class: Ogre::Billboard is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_Billboard(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_Billboard_PtrCaster);
	return st;
};

//
// Ogre::BillboardChain
//

// Ptr Caster
void *internalHG3D_BillboardChain_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "Ogre::BillboardChain") == 0) {
		return ptrIn;
	};
	if (strcmp(className, "Ogre::MovableObject") == 0) {
		return (void *)(Ogre::MovableObject *)(Ogre::BillboardChain *)ptrIn;
	};
	if (strcmp(className, "Ogre::Renderable") == 0) {
		return (void *)(Ogre::Renderable *)(Ogre::BillboardChain *)ptrIn;
	};
	if (strcmp(className, "Ogre::AnimableObject") == 0) {
		return (void *)(Ogre::AnimableObject *)(Ogre::BillboardChain *)ptrIn;
	};
	printf("PtrCaster not successful, Class: Ogre::BillboardChain is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_BillboardChain(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_BillboardChain_PtrCaster);
	return st;
};

//
// Ogre::BillboardChainFactory
//

// Ptr Caster
void *internalHG3D_BillboardChainFactory_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "Ogre::BillboardChainFactory") == 0) {
		return ptrIn;
	};
	if (strcmp(className, "Ogre::MovableObjectFactory") == 0) {
		return (void *)(Ogre::MovableObjectFactory *)(Ogre::BillboardChainFactory *)ptrIn;
	};
	printf("PtrCaster not successful, Class: Ogre::BillboardChainFactory is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_BillboardChainFactory(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_BillboardChainFactory_PtrCaster);
	return st;
};

//
// Ogre::BillboardSet
//

// Ptr Caster
void *internalHG3D_BillboardSet_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "Ogre::BillboardSet") == 0) {
		return ptrIn;
	};
	if (strcmp(className, "Ogre::MovableObject") == 0) {
		return (void *)(Ogre::MovableObject *)(Ogre::BillboardSet *)ptrIn;
	};
	if (strcmp(className, "Ogre::Renderable") == 0) {
		return (void *)(Ogre::Renderable *)(Ogre::BillboardSet *)ptrIn;
	};
	if (strcmp(className, "Ogre::AnimableObject") == 0) {
		return (void *)(Ogre::AnimableObject *)(Ogre::BillboardSet *)ptrIn;
	};
	printf("PtrCaster not successful, Class: Ogre::BillboardSet is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_BillboardSet(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_BillboardSet_PtrCaster);
	return st;
};

//
// Ogre::BillboardSetFactory
//

// Ptr Caster
void *internalHG3D_BillboardSetFactory_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "Ogre::BillboardSetFactory") == 0) {
		return ptrIn;
	};
	if (strcmp(className, "Ogre::MovableObjectFactory") == 0) {
		return (void *)(Ogre::MovableObjectFactory *)(Ogre::BillboardSetFactory *)ptrIn;
	};
	printf("PtrCaster not successful, Class: Ogre::BillboardSetFactory is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_BillboardSetFactory(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_BillboardSetFactory_PtrCaster);
	return st;
};

//
// Ogre::Bone
//

// Ptr Caster
void *internalHG3D_Bone_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "Ogre::Bone") == 0) {
		return ptrIn;
	};
	if (strcmp(className, "Ogre::Node") == 0) {
		return (void *)(Ogre::Node *)(Ogre::Bone *)ptrIn;
	};
	printf("PtrCaster not successful, Class: Ogre::Bone is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_Bone(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_Bone_PtrCaster);
	return st;
};

//
// Ogre::Camera
//

// Ptr Caster
void *internalHG3D_Camera_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "Ogre::Camera") == 0) {
		return ptrIn;
	};
	if (strcmp(className, "Ogre::Frustum") == 0) {
		return (void *)(Ogre::Frustum *)(Ogre::Camera *)ptrIn;
	};
	if (strcmp(className, "Ogre::MovableObject") == 0) {
		return (void *)(Ogre::MovableObject *)(Ogre::Camera *)ptrIn;
	};
	if (strcmp(className, "Ogre::Renderable") == 0) {
		return (void *)(Ogre::Renderable *)(Ogre::Camera *)ptrIn;
	};
	if (strcmp(className, "Ogre::AnimableObject") == 0) {
		return (void *)(Ogre::AnimableObject *)(Ogre::Camera *)ptrIn;
	};
	printf("PtrCaster not successful, Class: Ogre::Camera is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_Camera(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_Camera_PtrCaster);
	return st;
};

//
// Ogre::ConfigFile
//

// Ptr Caster
void *internalHG3D_ConfigFile_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "Ogre::ConfigFile") == 0) {
		return ptrIn;
	};
	printf("PtrCaster not successful, Class: Ogre::ConfigFile is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_ConfigFile(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_ConfigFile_PtrCaster);
	return st;
};

//
// Ogre::ControllerManager
//

// Ptr Caster
void *internalHG3D_ControllerManager_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "Ogre::ControllerManager") == 0) {
		return ptrIn;
	};
	printf("PtrCaster not successful, Class: Ogre::ControllerManager is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_ControllerManager(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_ControllerManager_PtrCaster);
	return st;
};

//
// Ogre::Entity
//

// Ptr Caster
void *internalHG3D_Entity_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "Ogre::Entity") == 0) {
		return ptrIn;
	};
	if (strcmp(className, "Ogre::MovableObject") == 0) {
		return (void *)(Ogre::MovableObject *)(Ogre::Entity *)ptrIn;
	};
	if (strcmp(className, "Ogre::Resource::Listener") == 0) {
		return (void *)(Ogre::Resource::Listener *)(Ogre::Entity *)ptrIn;
	};
	if (strcmp(className, "Ogre::AnimableObject") == 0) {
		return (void *)(Ogre::AnimableObject *)(Ogre::Entity *)ptrIn;
	};
	printf("PtrCaster not successful, Class: Ogre::Entity is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_Entity(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_Entity_PtrCaster);
	return st;
};

//
// Ogre::EntityFactory
//

// Ptr Caster
void *internalHG3D_EntityFactory_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "Ogre::EntityFactory") == 0) {
		return ptrIn;
	};
	if (strcmp(className, "Ogre::MovableObjectFactory") == 0) {
		return (void *)(Ogre::MovableObjectFactory *)(Ogre::EntityFactory *)ptrIn;
	};
	printf("PtrCaster not successful, Class: Ogre::EntityFactory is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_EntityFactory(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_EntityFactory_PtrCaster);
	return st;
};

//
// Ogre::Exception
//

// Ptr Caster
void *internalHG3D_Exception_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "Ogre::Exception") == 0) {
		return ptrIn;
	};
	printf("PtrCaster not successful, Class: Ogre::Exception is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_Exception(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_Exception_PtrCaster);
	return st;
};

//
// Ogre::Frustum
//

// Ptr Caster
void *internalHG3D_Frustum_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "Ogre::Frustum") == 0) {
		return ptrIn;
	};
	if (strcmp(className, "Ogre::MovableObject") == 0) {
		return (void *)(Ogre::MovableObject *)(Ogre::Frustum *)ptrIn;
	};
	if (strcmp(className, "Ogre::Renderable") == 0) {
		return (void *)(Ogre::Renderable *)(Ogre::Frustum *)ptrIn;
	};
	if (strcmp(className, "Ogre::AnimableObject") == 0) {
		return (void *)(Ogre::AnimableObject *)(Ogre::Frustum *)ptrIn;
	};
	printf("PtrCaster not successful, Class: Ogre::Frustum is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_Frustum(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_Frustum_PtrCaster);
	return st;
};

//
// Ogre::Light
//

// Ptr Caster
void *internalHG3D_Light_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "Ogre::Light") == 0) {
		return ptrIn;
	};
	if (strcmp(className, "Ogre::MovableObject") == 0) {
		return (void *)(Ogre::MovableObject *)(Ogre::Light *)ptrIn;
	};
	if (strcmp(className, "Ogre::AnimableObject") == 0) {
		return (void *)(Ogre::AnimableObject *)(Ogre::Light *)ptrIn;
	};
	printf("PtrCaster not successful, Class: Ogre::Light is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_Light(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_Light_PtrCaster);
	return st;
};

//
// Ogre::LightFactory
//

// Ptr Caster
void *internalHG3D_LightFactory_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "Ogre::LightFactory") == 0) {
		return ptrIn;
	};
	if (strcmp(className, "Ogre::MovableObjectFactory") == 0) {
		return (void *)(Ogre::MovableObjectFactory *)(Ogre::LightFactory *)ptrIn;
	};
	printf("PtrCaster not successful, Class: Ogre::LightFactory is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_LightFactory(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_LightFactory_PtrCaster);
	return st;
};

//
// Ogre::Log
//

// Ptr Caster
void *internalHG3D_Log_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "Ogre::Log") == 0) {
		return ptrIn;
	};
	printf("PtrCaster not successful, Class: Ogre::Log is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_Log(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_Log_PtrCaster);
	return st;
};

//
// Ogre::LogManager
//

// Ptr Caster
void *internalHG3D_LogManager_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "Ogre::LogManager") == 0) {
		return ptrIn;
	};
	printf("PtrCaster not successful, Class: Ogre::LogManager is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_LogManager(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_LogManager_PtrCaster);
	return st;
};

//
// Ogre::ManualObject
//

// Ptr Caster
void *internalHG3D_ManualObject_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "Ogre::ManualObject") == 0) {
		return ptrIn;
	};
	if (strcmp(className, "Ogre::MovableObject") == 0) {
		return (void *)(Ogre::MovableObject *)(Ogre::ManualObject *)ptrIn;
	};
	if (strcmp(className, "Ogre::AnimableObject") == 0) {
		return (void *)(Ogre::AnimableObject *)(Ogre::ManualObject *)ptrIn;
	};
	printf("PtrCaster not successful, Class: Ogre::ManualObject is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_ManualObject(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_ManualObject_PtrCaster);
	return st;
};

//
// Ogre::ManualObjectFactory
//

// Ptr Caster
void *internalHG3D_ManualObjectFactory_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "Ogre::ManualObjectFactory") == 0) {
		return ptrIn;
	};
	if (strcmp(className, "Ogre::MovableObjectFactory") == 0) {
		return (void *)(Ogre::MovableObjectFactory *)(Ogre::ManualObjectFactory *)ptrIn;
	};
	printf("PtrCaster not successful, Class: Ogre::ManualObjectFactory is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_ManualObjectFactory(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_ManualObjectFactory_PtrCaster);
	return st;
};

//
// Ogre::ManualObject::ManualObjectSection
//

// Ptr Caster
void *internalHG3D_ManualObjectSection_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "Ogre::ManualObject::ManualObjectSection") == 0) {
		return ptrIn;
	};
	if (strcmp(className, "Ogre::Renderable") == 0) {
		return (void *)(Ogre::Renderable *)(Ogre::ManualObject::ManualObjectSection *)ptrIn;
	};
	printf("PtrCaster not successful, Class: Ogre::ManualObject::ManualObjectSection is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_ManualObjectSection(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_ManualObjectSection_PtrCaster);
	return st;
};

//
// Ogre::Material
//

// Ptr Caster
void *internalHG3D_Material_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "Ogre::Material") == 0) {
		return ptrIn;
	};
	if (strcmp(className, "Ogre::Resource") == 0) {
		return (void *)(Ogre::Resource *)(Ogre::Material *)ptrIn;
	};
	printf("PtrCaster not successful, Class: Ogre::Material is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_Material(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_Material_PtrCaster);
	return st;
};

//
// Ogre::MaterialManager
//

// Ptr Caster
void *internalHG3D_MaterialManager_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "Ogre::MaterialManager") == 0) {
		return ptrIn;
	};
	if (strcmp(className, "Ogre::ResourceManager") == 0) {
		return (void *)(Ogre::ResourceManager *)(Ogre::MaterialManager *)ptrIn;
	};
	printf("PtrCaster not successful, Class: Ogre::MaterialManager is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_MaterialManager(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_MaterialManager_PtrCaster);
	return st;
};

//
// Ogre::Mesh
//

// Ptr Caster
void *internalHG3D_Mesh_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "Ogre::Mesh") == 0) {
		return ptrIn;
	};
	if (strcmp(className, "Ogre::Resource") == 0) {
		return (void *)(Ogre::Resource *)(Ogre::Mesh *)ptrIn;
	};
	if (strcmp(className, "Ogre::AnimationContainer") == 0) {
		return (void *)(Ogre::AnimationContainer *)(Ogre::Mesh *)ptrIn;
	};
	printf("PtrCaster not successful, Class: Ogre::Mesh is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_Mesh(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_Mesh_PtrCaster);
	return st;
};

//
// Ogre::MeshManager
//

// Ptr Caster
void *internalHG3D_MeshManager_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "Ogre::MeshManager") == 0) {
		return ptrIn;
	};
	if (strcmp(className, "Ogre::ResourceManager") == 0) {
		return (void *)(Ogre::ResourceManager *)(Ogre::MeshManager *)ptrIn;
	};
	if (strcmp(className, "Ogre::ManualResourceLoader") == 0) {
		return (void *)(Ogre::ManualResourceLoader *)(Ogre::MeshManager *)ptrIn;
	};
	printf("PtrCaster not successful, Class: Ogre::MeshManager is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_MeshManager(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_MeshManager_PtrCaster);
	return st;
};

//
// Ogre::MovableObject
//

// Ptr Caster
void *internalHG3D_MovableObject_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "Ogre::MovableObject") == 0) {
		return ptrIn;
	};
	if (strcmp(className, "Ogre::AnimableObject") == 0) {
		return (void *)(Ogre::AnimableObject *)(Ogre::MovableObject *)ptrIn;
	};
	printf("PtrCaster not successful, Class: Ogre::MovableObject is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_MovableObject(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_MovableObject_PtrCaster);
	return st;
};

//
// Ogre::MovableObjectFactory
//

// Ptr Caster
void *internalHG3D_MovableObjectFactory_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "Ogre::MovableObjectFactory") == 0) {
		return ptrIn;
	};
	printf("PtrCaster not successful, Class: Ogre::MovableObjectFactory is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_MovableObjectFactory(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_MovableObjectFactory_PtrCaster);
	return st;
};

//
// Ogre::MultiRenderTarget
//

// Ptr Caster
void *internalHG3D_MultiRenderTarget_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "Ogre::MultiRenderTarget") == 0) {
		return ptrIn;
	};
	if (strcmp(className, "Ogre::RenderTarget") == 0) {
		return (void *)(Ogre::RenderTarget *)(Ogre::MultiRenderTarget *)ptrIn;
	};
	printf("PtrCaster not successful, Class: Ogre::MultiRenderTarget is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_MultiRenderTarget(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_MultiRenderTarget_PtrCaster);
	return st;
};

//
// Ogre::Node
//

// Ptr Caster
void *internalHG3D_Node_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "Ogre::Node") == 0) {
		return ptrIn;
	};
	printf("PtrCaster not successful, Class: Ogre::Node is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_Node(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_Node_PtrCaster);
	return st;
};

//
// Ogre::NodeAnimationTrack
//

// Ptr Caster
void *internalHG3D_NodeAnimationTrack_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "Ogre::NodeAnimationTrack") == 0) {
		return ptrIn;
	};
	if (strcmp(className, "Ogre::AnimationTrack") == 0) {
		return (void *)(Ogre::AnimationTrack *)(Ogre::NodeAnimationTrack *)ptrIn;
	};
	printf("PtrCaster not successful, Class: Ogre::NodeAnimationTrack is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_NodeAnimationTrack(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_NodeAnimationTrack_PtrCaster);
	return st;
};

//
// Ogre::NumericAnimationTrack
//

// Ptr Caster
void *internalHG3D_NumericAnimationTrack_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "Ogre::NumericAnimationTrack") == 0) {
		return ptrIn;
	};
	if (strcmp(className, "Ogre::AnimationTrack") == 0) {
		return (void *)(Ogre::AnimationTrack *)(Ogre::NumericAnimationTrack *)ptrIn;
	};
	printf("PtrCaster not successful, Class: Ogre::NumericAnimationTrack is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_NumericAnimationTrack(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_NumericAnimationTrack_PtrCaster);
	return st;
};

//
// Ogre::Renderable
//

// Ptr Caster
void *internalHG3D_Renderable_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "Ogre::Renderable") == 0) {
		return ptrIn;
	};
	printf("PtrCaster not successful, Class: Ogre::Renderable is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_Renderable(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_Renderable_PtrCaster);
	return st;
};

//
// Ogre::RenderSystem
//

// Ptr Caster
void *internalHG3D_RenderSystem_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "Ogre::RenderSystem") == 0) {
		return ptrIn;
	};
	printf("PtrCaster not successful, Class: Ogre::RenderSystem is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_RenderSystem(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_RenderSystem_PtrCaster);
	return st;
};

//
// Ogre::RenderTarget
//

// Ptr Caster
void *internalHG3D_RenderTarget_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "Ogre::RenderTarget") == 0) {
		return ptrIn;
	};
	printf("PtrCaster not successful, Class: Ogre::RenderTarget is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_RenderTarget(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_RenderTarget_PtrCaster);
	return st;
};

//
// Ogre::RenderTexture
//

// Ptr Caster
void *internalHG3D_RenderTexture_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "Ogre::RenderTexture") == 0) {
		return ptrIn;
	};
	if (strcmp(className, "Ogre::RenderTarget") == 0) {
		return (void *)(Ogre::RenderTarget *)(Ogre::RenderTexture *)ptrIn;
	};
	printf("PtrCaster not successful, Class: Ogre::RenderTexture is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_RenderTexture(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_RenderTexture_PtrCaster);
	return st;
};

//
// Ogre::RenderWindow
//

// Ptr Caster
void *internalHG3D_RenderWindow_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "Ogre::RenderWindow") == 0) {
		return ptrIn;
	};
	if (strcmp(className, "Ogre::RenderTarget") == 0) {
		return (void *)(Ogre::RenderTarget *)(Ogre::RenderWindow *)ptrIn;
	};
	printf("PtrCaster not successful, Class: Ogre::RenderWindow is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_RenderWindow(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_RenderWindow_PtrCaster);
	return st;
};

//
// Ogre::Resource
//

// Ptr Caster
void *internalHG3D_Resource_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "Ogre::Resource") == 0) {
		return ptrIn;
	};
	printf("PtrCaster not successful, Class: Ogre::Resource is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_Resource(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_Resource_PtrCaster);
	return st;
};

//
// Ogre::ResourceGroupManager
//

// Ptr Caster
void *internalHG3D_ResourceGroupManager_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "Ogre::ResourceGroupManager") == 0) {
		return ptrIn;
	};
	printf("PtrCaster not successful, Class: Ogre::ResourceGroupManager is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_ResourceGroupManager(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_ResourceGroupManager_PtrCaster);
	return st;
};

//
// Ogre::ResourceManager
//

// Ptr Caster
void *internalHG3D_ResourceManager_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "Ogre::ResourceManager") == 0) {
		return ptrIn;
	};
	printf("PtrCaster not successful, Class: Ogre::ResourceManager is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_ResourceManager(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_ResourceManager_PtrCaster);
	return st;
};

//
// Ogre::Root
//

// Ptr Caster
void *internalHG3D_Root_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "Ogre::Root") == 0) {
		return ptrIn;
	};
	printf("PtrCaster not successful, Class: Ogre::Root is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_Root(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_Root_PtrCaster);
	return st;
};

//
// Ogre::SceneManager
//

// Ptr Caster
void *internalHG3D_SceneManager_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "Ogre::SceneManager") == 0) {
		return ptrIn;
	};
	printf("PtrCaster not successful, Class: Ogre::SceneManager is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_SceneManager(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_SceneManager_PtrCaster);
	return st;
};

//
// Ogre::SceneManagerFactory
//

// Ptr Caster
void *internalHG3D_SceneManagerFactory_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "Ogre::SceneManagerFactory") == 0) {
		return ptrIn;
	};
	printf("PtrCaster not successful, Class: Ogre::SceneManagerFactory is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_SceneManagerFactory(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_SceneManagerFactory_PtrCaster);
	return st;
};

//
// Ogre::SceneNode
//

// Ptr Caster
void *internalHG3D_SceneNode_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "Ogre::SceneNode") == 0) {
		return ptrIn;
	};
	if (strcmp(className, "Ogre::Node") == 0) {
		return (void *)(Ogre::Node *)(Ogre::SceneNode *)ptrIn;
	};
	printf("PtrCaster not successful, Class: Ogre::SceneNode is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_SceneNode(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_SceneNode_PtrCaster);
	return st;
};

//
// Ogre::Skeleton
//

// Ptr Caster
void *internalHG3D_Skeleton_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "Ogre::Skeleton") == 0) {
		return ptrIn;
	};
	if (strcmp(className, "Ogre::Resource") == 0) {
		return (void *)(Ogre::Resource *)(Ogre::Skeleton *)ptrIn;
	};
	if (strcmp(className, "Ogre::AnimationContainer") == 0) {
		return (void *)(Ogre::AnimationContainer *)(Ogre::Skeleton *)ptrIn;
	};
	printf("PtrCaster not successful, Class: Ogre::Skeleton is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_Skeleton(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_Skeleton_PtrCaster);
	return st;
};

//
// Ogre::SkeletonManager
//

// Ptr Caster
void *internalHG3D_SkeletonManager_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "Ogre::SkeletonManager") == 0) {
		return ptrIn;
	};
	if (strcmp(className, "Ogre::ResourceManager") == 0) {
		return (void *)(Ogre::ResourceManager *)(Ogre::SkeletonManager *)ptrIn;
	};
	printf("PtrCaster not successful, Class: Ogre::SkeletonManager is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_SkeletonManager(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_SkeletonManager_PtrCaster);
	return st;
};

//
// Ogre::TextureManager
//

// Ptr Caster
void *internalHG3D_TextureManager_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "Ogre::TextureManager") == 0) {
		return ptrIn;
	};
	if (strcmp(className, "Ogre::ResourceManager") == 0) {
		return (void *)(Ogre::ResourceManager *)(Ogre::TextureManager *)ptrIn;
	};
	printf("PtrCaster not successful, Class: Ogre::TextureManager is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_TextureManager(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_TextureManager_PtrCaster);
	return st;
};

//
// Ogre::TimeIndex
//

// Ptr Caster
void *internalHG3D_TimeIndex_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "Ogre::TimeIndex") == 0) {
		return ptrIn;
	};
	printf("PtrCaster not successful, Class: Ogre::TimeIndex is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_TimeIndex(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_TimeIndex_PtrCaster);
	return st;
};

//
// Ogre::VertexAnimationTrack
//

// Ptr Caster
void *internalHG3D_VertexAnimationTrack_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "Ogre::VertexAnimationTrack") == 0) {
		return ptrIn;
	};
	if (strcmp(className, "Ogre::AnimationTrack") == 0) {
		return (void *)(Ogre::AnimationTrack *)(Ogre::VertexAnimationTrack *)ptrIn;
	};
	printf("PtrCaster not successful, Class: Ogre::VertexAnimationTrack is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_VertexAnimationTrack(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_VertexAnimationTrack_PtrCaster);
	return st;
};

//
// Ogre::Viewport
//

// Ptr Caster
void *internalHG3D_Viewport_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "Ogre::Viewport") == 0) {
		return ptrIn;
	};
	printf("PtrCaster not successful, Class: Ogre::Viewport is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_Viewport(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_Viewport_PtrCaster);
	return st;
};

//
// Ogre::WindowEventUtilities
//

// Ptr Caster
void *internalHG3D_WindowEventUtilities_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "Ogre::WindowEventUtilities") == 0) {
		return ptrIn;
	};
	printf("PtrCaster not successful, Class: Ogre::WindowEventUtilities is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_WindowEventUtilities(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_WindowEventUtilities_PtrCaster);
	return st;
};

//
// HG3DUtilities
//

// Ptr Caster
void *internalHG3D_HG3DUtilities_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "HG3DUtilities") == 0) {
		return ptrIn;
	};
	printf("PtrCaster not successful, Class: HG3DUtilities is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_HG3DUtilities(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_HG3DUtilities_PtrCaster);
	return st;
};

