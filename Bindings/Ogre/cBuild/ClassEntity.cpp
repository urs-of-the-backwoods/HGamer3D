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

// ClassEntity.cpp

// 

#include <wchar.h>
#include <string>
#include <iostream>

#include <iostream>
	#include <typeinfo>
	#include <stdio.h>
	#include <cstring>
	#include <exception>
	#include "OgreDllDefines.h"
	#include "ClassPtr.h"
	#include "StructSharedPtr.h"
#include "EnumEntityVertexDataBindChoice.h"
#include "./Ogre.h"
#include "./OgreString.h"
#include "./WindowUtilsHG3D.h"
#include "HG3DUtilities.h"

using namespace Ogre;



// 
extern "C" Ogre_LIB_EXPORT void ogre_ent_destruct(struct hg3dclass_struct * thisclass_c)
{
  Ogre::Entity * thisclass_cpp = static_cast<Ogre::Entity*> (getHG3DClassPtr(*thisclass_c, "Ogre::Entity"));
  (delete thisclass_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_ent_getMesh(struct hg3dclass_struct * thisclass_c, struct sharedptr_struct * result_c)
{
  Ogre::Entity * thisclass_cpp = static_cast<Ogre::Entity*> (getHG3DClassPtr(*thisclass_c, "Ogre::Entity"));
  MeshPtr result_cpp;
  result_cpp = (thisclass_cpp->getMesh());
  *result_c = *((struct sharedptr_struct*) &result_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_ent_getNumSubEntities(struct hg3dclass_struct * thisclass_c, unsigned int * result_c)
{
  Ogre::Entity * thisclass_cpp = static_cast<Ogre::Entity*> (getHG3DClassPtr(*thisclass_c, "Ogre::Entity"));
  unsigned int result_cpp;
  result_cpp = (thisclass_cpp->getNumSubEntities());
  *result_c = (unsigned int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_ent_clone(struct hg3dclass_struct * thisclass_c, char * newName_c, struct hg3dclass_struct * result_c)
{
  Ogre::Entity * thisclass_cpp = static_cast<Ogre::Entity*> (getHG3DClassPtr(*thisclass_c, "Ogre::Entity"));
  Ogre::String newName_cpp = Ogre::String((const char*) newName_c);
  Ogre::Entity * result_cpp;
  result_cpp = (thisclass_cpp->clone(newName_cpp));
  *result_c = getHG3DClass_Entity((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_ent_setMaterialName(struct hg3dclass_struct * thisclass_c, char * name_c, char * groupName_c)
{
  Ogre::Entity * thisclass_cpp = static_cast<Ogre::Entity*> (getHG3DClassPtr(*thisclass_c, "Ogre::Entity"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  Ogre::String groupName_cpp = Ogre::String((const char*) groupName_c);
  (thisclass_cpp->setMaterialName(name_cpp, groupName_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_ent_setMaterial(struct hg3dclass_struct * thisclass_c, struct sharedptr_struct * material_c)
{
  Ogre::Entity * thisclass_cpp = static_cast<Ogre::Entity*> (getHG3DClassPtr(*thisclass_c, "Ogre::Entity"));
  MaterialPtr material_cpp = *((MaterialPtr*) material_c);
  (thisclass_cpp->setMaterial(material_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_ent_getMovableType(struct hg3dclass_struct * thisclass_c, char * result_c)
{
  Ogre::Entity * thisclass_cpp = static_cast<Ogre::Entity*> (getHG3DClassPtr(*thisclass_c, "Ogre::Entity"));
  Ogre::String result_cpp;
  result_cpp = (thisclass_cpp->getMovableType());
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_ent_getAnimationState(struct hg3dclass_struct * thisclass_c, char * name_c, struct hg3dclass_struct * result_c)
{
  Ogre::Entity * thisclass_cpp = static_cast<Ogre::Entity*> (getHG3DClassPtr(*thisclass_c, "Ogre::Entity"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  Ogre::AnimationState * result_cpp;
  result_cpp = (thisclass_cpp->getAnimationState(name_cpp));
  *result_c = getHG3DClass_AnimationState((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_ent_hasAnimationState(struct hg3dclass_struct * thisclass_c, char * name_c, int * result_c)
{
  Ogre::Entity * thisclass_cpp = static_cast<Ogre::Entity*> (getHG3DClassPtr(*thisclass_c, "Ogre::Entity"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  bool result_cpp;
  result_cpp = (thisclass_cpp->hasAnimationState(name_cpp));
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_ent_getAllAnimationStates(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  Ogre::Entity * thisclass_cpp = static_cast<Ogre::Entity*> (getHG3DClassPtr(*thisclass_c, "Ogre::Entity"));
  Ogre::AnimationStateSet * result_cpp;
  result_cpp = (thisclass_cpp->getAllAnimationStates());
  *result_c = getHG3DClass_AnimationStateSet((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_ent_setDisplaySkeleton(struct hg3dclass_struct * thisclass_c, int display_c)
{
  Ogre::Entity * thisclass_cpp = static_cast<Ogre::Entity*> (getHG3DClassPtr(*thisclass_c, "Ogre::Entity"));
  bool display_cpp = (bool)display_c;
  (thisclass_cpp->setDisplaySkeleton(display_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_ent_getDisplaySkeleton(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::Entity * thisclass_cpp = static_cast<Ogre::Entity*> (getHG3DClassPtr(*thisclass_c, "Ogre::Entity"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->getDisplaySkeleton());
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_ent_getManualLodLevel(struct hg3dclass_struct * thisclass_c, int index_c, struct hg3dclass_struct * result_c)
{
  Ogre::Entity * thisclass_cpp = static_cast<Ogre::Entity*> (getHG3DClassPtr(*thisclass_c, "Ogre::Entity"));
  size_t index_cpp = (size_t)index_c;
  Ogre::Entity * result_cpp;
  result_cpp = (thisclass_cpp->getManualLodLevel(index_cpp));
  *result_c = getHG3DClass_Entity((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_ent_getNumManualLodLevels(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::Entity * thisclass_cpp = static_cast<Ogre::Entity*> (getHG3DClassPtr(*thisclass_c, "Ogre::Entity"));
  size_t result_cpp;
  result_cpp = (thisclass_cpp->getNumManualLodLevels());
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_ent_setPolygonModeOverrideable(struct hg3dclass_struct * thisclass_c, int PolygonModeOverrideable_c)
{
  Ogre::Entity * thisclass_cpp = static_cast<Ogre::Entity*> (getHG3DClassPtr(*thisclass_c, "Ogre::Entity"));
  bool PolygonModeOverrideable_cpp = (bool)PolygonModeOverrideable_c;
  (thisclass_cpp->setPolygonModeOverrideable(PolygonModeOverrideable_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_ent_detachObjectFromBone(struct hg3dclass_struct * thisclass_c, char * movableName_c, struct hg3dclass_struct * result_c)
{
  Ogre::Entity * thisclass_cpp = static_cast<Ogre::Entity*> (getHG3DClassPtr(*thisclass_c, "Ogre::Entity"));
  Ogre::String movableName_cpp = Ogre::String((const char*) movableName_c);
  Ogre::MovableObject * result_cpp;
  result_cpp = (thisclass_cpp->detachObjectFromBone(movableName_cpp));
  *result_c = getHG3DClass_MovableObject((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_ent_detachObjectFromBone2(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * obj_c)
{
  Ogre::Entity * thisclass_cpp = static_cast<Ogre::Entity*> (getHG3DClassPtr(*thisclass_c, "Ogre::Entity"));
  Ogre::MovableObject * obj_cpp = static_cast<Ogre::MovableObject*> (getHG3DClassPtr(*obj_c, "Ogre::MovableObject"));
  (thisclass_cpp->detachObjectFromBone(obj_cpp));
};

// Detach all MovableObjects previously attached using attachObjectToBone. 
extern "C" Ogre_LIB_EXPORT void ogre_ent_detachAllObjectsFromBone(struct hg3dclass_struct * thisclass_c)
{
  Ogre::Entity * thisclass_cpp = static_cast<Ogre::Entity*> (getHG3DClassPtr(*thisclass_c, "Ogre::Entity"));
  (thisclass_cpp->detachAllObjectsFromBone());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_ent_getBoundingRadius(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  Ogre::Entity * thisclass_cpp = static_cast<Ogre::Entity*> (getHG3DClassPtr(*thisclass_c, "Ogre::Entity"));
  Real result_cpp;
  result_cpp = (thisclass_cpp->getBoundingRadius());
  *result_c = (float)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_ent_hasEdgeList(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::Entity * thisclass_cpp = static_cast<Ogre::Entity*> (getHG3DClassPtr(*thisclass_c, "Ogre::Entity"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->hasEdgeList());
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_ent_hasSkeleton(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::Entity * thisclass_cpp = static_cast<Ogre::Entity*> (getHG3DClassPtr(*thisclass_c, "Ogre::Entity"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->hasSkeleton());
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_ent_isHardwareAnimationEnabled(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::Entity * thisclass_cpp = static_cast<Ogre::Entity*> (getHG3DClassPtr(*thisclass_c, "Ogre::Entity"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isHardwareAnimationEnabled());
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_ent_getSoftwareAnimationRequests(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::Entity * thisclass_cpp = static_cast<Ogre::Entity*> (getHG3DClassPtr(*thisclass_c, "Ogre::Entity"));
  int result_cpp;
  result_cpp = (thisclass_cpp->getSoftwareAnimationRequests());
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_ent_getSoftwareAnimationNormalsRequests(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::Entity * thisclass_cpp = static_cast<Ogre::Entity*> (getHG3DClassPtr(*thisclass_c, "Ogre::Entity"));
  int result_cpp;
  result_cpp = (thisclass_cpp->getSoftwareAnimationNormalsRequests());
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_ent_addSoftwareAnimationRequest(struct hg3dclass_struct * thisclass_c, int normalsAlso_c)
{
  Ogre::Entity * thisclass_cpp = static_cast<Ogre::Entity*> (getHG3DClassPtr(*thisclass_c, "Ogre::Entity"));
  bool normalsAlso_cpp = (bool)normalsAlso_c;
  (thisclass_cpp->addSoftwareAnimationRequest(normalsAlso_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_ent_removeSoftwareAnimationRequest(struct hg3dclass_struct * thisclass_c, int normalsAlso_c)
{
  Ogre::Entity * thisclass_cpp = static_cast<Ogre::Entity*> (getHG3DClassPtr(*thisclass_c, "Ogre::Entity"));
  bool normalsAlso_cpp = (bool)normalsAlso_c;
  (thisclass_cpp->removeSoftwareAnimationRequest(normalsAlso_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_ent_shareSkeletonInstanceWith(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * entity_c)
{
  Ogre::Entity * thisclass_cpp = static_cast<Ogre::Entity*> (getHG3DClassPtr(*thisclass_c, "Ogre::Entity"));
  Ogre::Entity * entity_cpp = static_cast<Ogre::Entity*> (getHG3DClassPtr(*entity_c, "Ogre::Entity"));
  (thisclass_cpp->shareSkeletonInstanceWith(entity_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_ent_hasVertexAnimation(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::Entity * thisclass_cpp = static_cast<Ogre::Entity*> (getHG3DClassPtr(*thisclass_c, "Ogre::Entity"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->hasVertexAnimation());
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_ent_stopSharingSkeletonInstance(struct hg3dclass_struct * thisclass_c)
{
  Ogre::Entity * thisclass_cpp = static_cast<Ogre::Entity*> (getHG3DClassPtr(*thisclass_c, "Ogre::Entity"));
  (thisclass_cpp->stopSharingSkeletonInstance());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_ent_sharesSkeletonInstance(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::Entity * thisclass_cpp = static_cast<Ogre::Entity*> (getHG3DClassPtr(*thisclass_c, "Ogre::Entity"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->sharesSkeletonInstance());
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_ent_refreshAvailableAnimationState(struct hg3dclass_struct * thisclass_c)
{
  Ogre::Entity * thisclass_cpp = static_cast<Ogre::Entity*> (getHG3DClassPtr(*thisclass_c, "Ogre::Entity"));
  (thisclass_cpp->refreshAvailableAnimationState());
};

// Choose which vertex data to bind to the renderer. 
extern "C" Ogre_LIB_EXPORT void ogre_ent_chooseVertexDataForBinding(struct hg3dclass_struct * thisclass_c, int hasVertexAnim_c, enum EnumEntityVertexDataBindChoice * result_c)
{
  Ogre::Entity * thisclass_cpp = static_cast<Ogre::Entity*> (getHG3DClassPtr(*thisclass_c, "Ogre::Entity"));
  bool hasVertexAnim_cpp = (bool)hasVertexAnim_c;
  enum Ogre::Entity::VertexDataBindChoice result_cpp;
  result_cpp = (thisclass_cpp->chooseVertexDataForBinding(hasVertexAnim_cpp));
  *result_c = (enum EnumEntityVertexDataBindChoice) result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_ent_isInitialised(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::Entity * thisclass_cpp = static_cast<Ogre::Entity*> (getHG3DClassPtr(*thisclass_c, "Ogre::Entity"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isInitialised());
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_ent_backgroundLoadingComplete(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * res_c)
{
  Ogre::Entity * thisclass_cpp = static_cast<Ogre::Entity*> (getHG3DClassPtr(*thisclass_c, "Ogre::Entity"));
  Ogre::Resource * res_cpp = static_cast<Ogre::Resource*> (getHG3DClassPtr(*res_c, "Ogre::Resource"));
  (thisclass_cpp->backgroundLoadingComplete(res_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_ent_setSkipAnimationStateUpdate(struct hg3dclass_struct * thisclass_c, int skip_c)
{
  Ogre::Entity * thisclass_cpp = static_cast<Ogre::Entity*> (getHG3DClassPtr(*thisclass_c, "Ogre::Entity"));
  bool skip_cpp = (bool)skip_c;
  (thisclass_cpp->setSkipAnimationStateUpdate(skip_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_ent_getSkipAnimationStateUpdate(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::Entity * thisclass_cpp = static_cast<Ogre::Entity*> (getHG3DClassPtr(*thisclass_c, "Ogre::Entity"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->getSkipAnimationStateUpdate());
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_ent_setAlwaysUpdateMainSkeleton(struct hg3dclass_struct * thisclass_c, int update_c)
{
  Ogre::Entity * thisclass_cpp = static_cast<Ogre::Entity*> (getHG3DClassPtr(*thisclass_c, "Ogre::Entity"));
  bool update_cpp = (bool)update_c;
  (thisclass_cpp->setAlwaysUpdateMainSkeleton(update_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_ent_getAlwaysUpdateMainSkeleton(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::Entity * thisclass_cpp = static_cast<Ogre::Entity*> (getHG3DClassPtr(*thisclass_c, "Ogre::Entity"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->getAlwaysUpdateMainSkeleton());
  *result_c = (int)result_cpp;
};

