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

// ClassSceneManager.cpp

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
#include "EnumSceneManagerPrefabType.h"
#include "StructColour.h"
#include "StructQuaternion.h"
#include "EnumSceneManagerSpecialCaseRenderQueueMode.h"
#include "EnumLightType.h"
#include "./Ogre.h"
#include "./OgreString.h"
#include "./WindowUtilsHG3D.h"
#include "HG3DUtilities.h"

using namespace Ogre;
using Ogre::uint;



// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_destruct(struct hg3dclass_struct * thisclass_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  (delete thisclass_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_getName(struct hg3dclass_struct * thisclass_c, char * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::String result_cpp;
  result_cpp = (thisclass_cpp->getName());
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_getTypeName(struct hg3dclass_struct * thisclass_c, char * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::String result_cpp;
  result_cpp = (thisclass_cpp->getTypeName());
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_createCamera(struct hg3dclass_struct * thisclass_c, char * name_c, struct hg3dclass_struct * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  Ogre::Camera * result_cpp;
  result_cpp = (thisclass_cpp->createCamera(name_cpp));
  *result_c = getHG3DClass_Camera((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_getCamera(struct hg3dclass_struct * thisclass_c, char * name_c, struct hg3dclass_struct * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  Ogre::Camera * result_cpp;
  result_cpp = (thisclass_cpp->getCamera(name_cpp));
  *result_c = getHG3DClass_Camera((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_hasCamera(struct hg3dclass_struct * thisclass_c, char * name_c, int * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  bool result_cpp;
  result_cpp = (thisclass_cpp->hasCamera(name_cpp));
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_destroyCamera(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * cam_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::Camera * cam_cpp = static_cast<Ogre::Camera*> (getHG3DClassPtr(*cam_c, "Ogre::Camera"));
  (thisclass_cpp->destroyCamera(cam_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_destroyCamera2(struct hg3dclass_struct * thisclass_c, char * name_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  (thisclass_cpp->destroyCamera(name_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_destroyAllCameras(struct hg3dclass_struct * thisclass_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  (thisclass_cpp->destroyAllCameras());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_createLight(struct hg3dclass_struct * thisclass_c, char * name_c, struct hg3dclass_struct * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  Ogre::Light * result_cpp;
  result_cpp = (thisclass_cpp->createLight(name_cpp));
  *result_c = getHG3DClass_Light((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_createLight2(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::Light * result_cpp;
  result_cpp = (thisclass_cpp->createLight());
  *result_c = getHG3DClass_Light((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_getLight(struct hg3dclass_struct * thisclass_c, char * name_c, struct hg3dclass_struct * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  Ogre::Light * result_cpp;
  result_cpp = (thisclass_cpp->getLight(name_cpp));
  *result_c = getHG3DClass_Light((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_hasLight(struct hg3dclass_struct * thisclass_c, char * name_c, int * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  bool result_cpp;
  result_cpp = (thisclass_cpp->hasLight(name_cpp));
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_destroyLight(struct hg3dclass_struct * thisclass_c, char * name_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  (thisclass_cpp->destroyLight(name_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_destroyLight2(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * light_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::Light * light_cpp = static_cast<Ogre::Light*> (getHG3DClassPtr(*light_c, "Ogre::Light"));
  (thisclass_cpp->destroyLight(light_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_destroyAllLights(struct hg3dclass_struct * thisclass_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  (thisclass_cpp->destroyAllLights());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_createSceneNode(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::SceneNode * result_cpp;
  result_cpp = (thisclass_cpp->createSceneNode());
  *result_c = getHG3DClass_SceneNode((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_createSceneNode2(struct hg3dclass_struct * thisclass_c, char * name_c, struct hg3dclass_struct * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  Ogre::SceneNode * result_cpp;
  result_cpp = (thisclass_cpp->createSceneNode(name_cpp));
  *result_c = getHG3DClass_SceneNode((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_destroySceneNode(struct hg3dclass_struct * thisclass_c, char * name_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  (thisclass_cpp->destroySceneNode(name_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_destroySceneNode2(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * sn_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::SceneNode * sn_cpp = static_cast<Ogre::SceneNode*> (getHG3DClassPtr(*sn_c, "Ogre::SceneNode"));
  (thisclass_cpp->destroySceneNode(sn_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_getRootSceneNode(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::SceneNode * result_cpp;
  result_cpp = (thisclass_cpp->getRootSceneNode());
  *result_c = getHG3DClass_SceneNode((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_getSceneNode(struct hg3dclass_struct * thisclass_c, char * name_c, struct hg3dclass_struct * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  Ogre::SceneNode * result_cpp;
  result_cpp = (thisclass_cpp->getSceneNode(name_cpp));
  *result_c = getHG3DClass_SceneNode((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_hasSceneNode(struct hg3dclass_struct * thisclass_c, char * name_c, int * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  bool result_cpp;
  result_cpp = (thisclass_cpp->hasSceneNode(name_cpp));
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_createEntity(struct hg3dclass_struct * thisclass_c, char * entityName_c, char * meshName_c, char * groupName_c, struct hg3dclass_struct * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::String entityName_cpp = Ogre::String((const char*) entityName_c);
  Ogre::String meshName_cpp = Ogre::String((const char*) meshName_c);
  Ogre::String groupName_cpp = Ogre::String((const char*) groupName_c);
  Ogre::Entity * result_cpp;
  result_cpp = (thisclass_cpp->createEntity(entityName_cpp, meshName_cpp, groupName_cpp));
  *result_c = getHG3DClass_Entity((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_createEntity2(struct hg3dclass_struct * thisclass_c, char * entityName_c, struct sharedptr_struct * pMesh_c, struct hg3dclass_struct * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::String entityName_cpp = Ogre::String((const char*) entityName_c);
  MeshPtr pMesh_cpp = *((MeshPtr*) pMesh_c);
  Ogre::Entity * result_cpp;
  result_cpp = (thisclass_cpp->createEntity(entityName_cpp, pMesh_cpp));
  *result_c = getHG3DClass_Entity((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_createEntity3(struct hg3dclass_struct * thisclass_c, char * meshName_c, struct hg3dclass_struct * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::String meshName_cpp = Ogre::String((const char*) meshName_c);
  Ogre::Entity * result_cpp;
  result_cpp = (thisclass_cpp->createEntity(meshName_cpp));
  *result_c = getHG3DClass_Entity((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_createEntity4(struct hg3dclass_struct * thisclass_c, struct sharedptr_struct * pMesh_c, struct hg3dclass_struct * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  MeshPtr pMesh_cpp = *((MeshPtr*) pMesh_c);
  Ogre::Entity * result_cpp;
  result_cpp = (thisclass_cpp->createEntity(pMesh_cpp));
  *result_c = getHG3DClass_Entity((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_createEntity5(struct hg3dclass_struct * thisclass_c, char * entityName_c, enum EnumSceneManagerPrefabType ptype_c, struct hg3dclass_struct * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::String entityName_cpp = Ogre::String((const char*) entityName_c);
  enum Ogre::SceneManager::PrefabType ptype_cpp = (enum Ogre::SceneManager::PrefabType)ptype_c;
  Ogre::Entity * result_cpp;
  result_cpp = (thisclass_cpp->createEntity(entityName_cpp, ptype_cpp));
  *result_c = getHG3DClass_Entity((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_createEntity6(struct hg3dclass_struct * thisclass_c, enum EnumSceneManagerPrefabType ptype_c, struct hg3dclass_struct * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  enum Ogre::SceneManager::PrefabType ptype_cpp = (enum Ogre::SceneManager::PrefabType)ptype_c;
  Ogre::Entity * result_cpp;
  result_cpp = (thisclass_cpp->createEntity(ptype_cpp));
  *result_c = getHG3DClass_Entity((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_getEntity(struct hg3dclass_struct * thisclass_c, char * name_c, struct hg3dclass_struct * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  Ogre::Entity * result_cpp;
  result_cpp = (thisclass_cpp->getEntity(name_cpp));
  *result_c = getHG3DClass_Entity((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_hasEntity(struct hg3dclass_struct * thisclass_c, char * name_c, int * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  bool result_cpp;
  result_cpp = (thisclass_cpp->hasEntity(name_cpp));
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_destroyEntity(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * ent_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::Entity * ent_cpp = static_cast<Ogre::Entity*> (getHG3DClassPtr(*ent_c, "Ogre::Entity"));
  (thisclass_cpp->destroyEntity(ent_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_destroyEntity2(struct hg3dclass_struct * thisclass_c, char * name_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  (thisclass_cpp->destroyEntity(name_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_destroyAllEntities(struct hg3dclass_struct * thisclass_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  (thisclass_cpp->destroyAllEntities());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_createManualObject(struct hg3dclass_struct * thisclass_c, char * name_c, struct hg3dclass_struct * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  Ogre::ManualObject * result_cpp;
  result_cpp = (thisclass_cpp->createManualObject(name_cpp));
  *result_c = getHG3DClass_ManualObject((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_createManualObject2(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::ManualObject * result_cpp;
  result_cpp = (thisclass_cpp->createManualObject());
  *result_c = getHG3DClass_ManualObject((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_getManualObject(struct hg3dclass_struct * thisclass_c, char * name_c, struct hg3dclass_struct * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  Ogre::ManualObject * result_cpp;
  result_cpp = (thisclass_cpp->getManualObject(name_cpp));
  *result_c = getHG3DClass_ManualObject((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_hasManualObject(struct hg3dclass_struct * thisclass_c, char * name_c, int * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  bool result_cpp;
  result_cpp = (thisclass_cpp->hasManualObject(name_cpp));
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_destroyManualObject(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * obj_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::ManualObject * obj_cpp = static_cast<Ogre::ManualObject*> (getHG3DClassPtr(*obj_c, "Ogre::ManualObject"));
  (thisclass_cpp->destroyManualObject(obj_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_destroyManualObject2(struct hg3dclass_struct * thisclass_c, char * name_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  (thisclass_cpp->destroyManualObject(name_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_destroyAllManualObjects(struct hg3dclass_struct * thisclass_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  (thisclass_cpp->destroyAllManualObjects());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_createBillboardChain(struct hg3dclass_struct * thisclass_c, char * name_c, struct hg3dclass_struct * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  Ogre::BillboardChain * result_cpp;
  result_cpp = (thisclass_cpp->createBillboardChain(name_cpp));
  *result_c = getHG3DClass_BillboardChain((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_createBillboardChain2(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::BillboardChain * result_cpp;
  result_cpp = (thisclass_cpp->createBillboardChain());
  *result_c = getHG3DClass_BillboardChain((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_getBillboardChain(struct hg3dclass_struct * thisclass_c, char * name_c, struct hg3dclass_struct * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  Ogre::BillboardChain * result_cpp;
  result_cpp = (thisclass_cpp->getBillboardChain(name_cpp));
  *result_c = getHG3DClass_BillboardChain((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_hasBillboardChain(struct hg3dclass_struct * thisclass_c, char * name_c, int * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  bool result_cpp;
  result_cpp = (thisclass_cpp->hasBillboardChain(name_cpp));
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_destroyBillboardChain(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * obj_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::BillboardChain * obj_cpp = static_cast<Ogre::BillboardChain*> (getHG3DClassPtr(*obj_c, "Ogre::BillboardChain"));
  (thisclass_cpp->destroyBillboardChain(obj_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_destroyBillboardChain2(struct hg3dclass_struct * thisclass_c, char * name_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  (thisclass_cpp->destroyBillboardChain(name_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_destroyAllBillboardChains(struct hg3dclass_struct * thisclass_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  (thisclass_cpp->destroyAllBillboardChains());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_hasRibbonTrail(struct hg3dclass_struct * thisclass_c, char * name_c, int * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  bool result_cpp;
  result_cpp = (thisclass_cpp->hasRibbonTrail(name_cpp));
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_destroyRibbonTrail2(struct hg3dclass_struct * thisclass_c, char * name_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  (thisclass_cpp->destroyRibbonTrail(name_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_destroyAllRibbonTrails(struct hg3dclass_struct * thisclass_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  (thisclass_cpp->destroyAllRibbonTrails());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_hasParticleSystem(struct hg3dclass_struct * thisclass_c, char * name_c, int * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  bool result_cpp;
  result_cpp = (thisclass_cpp->hasParticleSystem(name_cpp));
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_destroyParticleSystem2(struct hg3dclass_struct * thisclass_c, char * name_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  (thisclass_cpp->destroyParticleSystem(name_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_destroyAllParticleSystems(struct hg3dclass_struct * thisclass_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  (thisclass_cpp->destroyAllParticleSystems());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_clearScene(struct hg3dclass_struct * thisclass_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  (thisclass_cpp->clearScene());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_setAmbientLight(struct hg3dclass_struct * thisclass_c, struct colourvalue_struct * colour_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  ColourValue colour_cpp = *((ColourValue*) colour_c);
  (thisclass_cpp->setAmbientLight(colour_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_getAmbientLight(struct hg3dclass_struct * thisclass_c, struct colourvalue_struct * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  ColourValue result_cpp;
  result_cpp = (thisclass_cpp->getAmbientLight());
  *result_c = *((struct colourvalue_struct*) &result_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_prepareWorldGeometry(struct hg3dclass_struct * thisclass_c, char * filename_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::String filename_cpp = Ogre::String((const char*) filename_c);
  (thisclass_cpp->prepareWorldGeometry(filename_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_setWorldGeometry(struct hg3dclass_struct * thisclass_c, char * filename_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::String filename_cpp = Ogre::String((const char*) filename_c);
  (thisclass_cpp->setWorldGeometry(filename_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_estimateWorldGeometry(struct hg3dclass_struct * thisclass_c, char * filename_c, int * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::String filename_cpp = Ogre::String((const char*) filename_c);
  size_t result_cpp;
  result_cpp = (thisclass_cpp->estimateWorldGeometry(filename_cpp));
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_hasOption(struct hg3dclass_struct * thisclass_c, char * strKey_c, int * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::String strKey_cpp = Ogre::String((const char*) strKey_c);
  bool result_cpp;
  result_cpp = (thisclass_cpp->hasOption(strKey_cpp));
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_setSkyPlaneEnabled(struct hg3dclass_struct * thisclass_c, int enable_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  bool enable_cpp = (bool)enable_c;
  (thisclass_cpp->setSkyPlaneEnabled(enable_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_isSkyPlaneEnabled(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isSkyPlaneEnabled());
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_getSkyPlaneNode(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::SceneNode * result_cpp;
  result_cpp = (thisclass_cpp->getSkyPlaneNode());
  *result_c = getHG3DClass_SceneNode((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_setSkyBox(struct hg3dclass_struct * thisclass_c, int enable_c, char * materialName_c, float distance_c, int drawFirst_c, struct quaternion_struct * orientation_c, char * groupName_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  bool enable_cpp = (bool)enable_c;
  Ogre::String materialName_cpp = Ogre::String((const char*) materialName_c);
  Real distance_cpp = (Real)distance_c;
  bool drawFirst_cpp = (bool)drawFirst_c;
  Quaternion orientation_cpp = *((Quaternion*) orientation_c);
  Ogre::String groupName_cpp = Ogre::String((const char*) groupName_c);
  (thisclass_cpp->setSkyBox(enable_cpp, materialName_cpp, distance_cpp, drawFirst_cpp, orientation_cpp, groupName_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_setSkyBoxEnabled(struct hg3dclass_struct * thisclass_c, int enable_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  bool enable_cpp = (bool)enable_c;
  (thisclass_cpp->setSkyBoxEnabled(enable_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_isSkyBoxEnabled(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isSkyBoxEnabled());
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_getSkyBoxNode(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::SceneNode * result_cpp;
  result_cpp = (thisclass_cpp->getSkyBoxNode());
  *result_c = getHG3DClass_SceneNode((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_setSkyDome(struct hg3dclass_struct * thisclass_c, int enable_c, char * materialName_c, float curvature_c, float tiling_c, float distance_c, int drawFirst_c, struct quaternion_struct * orientation_c, int xsegments_c, int ysegments_c, int ysegments_keep_c, char * groupName_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  bool enable_cpp = (bool)enable_c;
  Ogre::String materialName_cpp = Ogre::String((const char*) materialName_c);
  Real curvature_cpp = (Real)curvature_c;
  Real tiling_cpp = (Real)tiling_c;
  Real distance_cpp = (Real)distance_c;
  bool drawFirst_cpp = (bool)drawFirst_c;
  Quaternion orientation_cpp = *((Quaternion*) orientation_c);
  int xsegments_cpp = (int)xsegments_c;
  int ysegments_cpp = (int)ysegments_c;
  int ysegments_keep_cpp = (int)ysegments_keep_c;
  Ogre::String groupName_cpp = Ogre::String((const char*) groupName_c);
  (thisclass_cpp->setSkyDome(enable_cpp, materialName_cpp, curvature_cpp, tiling_cpp, distance_cpp, drawFirst_cpp, orientation_cpp, xsegments_cpp, ysegments_cpp, ysegments_keep_cpp, groupName_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_setSkyDomeEnabled(struct hg3dclass_struct * thisclass_c, int enable_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  bool enable_cpp = (bool)enable_c;
  (thisclass_cpp->setSkyDomeEnabled(enable_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_isSkyDomeEnabled(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isSkyDomeEnabled());
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_getSkyDomeNode(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::SceneNode * result_cpp;
  result_cpp = (thisclass_cpp->getSkyDomeNode());
  *result_c = getHG3DClass_SceneNode((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_getFogColour(struct hg3dclass_struct * thisclass_c, struct colourvalue_struct * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  ColourValue result_cpp;
  result_cpp = (thisclass_cpp->getFogColour());
  *result_c = *((struct colourvalue_struct*) &result_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_getFogStart(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Real result_cpp;
  result_cpp = (thisclass_cpp->getFogStart());
  *result_c = (float)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_getFogEnd(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Real result_cpp;
  result_cpp = (thisclass_cpp->getFogEnd());
  *result_c = (float)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_getFogDensity(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Real result_cpp;
  result_cpp = (thisclass_cpp->getFogDensity());
  *result_c = (float)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_createBillboardSet(struct hg3dclass_struct * thisclass_c, char * name_c, unsigned int poolSize_c, struct hg3dclass_struct * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  unsigned int poolSize_cpp = (unsigned int)poolSize_c;
  Ogre::BillboardSet * result_cpp;
  result_cpp = (thisclass_cpp->createBillboardSet(name_cpp, poolSize_cpp));
  *result_c = getHG3DClass_BillboardSet((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_createBillboardSet2(struct hg3dclass_struct * thisclass_c, unsigned int poolSize_c, struct hg3dclass_struct * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  unsigned int poolSize_cpp = (unsigned int)poolSize_c;
  Ogre::BillboardSet * result_cpp;
  result_cpp = (thisclass_cpp->createBillboardSet(poolSize_cpp));
  *result_c = getHG3DClass_BillboardSet((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_getBillboardSet(struct hg3dclass_struct * thisclass_c, char * name_c, struct hg3dclass_struct * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  Ogre::BillboardSet * result_cpp;
  result_cpp = (thisclass_cpp->getBillboardSet(name_cpp));
  *result_c = getHG3DClass_BillboardSet((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_hasBillboardSet(struct hg3dclass_struct * thisclass_c, char * name_c, int * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  bool result_cpp;
  result_cpp = (thisclass_cpp->hasBillboardSet(name_cpp));
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_destroyBillboardSet(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * set_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::BillboardSet * set_cpp = static_cast<Ogre::BillboardSet*> (getHG3DClassPtr(*set_c, "Ogre::BillboardSet"));
  (thisclass_cpp->destroyBillboardSet(set_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_destroyBillboardSet2(struct hg3dclass_struct * thisclass_c, char * name_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  (thisclass_cpp->destroyBillboardSet(name_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_destroyAllBillboardSets(struct hg3dclass_struct * thisclass_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  (thisclass_cpp->destroyAllBillboardSets());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_setDisplaySceneNodes(struct hg3dclass_struct * thisclass_c, int display_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  bool display_cpp = (bool)display_c;
  (thisclass_cpp->setDisplaySceneNodes(display_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_getDisplaySceneNodes(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->getDisplaySceneNodes());
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_createAnimation(struct hg3dclass_struct * thisclass_c, char * name_c, float length_c, struct hg3dclass_struct * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  Real length_cpp = (Real)length_c;
  Ogre::Animation * result_cpp;
  result_cpp = (thisclass_cpp->createAnimation(name_cpp, length_cpp));
  *result_c = getHG3DClass_Animation((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_getAnimation(struct hg3dclass_struct * thisclass_c, char * name_c, struct hg3dclass_struct * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  Ogre::Animation * result_cpp;
  result_cpp = (thisclass_cpp->getAnimation(name_cpp));
  *result_c = getHG3DClass_Animation((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_hasAnimation(struct hg3dclass_struct * thisclass_c, char * name_c, int * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  bool result_cpp;
  result_cpp = (thisclass_cpp->hasAnimation(name_cpp));
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_destroyAnimation(struct hg3dclass_struct * thisclass_c, char * name_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  (thisclass_cpp->destroyAnimation(name_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_destroyAllAnimations(struct hg3dclass_struct * thisclass_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  (thisclass_cpp->destroyAllAnimations());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_createAnimationState(struct hg3dclass_struct * thisclass_c, char * animName_c, struct hg3dclass_struct * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::String animName_cpp = Ogre::String((const char*) animName_c);
  Ogre::AnimationState * result_cpp;
  result_cpp = (thisclass_cpp->createAnimationState(animName_cpp));
  *result_c = getHG3DClass_AnimationState((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_getAnimationState(struct hg3dclass_struct * thisclass_c, char * animName_c, struct hg3dclass_struct * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::String animName_cpp = Ogre::String((const char*) animName_c);
  Ogre::AnimationState * result_cpp;
  result_cpp = (thisclass_cpp->getAnimationState(animName_cpp));
  *result_c = getHG3DClass_AnimationState((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_hasAnimationState(struct hg3dclass_struct * thisclass_c, char * name_c, int * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  bool result_cpp;
  result_cpp = (thisclass_cpp->hasAnimationState(name_cpp));
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_destroyAnimationState(struct hg3dclass_struct * thisclass_c, char * name_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  (thisclass_cpp->destroyAnimationState(name_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_destroyAllAnimationStates(struct hg3dclass_struct * thisclass_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  (thisclass_cpp->destroyAllAnimationStates());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_clearSpecialCaseRenderQueues(struct hg3dclass_struct * thisclass_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  (thisclass_cpp->clearSpecialCaseRenderQueues());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_setSpecialCaseRenderQueueMode(struct hg3dclass_struct * thisclass_c, enum EnumSceneManagerSpecialCaseRenderQueueMode mode_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  enum Ogre::SceneManager::SpecialCaseRenderQueueMode mode_cpp = (enum Ogre::SceneManager::SpecialCaseRenderQueueMode)mode_c;
  (thisclass_cpp->setSpecialCaseRenderQueueMode(mode_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_getSpecialCaseRenderQueueMode(struct hg3dclass_struct * thisclass_c, enum EnumSceneManagerSpecialCaseRenderQueueMode * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  enum Ogre::SceneManager::SpecialCaseRenderQueueMode result_cpp;
  result_cpp = (thisclass_cpp->getSpecialCaseRenderQueueMode());
  *result_c = (enum EnumSceneManagerSpecialCaseRenderQueueMode) result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_showBoundingBoxes(struct hg3dclass_struct * thisclass_c, int bShow_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  bool bShow_cpp = (bool)bShow_c;
  (thisclass_cpp->showBoundingBoxes(bShow_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_getShowBoundingBoxes(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->getShowBoundingBoxes());
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_setShowDebugShadows(struct hg3dclass_struct * thisclass_c, int debug_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  bool debug_cpp = (bool)debug_c;
  (thisclass_cpp->setShowDebugShadows(debug_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_getShowDebugShadows(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->getShowDebugShadows());
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_setShadowColour(struct hg3dclass_struct * thisclass_c, struct colourvalue_struct * colour_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  ColourValue colour_cpp = *((ColourValue*) colour_c);
  (thisclass_cpp->setShadowColour(colour_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_getShadowColour(struct hg3dclass_struct * thisclass_c, struct colourvalue_struct * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  ColourValue result_cpp;
  result_cpp = (thisclass_cpp->getShadowColour());
  *result_c = *((struct colourvalue_struct*) &result_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_setShadowDirectionalLightExtrusionDistance(struct hg3dclass_struct * thisclass_c, float dist_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Real dist_cpp = (Real)dist_c;
  (thisclass_cpp->setShadowDirectionalLightExtrusionDistance(dist_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_getShadowDirectionalLightExtrusionDistance(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Real result_cpp;
  result_cpp = (thisclass_cpp->getShadowDirectionalLightExtrusionDistance());
  *result_c = (float)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_setShadowFarDistance(struct hg3dclass_struct * thisclass_c, float distance_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Real distance_cpp = (Real)distance_c;
  (thisclass_cpp->setShadowFarDistance(distance_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_getShadowFarDistance(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Real result_cpp;
  result_cpp = (thisclass_cpp->getShadowFarDistance());
  *result_c = (float)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_getShadowFarDistanceSquared(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Real result_cpp;
  result_cpp = (thisclass_cpp->getShadowFarDistanceSquared());
  *result_c = (float)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_setShadowIndexBufferSize(struct hg3dclass_struct * thisclass_c, int size_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  size_t size_cpp = (size_t)size_c;
  (thisclass_cpp->setShadowIndexBufferSize(size_cpp));
};

// Get the size of the shadow index buffer. 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_getShadowIndexBufferSize(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  size_t result_cpp;
  result_cpp = (thisclass_cpp->getShadowIndexBufferSize());
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_setShadowTextureSize(struct hg3dclass_struct * thisclass_c, unsigned short size_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  unsigned short size_cpp = (unsigned short)size_c;
  (thisclass_cpp->setShadowTextureSize(size_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_setShadowTextureFSAA(struct hg3dclass_struct * thisclass_c, unsigned short fsaa_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  unsigned short fsaa_cpp = (unsigned short)fsaa_c;
  (thisclass_cpp->setShadowTextureFSAA(fsaa_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_setShadowTextureCount(struct hg3dclass_struct * thisclass_c, int count_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  size_t count_cpp = (size_t)count_c;
  (thisclass_cpp->setShadowTextureCount(count_cpp));
};

// Get the number of the textures allocated for texture based shadows. 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_getShadowTextureCount(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  size_t result_cpp;
  result_cpp = (thisclass_cpp->getShadowTextureCount());
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_setShadowTextureCountPerLightType(struct hg3dclass_struct * thisclass_c, enum EnumLightType type_c, int count_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  enum Light::LightTypes type_cpp = (enum Light::LightTypes)type_c;
  size_t count_cpp = (size_t)count_c;
  (thisclass_cpp->setShadowTextureCountPerLightType(type_cpp, count_cpp));
};

// Get the number of shadow textures is assigned for the given light type. 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_getShadowTextureCountPerLightType(struct hg3dclass_struct * thisclass_c, enum EnumLightType type_c, int * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  enum Light::LightTypes type_cpp = (enum Light::LightTypes)type_c;
  size_t result_cpp;
  result_cpp = (thisclass_cpp->getShadowTextureCountPerLightType(type_cpp));
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_getShadowTexture(struct hg3dclass_struct * thisclass_c, int shadowIndex_c, struct sharedptr_struct * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  size_t shadowIndex_cpp = (size_t)shadowIndex_c;
  TexturePtr result_cpp;
  result_cpp = (thisclass_cpp->getShadowTexture(shadowIndex_cpp));
  *result_c = *((struct sharedptr_struct*) &result_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_setShadowDirLightTextureOffset(struct hg3dclass_struct * thisclass_c, float offset_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Real offset_cpp = (Real)offset_c;
  (thisclass_cpp->setShadowDirLightTextureOffset(offset_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_getShadowDirLightTextureOffset(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Real result_cpp;
  result_cpp = (thisclass_cpp->getShadowDirLightTextureOffset());
  *result_c = (float)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_setShadowTextureFadeStart(struct hg3dclass_struct * thisclass_c, float fadeStart_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Real fadeStart_cpp = (Real)fadeStart_c;
  (thisclass_cpp->setShadowTextureFadeStart(fadeStart_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_setShadowTextureFadeEnd(struct hg3dclass_struct * thisclass_c, float fadeEnd_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Real fadeEnd_cpp = (Real)fadeEnd_c;
  (thisclass_cpp->setShadowTextureFadeEnd(fadeEnd_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_setShadowTextureSelfShadow(struct hg3dclass_struct * thisclass_c, int selfShadow_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  bool selfShadow_cpp = (bool)selfShadow_c;
  (thisclass_cpp->setShadowTextureSelfShadow(selfShadow_cpp));
};

// Gets whether or not texture shadows attempt to self-shadow. 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_getShadowTextureSelfShadow(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->getShadowTextureSelfShadow());
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_setShadowTextureCasterMaterial(struct hg3dclass_struct * thisclass_c, char * name_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  (thisclass_cpp->setShadowTextureCasterMaterial(name_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_setShadowTextureReceiverMaterial(struct hg3dclass_struct * thisclass_c, char * name_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  (thisclass_cpp->setShadowTextureReceiverMaterial(name_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_setShadowCasterRenderBackFaces(struct hg3dclass_struct * thisclass_c, int bf_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  bool bf_cpp = (bool)bf_c;
  (thisclass_cpp->setShadowCasterRenderBackFaces(bf_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_getShadowCasterRenderBackFaces(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->getShadowCasterRenderBackFaces());
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_setShadowUseInfiniteFarPlane(struct hg3dclass_struct * thisclass_c, int enable_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  bool enable_cpp = (bool)enable_c;
  (thisclass_cpp->setShadowUseInfiniteFarPlane(enable_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_isShadowTechniqueStencilBased(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isShadowTechniqueStencilBased());
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_isShadowTechniqueTextureBased(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isShadowTechniqueTextureBased());
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_isShadowTechniqueModulative(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isShadowTechniqueModulative());
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_isShadowTechniqueAdditive(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isShadowTechniqueAdditive());
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_isShadowTechniqueIntegrated(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isShadowTechniqueIntegrated());
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_isShadowTechniqueInUse(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isShadowTechniqueInUse());
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_setShadowUseLightClipPlanes(struct hg3dclass_struct * thisclass_c, int enabled_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  bool enabled_cpp = (bool)enabled_c;
  (thisclass_cpp->setShadowUseLightClipPlanes(enabled_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_getShadowUseLightClipPlanes(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->getShadowUseLightClipPlanes());
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_setLateMaterialResolving(struct hg3dclass_struct * thisclass_c, int isLate_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  bool isLate_cpp = (bool)isLate_c;
  (thisclass_cpp->setLateMaterialResolving(isLate_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_isLateMaterialResolving(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isLateMaterialResolving());
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_hasStaticGeometry(struct hg3dclass_struct * thisclass_c, char * name_c, int * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  bool result_cpp;
  result_cpp = (thisclass_cpp->hasStaticGeometry(name_cpp));
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_destroyStaticGeometry2(struct hg3dclass_struct * thisclass_c, char * name_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  (thisclass_cpp->destroyStaticGeometry(name_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_destroyAllStaticGeometry(struct hg3dclass_struct * thisclass_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  (thisclass_cpp->destroyAllStaticGeometry());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_destroyInstancedGeometry2(struct hg3dclass_struct * thisclass_c, char * name_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  (thisclass_cpp->destroyInstancedGeometry(name_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_destroyAllInstancedGeometry(struct hg3dclass_struct * thisclass_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  (thisclass_cpp->destroyAllInstancedGeometry());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_hasInstanceManager(struct hg3dclass_struct * thisclass_c, char * managerName_c, int * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::String managerName_cpp = Ogre::String((const char*) managerName_c);
  bool result_cpp;
  result_cpp = (thisclass_cpp->hasInstanceManager(managerName_cpp));
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_destroyInstanceManager(struct hg3dclass_struct * thisclass_c, char * name_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  (thisclass_cpp->destroyInstanceManager(name_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_destroyAllInstanceManagers(struct hg3dclass_struct * thisclass_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  (thisclass_cpp->destroyAllInstanceManagers());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_destroyMovableObject(struct hg3dclass_struct * thisclass_c, char * name_c, char * typeName_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  Ogre::String typeName_cpp = Ogre::String((const char*) typeName_c);
  (thisclass_cpp->destroyMovableObject(name_cpp, typeName_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_destroyMovableObject2(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * m_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::MovableObject * m_cpp = static_cast<Ogre::MovableObject*> (getHG3DClassPtr(*m_c, "Ogre::MovableObject"));
  (thisclass_cpp->destroyMovableObject(m_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_destroyAllMovableObjectsByType(struct hg3dclass_struct * thisclass_c, char * typeName_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::String typeName_cpp = Ogre::String((const char*) typeName_c);
  (thisclass_cpp->destroyAllMovableObjectsByType(typeName_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_destroyAllMovableObjects(struct hg3dclass_struct * thisclass_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  (thisclass_cpp->destroyAllMovableObjects());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_getMovableObject(struct hg3dclass_struct * thisclass_c, char * name_c, char * typeName_c, struct hg3dclass_struct * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  Ogre::String typeName_cpp = Ogre::String((const char*) typeName_c);
  Ogre::MovableObject * result_cpp;
  result_cpp = (thisclass_cpp->getMovableObject(name_cpp, typeName_cpp));
  *result_c = getHG3DClass_MovableObject((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_hasMovableObject(struct hg3dclass_struct * thisclass_c, char * name_c, char * typeName_c, int * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  Ogre::String typeName_cpp = Ogre::String((const char*) typeName_c);
  bool result_cpp;
  result_cpp = (thisclass_cpp->hasMovableObject(name_cpp, typeName_cpp));
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_injectMovableObject(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * m_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::MovableObject * m_cpp = static_cast<Ogre::MovableObject*> (getHG3DClassPtr(*m_c, "Ogre::MovableObject"));
  (thisclass_cpp->injectMovableObject(m_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_extractMovableObject(struct hg3dclass_struct * thisclass_c, char * name_c, char * typeName_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  Ogre::String typeName_cpp = Ogre::String((const char*) typeName_c);
  (thisclass_cpp->extractMovableObject(name_cpp, typeName_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_extractMovableObject2(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * m_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::MovableObject * m_cpp = static_cast<Ogre::MovableObject*> (getHG3DClassPtr(*m_c, "Ogre::MovableObject"));
  (thisclass_cpp->extractMovableObject(m_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_extractAllMovableObjectsByType(struct hg3dclass_struct * thisclass_c, char * typeName_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::String typeName_cpp = Ogre::String((const char*) typeName_c);
  (thisclass_cpp->extractAllMovableObjectsByType(typeName_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_setVisibilityMask(struct hg3dclass_struct * thisclass_c, unsigned int vmask_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  uint32 vmask_cpp = (uint32)vmask_c;
  (thisclass_cpp->setVisibilityMask(vmask_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_getVisibilityMask(struct hg3dclass_struct * thisclass_c, unsigned int * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  uint32 result_cpp;
  result_cpp = (thisclass_cpp->getVisibilityMask());
  *result_c = (unsigned int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_setFindVisibleObjects(struct hg3dclass_struct * thisclass_c, int find_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  bool find_cpp = (bool)find_c;
  (thisclass_cpp->setFindVisibleObjects(find_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_getFindVisibleObjects(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->getFindVisibleObjects());
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_setNormaliseNormalsOnScale(struct hg3dclass_struct * thisclass_c, int n_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  bool n_cpp = (bool)n_c;
  (thisclass_cpp->setNormaliseNormalsOnScale(n_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_getNormaliseNormalsOnScale(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->getNormaliseNormalsOnScale());
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_setFlipCullingOnNegativeScale(struct hg3dclass_struct * thisclass_c, int n_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  bool n_cpp = (bool)n_c;
  (thisclass_cpp->setFlipCullingOnNegativeScale(n_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_getFlipCullingOnNegativeScale(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->getFlipCullingOnNegativeScale());
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_getDestinationRenderSystem(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::RenderSystem * result_cpp;
  result_cpp = (thisclass_cpp->getDestinationRenderSystem());
  *result_c = getHG3DClass_RenderSystem((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_getCurrentViewport(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  Ogre::Viewport * result_cpp;
  result_cpp = (thisclass_cpp->getCurrentViewport());
  *result_c = getHG3DClass_Viewport((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_setCameraRelativeRendering(struct hg3dclass_struct * thisclass_c, int rel_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  bool rel_cpp = (bool)rel_c;
  (thisclass_cpp->setCameraRelativeRendering(rel_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_scmgr_getCameraRelativeRendering(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::SceneManager * thisclass_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManager"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->getCameraRelativeRendering());
  *result_c = (int)result_cpp;
};

