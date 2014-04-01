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

// ClassResourceGroupManager.cpp

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
	#include "./Ogre.h"
#include "./OgreString.h"
#include "./WindowUtilsHG3D.h"
#include "HG3DUtilities.h"

using namespace Ogre;
using Ogre::uint;



// 
extern "C" Ogre_LIB_EXPORT void ogre_rgmgr_construct(struct hg3dclass_struct * result_c)
{
  Ogre::ResourceGroupManager * result_cpp;
  result_cpp = (new Ogre::ResourceGroupManager());
  *result_c = getHG3DClass_ResourceGroupManager((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rgmgr_destruct(struct hg3dclass_struct * thisclass_c)
{
  Ogre::ResourceGroupManager * thisclass_cpp = static_cast<Ogre::ResourceGroupManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::ResourceGroupManager"));
  (delete thisclass_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rgmgr_createResourceGroup(struct hg3dclass_struct * thisclass_c, char * name_c, const int inGlobalPool_c)
{
  Ogre::ResourceGroupManager * thisclass_cpp = static_cast<Ogre::ResourceGroupManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::ResourceGroupManager"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  const bool inGlobalPool_cpp = (bool)inGlobalPool_c;
  (thisclass_cpp->createResourceGroup(name_cpp, inGlobalPool_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rgmgr_initialiseResourceGroup(struct hg3dclass_struct * thisclass_c, char * name_c)
{
  Ogre::ResourceGroupManager * thisclass_cpp = static_cast<Ogre::ResourceGroupManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::ResourceGroupManager"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  (thisclass_cpp->initialiseResourceGroup(name_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rgmgr_initialiseAllResourceGroups(struct hg3dclass_struct * thisclass_c)
{
  Ogre::ResourceGroupManager * thisclass_cpp = static_cast<Ogre::ResourceGroupManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::ResourceGroupManager"));
  (thisclass_cpp->initialiseAllResourceGroups());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rgmgr_prepareResourceGroup(struct hg3dclass_struct * thisclass_c, char * name_c, int prepareMainResources_c, int prepareWorldGeom_c)
{
  Ogre::ResourceGroupManager * thisclass_cpp = static_cast<Ogre::ResourceGroupManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::ResourceGroupManager"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  bool prepareMainResources_cpp = (bool)prepareMainResources_c;
  bool prepareWorldGeom_cpp = (bool)prepareWorldGeom_c;
  (thisclass_cpp->prepareResourceGroup(name_cpp, prepareMainResources_cpp, prepareWorldGeom_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rgmgr_loadResourceGroup(struct hg3dclass_struct * thisclass_c, char * name_c, int loadMainResources_c, int loadWorldGeom_c)
{
  Ogre::ResourceGroupManager * thisclass_cpp = static_cast<Ogre::ResourceGroupManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::ResourceGroupManager"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  bool loadMainResources_cpp = (bool)loadMainResources_c;
  bool loadWorldGeom_cpp = (bool)loadWorldGeom_c;
  (thisclass_cpp->loadResourceGroup(name_cpp, loadMainResources_cpp, loadWorldGeom_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rgmgr_unloadResourceGroup(struct hg3dclass_struct * thisclass_c, char * name_c, int reloadableOnly_c)
{
  Ogre::ResourceGroupManager * thisclass_cpp = static_cast<Ogre::ResourceGroupManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::ResourceGroupManager"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  bool reloadableOnly_cpp = (bool)reloadableOnly_c;
  (thisclass_cpp->unloadResourceGroup(name_cpp, reloadableOnly_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rgmgr_unloadUnreferencedResourcesInGroup(struct hg3dclass_struct * thisclass_c, char * name_c, int reloadableOnly_c)
{
  Ogre::ResourceGroupManager * thisclass_cpp = static_cast<Ogre::ResourceGroupManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::ResourceGroupManager"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  bool reloadableOnly_cpp = (bool)reloadableOnly_c;
  (thisclass_cpp->unloadUnreferencedResourcesInGroup(name_cpp, reloadableOnly_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rgmgr_clearResourceGroup(struct hg3dclass_struct * thisclass_c, char * name_c)
{
  Ogre::ResourceGroupManager * thisclass_cpp = static_cast<Ogre::ResourceGroupManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::ResourceGroupManager"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  (thisclass_cpp->clearResourceGroup(name_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rgmgr_destroyResourceGroup(struct hg3dclass_struct * thisclass_c, char * name_c)
{
  Ogre::ResourceGroupManager * thisclass_cpp = static_cast<Ogre::ResourceGroupManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::ResourceGroupManager"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  (thisclass_cpp->destroyResourceGroup(name_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rgmgr_isResourceGroupInitialised(struct hg3dclass_struct * thisclass_c, char * name_c, int * result_c)
{
  Ogre::ResourceGroupManager * thisclass_cpp = static_cast<Ogre::ResourceGroupManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::ResourceGroupManager"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  bool result_cpp;
  result_cpp = (thisclass_cpp->isResourceGroupInitialised(name_cpp));
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rgmgr_isResourceGroupLoaded(struct hg3dclass_struct * thisclass_c, char * name_c, int * result_c)
{
  Ogre::ResourceGroupManager * thisclass_cpp = static_cast<Ogre::ResourceGroupManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::ResourceGroupManager"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  bool result_cpp;
  result_cpp = (thisclass_cpp->isResourceGroupLoaded(name_cpp));
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rgmgr_resourceGroupExists(struct hg3dclass_struct * thisclass_c, char * name_c, int * result_c)
{
  Ogre::ResourceGroupManager * thisclass_cpp = static_cast<Ogre::ResourceGroupManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::ResourceGroupManager"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  bool result_cpp;
  result_cpp = (thisclass_cpp->resourceGroupExists(name_cpp));
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rgmgr_addResourceLocation(struct hg3dclass_struct * thisclass_c, char * name_c, char * locType_c, char * resGroup_c, int recursive_c)
{
  Ogre::ResourceGroupManager * thisclass_cpp = static_cast<Ogre::ResourceGroupManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::ResourceGroupManager"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  Ogre::String locType_cpp = Ogre::String((const char*) locType_c);
  Ogre::String resGroup_cpp = Ogre::String((const char*) resGroup_c);
  bool recursive_cpp = (bool)recursive_c;
  (thisclass_cpp->addResourceLocation(name_cpp, locType_cpp, resGroup_cpp, recursive_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rgmgr_removeResourceLocation(struct hg3dclass_struct * thisclass_c, char * name_c, char * resGroup_c)
{
  Ogre::ResourceGroupManager * thisclass_cpp = static_cast<Ogre::ResourceGroupManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::ResourceGroupManager"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  Ogre::String resGroup_cpp = Ogre::String((const char*) resGroup_c);
  (thisclass_cpp->removeResourceLocation(name_cpp, resGroup_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rgmgr_resourceLocationExists(struct hg3dclass_struct * thisclass_c, char * name_c, char * resGroup_c, int * result_c)
{
  Ogre::ResourceGroupManager * thisclass_cpp = static_cast<Ogre::ResourceGroupManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::ResourceGroupManager"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  Ogre::String resGroup_cpp = Ogre::String((const char*) resGroup_c);
  bool result_cpp;
  result_cpp = (thisclass_cpp->resourceLocationExists(name_cpp, resGroup_cpp));
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rgmgr_undeclareResource(struct hg3dclass_struct * thisclass_c, char * name_c, char * groupName_c)
{
  Ogre::ResourceGroupManager * thisclass_cpp = static_cast<Ogre::ResourceGroupManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::ResourceGroupManager"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  Ogre::String groupName_cpp = Ogre::String((const char*) groupName_c);
  (thisclass_cpp->undeclareResource(name_cpp, groupName_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rgmgr_resourceExists(struct hg3dclass_struct * thisclass_c, char * group_c, char * filename_c, int * result_c)
{
  Ogre::ResourceGroupManager * thisclass_cpp = static_cast<Ogre::ResourceGroupManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::ResourceGroupManager"));
  Ogre::String group_cpp = Ogre::String((const char*) group_c);
  Ogre::String filename_cpp = Ogre::String((const char*) filename_c);
  bool result_cpp;
  result_cpp = (thisclass_cpp->resourceExists(group_cpp, filename_cpp));
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rgmgr_resourceExistsInAnyGroup(struct hg3dclass_struct * thisclass_c, char * filename_c, int * result_c)
{
  Ogre::ResourceGroupManager * thisclass_cpp = static_cast<Ogre::ResourceGroupManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::ResourceGroupManager"));
  Ogre::String filename_cpp = Ogre::String((const char*) filename_c);
  bool result_cpp;
  result_cpp = (thisclass_cpp->resourceExistsInAnyGroup(filename_cpp));
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rgmgr_findGroupContainingResource(struct hg3dclass_struct * thisclass_c, char * filename_c, char * result_c)
{
  Ogre::ResourceGroupManager * thisclass_cpp = static_cast<Ogre::ResourceGroupManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::ResourceGroupManager"));
  Ogre::String filename_cpp = Ogre::String((const char*) filename_c);
  Ogre::String result_cpp;
  result_cpp = (thisclass_cpp->findGroupContainingResource(filename_cpp));
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rgmgr_deleteResource(struct hg3dclass_struct * thisclass_c, char * filename_c, char * groupName_c, char * locationPattern_c)
{
  Ogre::ResourceGroupManager * thisclass_cpp = static_cast<Ogre::ResourceGroupManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::ResourceGroupManager"));
  Ogre::String filename_cpp = Ogre::String((const char*) filename_c);
  Ogre::String groupName_cpp = Ogre::String((const char*) groupName_c);
  Ogre::String locationPattern_cpp = Ogre::String((const char*) locationPattern_c);
  (thisclass_cpp->deleteResource(filename_cpp, groupName_cpp, locationPattern_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rgmgr_deleteMatchingResources(struct hg3dclass_struct * thisclass_c, char * filePattern_c, char * groupName_c, char * locationPattern_c)
{
  Ogre::ResourceGroupManager * thisclass_cpp = static_cast<Ogre::ResourceGroupManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::ResourceGroupManager"));
  Ogre::String filePattern_cpp = Ogre::String((const char*) filePattern_c);
  Ogre::String groupName_cpp = Ogre::String((const char*) groupName_c);
  Ogre::String locationPattern_cpp = Ogre::String((const char*) locationPattern_c);
  (thisclass_cpp->deleteMatchingResources(filePattern_cpp, groupName_cpp, locationPattern_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rgmgr_setWorldResourceGroupName(struct hg3dclass_struct * thisclass_c, char * groupName_c)
{
  Ogre::ResourceGroupManager * thisclass_cpp = static_cast<Ogre::ResourceGroupManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::ResourceGroupManager"));
  Ogre::String groupName_cpp = Ogre::String((const char*) groupName_c);
  (thisclass_cpp->setWorldResourceGroupName(groupName_cpp));
};

// Gets the resource group that 'world' resources will use. 
extern "C" Ogre_LIB_EXPORT void ogre_rgmgr_getWorldResourceGroupName(struct hg3dclass_struct * thisclass_c, char * result_c)
{
  Ogre::ResourceGroupManager * thisclass_cpp = static_cast<Ogre::ResourceGroupManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::ResourceGroupManager"));
  Ogre::String result_cpp;
  result_cpp = (thisclass_cpp->getWorldResourceGroupName());
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rgmgr_linkWorldGeometryToResourceGroup(struct hg3dclass_struct * thisclass_c, char * group_c, char * worldGeometry_c, struct hg3dclass_struct * sceneManager_c)
{
  Ogre::ResourceGroupManager * thisclass_cpp = static_cast<Ogre::ResourceGroupManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::ResourceGroupManager"));
  Ogre::String group_cpp = Ogre::String((const char*) group_c);
  Ogre::String worldGeometry_cpp = Ogre::String((const char*) worldGeometry_c);
  Ogre::SceneManager * sceneManager_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*sceneManager_c, "Ogre::SceneManager"));
  (thisclass_cpp->linkWorldGeometryToResourceGroup(group_cpp, worldGeometry_cpp, sceneManager_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rgmgr_unlinkWorldGeometryFromResourceGroup(struct hg3dclass_struct * thisclass_c, char * group_c)
{
  Ogre::ResourceGroupManager * thisclass_cpp = static_cast<Ogre::ResourceGroupManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::ResourceGroupManager"));
  Ogre::String group_cpp = Ogre::String((const char*) group_c);
  (thisclass_cpp->unlinkWorldGeometryFromResourceGroup(group_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rgmgr_isResourceGroupInGlobalPool(struct hg3dclass_struct * thisclass_c, char * name_c, int * result_c)
{
  Ogre::ResourceGroupManager * thisclass_cpp = static_cast<Ogre::ResourceGroupManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::ResourceGroupManager"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  bool result_cpp;
  result_cpp = (thisclass_cpp->isResourceGroupInGlobalPool(name_cpp));
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rgmgr_shutdownAll(struct hg3dclass_struct * thisclass_c)
{
  Ogre::ResourceGroupManager * thisclass_cpp = static_cast<Ogre::ResourceGroupManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::ResourceGroupManager"));
  (thisclass_cpp->shutdownAll());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rgmgr_getSingleton(struct hg3dclass_struct * result_c)
{
  Ogre::ResourceGroupManager * result_cpp;
  result_cpp = &(Ogre::ResourceGroupManager::getSingleton());
  *result_c = getHG3DClass_ResourceGroupManager((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rgmgr_getSingletonPtr(struct hg3dclass_struct * result_c)
{
  Ogre::ResourceGroupManager * result_cpp;
  result_cpp = (Ogre::ResourceGroupManager::getSingletonPtr());
  *result_c = getHG3DClass_ResourceGroupManager((void *) result_cpp);
;
};

