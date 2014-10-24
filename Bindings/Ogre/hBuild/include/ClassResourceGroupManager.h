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

// ClassResourceGroupManager.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassResourceGroupManager
#define _DEFINED_HG3D_ClassResourceGroupManager

#include "ClassPtr.h"
#include "ClassSceneManager.h"


// 
void ogre_rgmgr_construct(struct hg3dclass_struct * result_c);

// 
void ogre_rgmgr_destruct(struct hg3dclass_struct * thisclass_c);

// 
void ogre_rgmgr_createResourceGroup(struct hg3dclass_struct * thisclass_c, char * name_c, const long inGlobalPool_c);

// 
void ogre_rgmgr_initialiseResourceGroup(struct hg3dclass_struct * thisclass_c, char * name_c);

// 
void ogre_rgmgr_initialiseAllResourceGroups(struct hg3dclass_struct * thisclass_c);

// 
void ogre_rgmgr_prepareResourceGroup(struct hg3dclass_struct * thisclass_c, char * name_c, long prepareMainResources_c, long prepareWorldGeom_c);

// 
void ogre_rgmgr_loadResourceGroup(struct hg3dclass_struct * thisclass_c, char * name_c, long loadMainResources_c, long loadWorldGeom_c);

// 
void ogre_rgmgr_unloadResourceGroup(struct hg3dclass_struct * thisclass_c, char * name_c, long reloadableOnly_c);

// 
void ogre_rgmgr_unloadUnreferencedResourcesInGroup(struct hg3dclass_struct * thisclass_c, char * name_c, long reloadableOnly_c);

// 
void ogre_rgmgr_clearResourceGroup(struct hg3dclass_struct * thisclass_c, char * name_c);

// 
void ogre_rgmgr_destroyResourceGroup(struct hg3dclass_struct * thisclass_c, char * name_c);

// 
void ogre_rgmgr_isResourceGroupInitialised(struct hg3dclass_struct * thisclass_c, char * name_c, long * result_c);

// 
void ogre_rgmgr_isResourceGroupLoaded(struct hg3dclass_struct * thisclass_c, char * name_c, long * result_c);

// 
void ogre_rgmgr_resourceGroupExists(struct hg3dclass_struct * thisclass_c, char * name_c, long * result_c);

// 
void ogre_rgmgr_addResourceLocation(struct hg3dclass_struct * thisclass_c, char * name_c, char * locType_c, char * resGroup_c, long recursive_c);

// 
void ogre_rgmgr_removeResourceLocation(struct hg3dclass_struct * thisclass_c, char * name_c, char * resGroup_c);

// 
void ogre_rgmgr_resourceLocationExists(struct hg3dclass_struct * thisclass_c, char * name_c, char * resGroup_c, long * result_c);

// 
void ogre_rgmgr_undeclareResource(struct hg3dclass_struct * thisclass_c, char * name_c, char * groupName_c);

// 
void ogre_rgmgr_resourceExists(struct hg3dclass_struct * thisclass_c, char * group_c, char * filename_c, long * result_c);

// 
void ogre_rgmgr_resourceExistsInAnyGroup(struct hg3dclass_struct * thisclass_c, char * filename_c, long * result_c);

// 
void ogre_rgmgr_findGroupContainingResource(struct hg3dclass_struct * thisclass_c, char * filename_c, char * result_c);

// 
void ogre_rgmgr_deleteResource(struct hg3dclass_struct * thisclass_c, char * filename_c, char * groupName_c, char * locationPattern_c);

// 
void ogre_rgmgr_deleteMatchingResources(struct hg3dclass_struct * thisclass_c, char * filePattern_c, char * groupName_c, char * locationPattern_c);

// 
void ogre_rgmgr_setWorldResourceGroupName(struct hg3dclass_struct * thisclass_c, char * groupName_c);

// Gets the resource group that 'world' resources will use. 
void ogre_rgmgr_getWorldResourceGroupName(struct hg3dclass_struct * thisclass_c, char * result_c);

// 
void ogre_rgmgr_linkWorldGeometryToResourceGroup(struct hg3dclass_struct * thisclass_c, char * group_c, char * worldGeometry_c, struct hg3dclass_struct * sceneManager_c);

// 
void ogre_rgmgr_unlinkWorldGeometryFromResourceGroup(struct hg3dclass_struct * thisclass_c, char * group_c);

// 
void ogre_rgmgr_isResourceGroupInGlobalPool(struct hg3dclass_struct * thisclass_c, char * name_c, long * result_c);

// 
void ogre_rgmgr_shutdownAll(struct hg3dclass_struct * thisclass_c);

// 
void ogre_rgmgr_getSingleton(struct hg3dclass_struct * result_c);

// 
void ogre_rgmgr_getSingletonPtr(struct hg3dclass_struct * result_c);

#endif 
