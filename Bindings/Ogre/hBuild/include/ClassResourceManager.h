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

// ClassResourceManager.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassResourceManager
#define _DEFINED_HG3D_ClassResourceManager

#include "ClassPtr.h"


// 
void ogre_rsrcmgr_destruct(struct hg3dclass_struct * thisclass_c);

// 
void ogre_rsrcmgr_setMemoryBudget(struct hg3dclass_struct * thisclass_c, int bytes_c);

// 
void ogre_rsrcmgr_getMemoryBudget(struct hg3dclass_struct * thisclass_c, int * result_c);

// 
void ogre_rsrcmgr_getMemoryUsage(struct hg3dclass_struct * thisclass_c, int * result_c);

// 
void ogre_rsrcmgr_unload(struct hg3dclass_struct * thisclass_c, char * name_c);

// 
void ogre_rsrcmgr_unloadAll(struct hg3dclass_struct * thisclass_c, int reloadableOnly_c);

// 
void ogre_rsrcmgr_reloadAll(struct hg3dclass_struct * thisclass_c, int reloadableOnly_c);

// 
void ogre_rsrcmgr_unloadUnreferencedResources(struct hg3dclass_struct * thisclass_c, int reloadableOnly_c);

// 
void ogre_rsrcmgr_reloadUnreferencedResources(struct hg3dclass_struct * thisclass_c, int reloadableOnly_c);

// 
void ogre_rsrcmgr_remove2(struct hg3dclass_struct * thisclass_c, char * name_c);

// 
void ogre_rsrcmgr_removeAll(struct hg3dclass_struct * thisclass_c);

// 
void ogre_rsrcmgr_removeUnreferencedResources(struct hg3dclass_struct * thisclass_c, int reloadableOnly_c);

// Returns whether the named resource exists in this manager. 
void ogre_rsrcmgr_resourceExists(struct hg3dclass_struct * thisclass_c, char * name_c, int * result_c);

// 
void ogre_rsrcmgr_getLoadingOrder(struct hg3dclass_struct * thisclass_c, float * result_c);

// 
void ogre_rsrcmgr_getResourceType(struct hg3dclass_struct * thisclass_c, char * result_c);

// 
void ogre_rsrcmgr_setVerbose(struct hg3dclass_struct * thisclass_c, int v_c);

// 
void ogre_rsrcmgr_getVerbose(struct hg3dclass_struct * thisclass_c, int * result_c);

// Destroy a resource pool. 
void ogre_rsrcmgr_destroyResourcePool2(struct hg3dclass_struct * thisclass_c, char * name_c);

// destroy all pools 
void ogre_rsrcmgr_destroyAllResourcePools(struct hg3dclass_struct * thisclass_c);

#endif 
