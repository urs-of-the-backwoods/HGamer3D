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

// ClassMaterialManager.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassMaterialManager
#define _DEFINED_HG3D_ClassMaterialManager

#include "ClassPtr.h"
#include "StructSharedPtr.h"


// 
void ogre_mtrlmgr_construct(struct hg3dclass_struct * result_c);

// 
void ogre_mtrlmgr_destruct(struct hg3dclass_struct * thisclass_c);

// 
void ogre_mtrlmgr_initialise(struct hg3dclass_struct * thisclass_c);

// 
void ogre_mtrlmgr_setDefaultAnisotropy(struct hg3dclass_struct * thisclass_c, unsigned long maxAniso_c);

// Get the default maxAnisotropy. 
void ogre_mtrlmgr_getDefaultAnisotropy(struct hg3dclass_struct * thisclass_c, unsigned long * result_c);

// 
void ogre_mtrlmgr_getDefaultSettings(struct hg3dclass_struct * thisclass_c, struct sharedptr_struct * result_c);

// 
void ogre_mtrlmgr_getActiveScheme(struct hg3dclass_struct * thisclass_c, char * result_c);

// 
void ogre_mtrlmgr_setActiveScheme(struct hg3dclass_struct * thisclass_c, char * schemeName_c);

// 
void ogre_mtrlmgr_getSingleton(struct hg3dclass_struct * result_c);

// 
void ogre_mtrlmgr_getSingletonPtr(struct hg3dclass_struct * result_c);

#endif 
