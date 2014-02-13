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

// ClassManualObjectSection.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassManualObjectSection
#define _DEFINED_HG3D_ClassManualObjectSection

#include "ClassPtr.h"
#include "ClassManualObject.h"
#include "EnumRenderOperationOperationType.h"
#include "StructSharedPtr.h"
#include "ClassCamera.h"


// 
void ogre_mnos_construct(struct hg3dclass_struct * parent_c, char * materialName_c, enum EnumRenderOperationOperationType opType_c, char * groupName_c, struct hg3dclass_struct * result_c);

// 
void ogre_mnos_destruct(struct hg3dclass_struct * thisclass_c);

// Retrieve the material name in use. 
void ogre_mnos_getMaterialName(struct hg3dclass_struct * thisclass_c, char * result_c);

// Retrieve the material group in use. 
void ogre_mnos_getMaterialGroup(struct hg3dclass_struct * thisclass_c, char * result_c);

// update the material name in use 
void ogre_mnos_setMaterialName(struct hg3dclass_struct * thisclass_c, char * name_c, char * groupName_c);

// Set whether we need 32-bit indices. 
void ogre_mnos_set32BitIndices(struct hg3dclass_struct * thisclass_c, int n32_c);

// Get whether we need 32-bit indices. 
void ogre_mnos_get32BitIndices(struct hg3dclass_struct * thisclass_c, int * result_c);

// . 
void ogre_mnos_getMaterial(struct hg3dclass_struct * thisclass_c, struct sharedptr_struct * result_c);

// . 
void ogre_mnos_getSquaredViewDepth(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * _c, float * result_c);

#endif 
