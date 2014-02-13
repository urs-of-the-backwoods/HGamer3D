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

// ClassDefaultResourceProvider.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassDefaultResourceProvider
#define _DEFINED_HG3D_ClassDefaultResourceProvider

#include "ClassPtr.h"


// 
void cegui_drpr_construct(struct hg3dclass_struct * result_c);

// 
void cegui_drpr_destruct(struct hg3dclass_struct * thisclass_c);

// Set the directory associated with a given resource group identifier. 
void cegui_drpr_setResourceGroupDirectory(struct hg3dclass_struct * thisclass_c, char * resourceGroup_c, char * directory_c);

// Return the directory associated with the specified resource group identifier. 
void cegui_drpr_getResourceGroupDirectory(struct hg3dclass_struct * thisclass_c, char * resourceGroup_c, char * result_c);

// clears any currently set directory for the specified resource group identifier. 
void cegui_drpr_clearResourceGroupDirectory(struct hg3dclass_struct * thisclass_c, char * resourceGroup_c);

#endif 
