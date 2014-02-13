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

// ClassScheme.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassScheme
#define _DEFINED_HG3D_ClassScheme

#include "ClassPtr.h"


// Loads all resources for this scheme. 
void cegui_schm_loadResources(struct hg3dclass_struct * thisclass_c);

// Unloads all resources for this scheme. This should be used very carefully. 
void cegui_schm_unloadResources(struct hg3dclass_struct * thisclass_c);

// Return whether the resources for this Scheme
void cegui_schm_resourcesLoaded(struct hg3dclass_struct * thisclass_c, int * result_c);

// Return the name of this Scheme
void cegui_schm_getName(struct hg3dclass_struct * thisclass_c, char * result_c);

// Destroys a Scheme
void cegui_schm_destruct(struct hg3dclass_struct * thisclass_c);

// Returns the default resource group currently set for Schemes. 
void cegui_schm_getDefaultResourceGroup(char * result_c);

// Sets the default resource group to be used when loading scheme xml data. 
void cegui_schm_setDefaultResourceGroup(char * resourceGroup_c);

#endif 
