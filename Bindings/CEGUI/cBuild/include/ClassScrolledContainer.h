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

// ClassScrolledContainer.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassScrolledContainer
#define _DEFINED_HG3D_ClassScrolledContainer

#include "ClassPtr.h"


// Constructor for ScrolledContainer
void cegui_scrlcnt_construct(char * type_c, char * name_c, struct hg3dclass_struct * result_c);

// Destructor for ScrolledContainer
void cegui_scrlcnt_destruct(struct hg3dclass_struct * thisclass_c);

// Return whether the content pane is auto sized. 
void cegui_scrlcnt_isContentPaneAutoSized(struct hg3dclass_struct * thisclass_c, int * result_c);

// Set whether the content pane should be auto-sized. 
void cegui_scrlcnt_setContentPaneAutoSized(struct hg3dclass_struct * thisclass_c, int setting_c);

#endif 
