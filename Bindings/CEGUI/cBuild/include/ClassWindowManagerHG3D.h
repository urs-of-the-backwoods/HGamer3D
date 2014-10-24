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

// ClassWindowManagerHG3D.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassWindowManagerHG3D
#define _DEFINED_HG3D_ClassWindowManagerHG3D

#include "ClassPtr.h"
#include "ClassWindow.h"
#include "ClassWindowManager.h"


// 
void cegui_wmgr_hg3d_construct(struct hg3dclass_struct * result_c);

// 
void cegui_wmgr_hg3d_destruct(struct hg3dclass_struct * thisclass_c);

// 
void cegui_wmgr_hg3d_loadWindowLayoutHG3D(struct hg3dclass_struct * thisclass_c, char * filename_c, char * name_prefix_c, struct hg3dclass_struct * result_c);

// 
void cegui_wmgr_hg3d_getSingleton(struct hg3dclass_struct * result_c);

#endif 
