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

// ClassTabButton.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassTabButton
#define _DEFINED_HG3D_ClassTabButton

#include "ClassPtr.h"
#include "ClassWindow.h"


// Constructor for base TabButton
void cegui_tbbtn_construct(char * type_c, char * name_c, struct hg3dclass_struct * result_c);

// Destructor for TabButton
void cegui_tbbtn_destruct(struct hg3dclass_struct * thisclass_c);

// Set whether this tab button is selected or not. 
void cegui_tbbtn_setSelected(struct hg3dclass_struct * thisclass_c, int selected_c);

// Return whether this tab button is selected or not. 
void cegui_tbbtn_isSelected(struct hg3dclass_struct * thisclass_c, int * result_c);

// Set the target window which is the content pane which this button is covering. 
void cegui_tbbtn_setTargetWindow(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * wnd_c);

// Get the target window which is the content pane which this button is covering. 
void cegui_tbbtn_getTargetWindow(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

#endif 
