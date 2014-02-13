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

// ClassComboDropList.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassComboDropList
#define _DEFINED_HG3D_ClassComboDropList

#include "ClassPtr.h"


// Initialise the Window
void cegui_cmbdrplst_initialiseComponents(struct hg3dclass_struct * thisclass_c);

// Set whether the drop-list is 'armed' for selection. 
void cegui_cmbdrplst_setArmed(struct hg3dclass_struct * thisclass_c, int setting_c);

// Return the 'armed' state of the ComboDropList
void cegui_cmbdrplst_isArmed(struct hg3dclass_struct * thisclass_c, int * result_c);

// Set the mode of operation for the ComboDropList
void cegui_cmbdrplst_setAutoArmEnabled(struct hg3dclass_struct * thisclass_c, int setting_c);

// returns the mode of operation for the drop-list 
void cegui_cmbdrplst_isAutoArmEnabled(struct hg3dclass_struct * thisclass_c, int * result_c);

// Constructor for ComboDropList
void cegui_cmbdrplst_construct(char * type_c, char * name_c, struct hg3dclass_struct * result_c);

// Destructor for ComboDropList
void cegui_cmbdrplst_destruct(struct hg3dclass_struct * thisclass_c);

#endif 
