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

// ClassItemEntry.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassItemEntry
#define _DEFINED_HG3D_ClassItemEntry

#include "ClassPtr.h"


// Returns whether this item is selected or not. 
void cegui_itmentr_isSelected(struct hg3dclass_struct * thisclass_c, int * result_c);

// Returns whether this item is selectable or not. 
void cegui_itmentr_isSelectable(struct hg3dclass_struct * thisclass_c, int * result_c);

// Sets the selection state of this item (on/off). If this item is not selectable this function does nothing. 
void cegui_itmentr_setSelected(struct hg3dclass_struct * thisclass_c, int setting_c);

// Selects the item. 
void cegui_itmentr_select(struct hg3dclass_struct * thisclass_c);

// Deselects the item. 
void cegui_itmentr_deselect(struct hg3dclass_struct * thisclass_c);

// Set the selection state for this ListItem. Internal version. Should NOT be used by client code. 
void cegui_itmentr_setSelected_impl(struct hg3dclass_struct * thisclass_c, int state_c, int notify_c);

// Sets whether this item will be selectable. 
void cegui_itmentr_setSelectable(struct hg3dclass_struct * thisclass_c, int setting_c);

// Constructor for ItemEntry
void cegui_itmentr_construct(char * type_c, char * name_c, struct hg3dclass_struct * result_c);

// Destructor for ItemEntry
void cegui_itmentr_destruct(struct hg3dclass_struct * thisclass_c);

#endif 
