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

// ClassItemListbox.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassItemListbox
#define _DEFINED_HG3D_ClassItemListbox

#include "ClassPtr.h"
#include "ClassItemEntry.h"


// Returns the number of selected items in this ItemListbox
void cegui_itmlstbx_getSelectedCount(struct hg3dclass_struct * thisclass_c, int * result_c);

// Returns a pointer to the last selected item. 
void cegui_itmlstbx_getLastSelectedItem(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// Returns a pointer to the first selected item. 
void cegui_itmlstbx_getFirstSelectedItem(struct hg3dclass_struct * thisclass_c, int start_index_c, struct hg3dclass_struct * result_c);

// Returns a pointer to the next seleced item relative to a previous call to getFirstSelectedItem or getNextSelectedItem. 
void cegui_itmlstbx_getNextSelectedItem(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// Returns a pointer to the next selected item after the item 'start_item' given. 
void cegui_itmlstbx_getNextSelectedItemAfter(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * start_item_c, struct hg3dclass_struct * result_c);

// Returns 'true' if multiple selections are allowed. 'false' if not. 
void cegui_itmlstbx_isMultiSelectEnabled(struct hg3dclass_struct * thisclass_c, int * result_c);

// Returns 'true' if the item at the given index is selectable and currently selected. 
void cegui_itmlstbx_isItemSelected(struct hg3dclass_struct * thisclass_c, int index_c, int * result_c);

// 
void cegui_itmlstbx_initialiseComponents(struct hg3dclass_struct * thisclass_c);

// Set whether or not multiple selections should be allowed. 
void cegui_itmlstbx_setMultiSelectEnabled(struct hg3dclass_struct * thisclass_c, int state_c);

// Clears all selections. 
void cegui_itmlstbx_clearAllSelections(struct hg3dclass_struct * thisclass_c);

// Select a range of items. 
void cegui_itmlstbx_selectRange(struct hg3dclass_struct * thisclass_c, int a_c, int z_c);

// Select all items. Does nothing if multiselect is disabled. 
void cegui_itmlstbx_selectAllItems(struct hg3dclass_struct * thisclass_c);

// Constructor for the ItemListbox
void cegui_itmlstbx_construct(char * type_c, char * name_c, struct hg3dclass_struct * result_c);

// Destructor for the ItemListbox
void cegui_itmlstbx_destruct(struct hg3dclass_struct * thisclass_c);

// Setup size and position for the item widgets attached to this ItemListbox
void cegui_itmlstbx_layoutItemWidgets(struct hg3dclass_struct * thisclass_c);

// Return whether this window was inherited from the given class name at some point in the inheritance hierarchy. 
void cegui_itmlstbx_testClassName_impl(struct hg3dclass_struct * thisclass_c, char * class_name_c, int * result_c);

// Notify this ItemListbox
void cegui_itmlstbx_notifyItemClicked(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * li_c);

// Notify this ItemListbox
void cegui_itmlstbx_notifyItemSelectState(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * li_c, int state_c);

#endif 
