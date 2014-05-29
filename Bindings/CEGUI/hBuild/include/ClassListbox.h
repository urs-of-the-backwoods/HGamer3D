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

// ClassListbox.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassListbox
#define _DEFINED_HG3D_ClassListbox

#include "ClassPtr.h"
#include "ClassListboxItem.h"
#include "ClassScrollbar.h"


// Return number of items attached to the list box. 
void cegui_lstbx_getItemCount(struct hg3dclass_struct * thisclass_c, int * result_c);

// Return the number of selected items in the list box. 
void cegui_lstbx_getSelectedCount(struct hg3dclass_struct * thisclass_c, int * result_c);

// Return a pointer to the first selected item. 
void cegui_lstbx_getFirstSelectedItem(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// Return a pointer to the next selected item after item start_item
void cegui_lstbx_getNextSelected(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * start_item_c, struct hg3dclass_struct * result_c);

// Return the item at index position index
void cegui_lstbx_getListboxItemFromIndex(struct hg3dclass_struct * thisclass_c, int index_c, struct hg3dclass_struct * result_c);

// Return the index of ListboxItemitem
void cegui_lstbx_getItemIndex(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * item_c, int * result_c);

// return whether list sorting is enabled 
void cegui_lstbx_isSortEnabled(struct hg3dclass_struct * thisclass_c, int * result_c);

// return whether multi-select is enabled 
void cegui_lstbx_isMultiselectEnabled(struct hg3dclass_struct * thisclass_c, int * result_c);

// 
void cegui_lstbx_isItemTooltipsEnabled(struct hg3dclass_struct * thisclass_c, int * result_c);

// return whether the string at index position index
void cegui_lstbx_isItemSelected(struct hg3dclass_struct * thisclass_c, int index_c, int * result_c);

// Search the list for an item with the specified text. 
void cegui_lstbx_findItemWithText(struct hg3dclass_struct * thisclass_c, char * text_c, struct hg3dclass_struct * start_item_c, struct hg3dclass_struct * result_c);

// Return whether the specified ListboxItem
void cegui_lstbx_isListboxItemInList(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * item_c, int * result_c);

// Return whether the vertical scroll bar is always shown. 
void cegui_lstbx_isVertScrollbarAlwaysShown(struct hg3dclass_struct * thisclass_c, int * result_c);

// Return whether the horizontal scroll bar is always shown. 
void cegui_lstbx_isHorzScrollbarAlwaysShown(struct hg3dclass_struct * thisclass_c, int * result_c);

// Initialise the Window
void cegui_lstbx_initialiseComponents(struct hg3dclass_struct * thisclass_c);

// Remove all items from the list. 
void cegui_lstbx_resetList(struct hg3dclass_struct * thisclass_c);

// Add the given ListboxItem
void cegui_lstbx_addItem(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * item_c);

// Insert an item into the list box before a specified item already in the list. 
void cegui_lstbx_insertItem(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * item_c, struct hg3dclass_struct * position_c);

// Removes the given item from the list box. If the item is has the auto delete state set, the item will be deleted. 
void cegui_lstbx_removeItem(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * item_c);

// Clear the selected state for all items. 
void cegui_lstbx_clearAllSelections(struct hg3dclass_struct * thisclass_c);

// Set whether the list should be sorted. 
void cegui_lstbx_setSortingEnabled(struct hg3dclass_struct * thisclass_c, int setting_c);

// Set whether the list should allow multiple selections or just a single selection. 
void cegui_lstbx_setMultiselectEnabled(struct hg3dclass_struct * thisclass_c, int setting_c);

// Set whether the vertical scroll bar should always be shown. 
void cegui_lstbx_setShowVertScrollbar(struct hg3dclass_struct * thisclass_c, int setting_c);

// Set whether the horizontal scroll bar should always be shown. 
void cegui_lstbx_setShowHorzScrollbar(struct hg3dclass_struct * thisclass_c, int setting_c);

// 
void cegui_lstbx_setItemTooltipsEnabled(struct hg3dclass_struct * thisclass_c, int setting_c);

// Set the select state of an attached ListboxItem
void cegui_lstbx_setItemSelectState(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * item_c, int state_c);

// Set the select state of an attached ListboxItem
void cegui_lstbx_setItemSelectState2(struct hg3dclass_struct * thisclass_c, int item_index_c, int state_c);

// Causes the list box to update it's internal state after changes have been made to one or more attached ListboxItem
void cegui_lstbx_handleUpdatedItemData(struct hg3dclass_struct * thisclass_c);

// Ensure the item at the specified index is visible within the list box. 
void cegui_lstbx_ensureItemIsVisible(struct hg3dclass_struct * thisclass_c, int item_index_c);

// Ensure the item at the specified index is visible within the list box. 
void cegui_lstbx_ensureItemIsVisible2(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * item_c);

// Return a pointer to the vertical scrollbar component widget for this Listbox
void cegui_lstbx_getVertScrollbar(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// Return a pointer to the horizontal scrollbar component widget for this Listbox
void cegui_lstbx_getHorzScrollbar(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// Return the sum of all item heights. 
void cegui_lstbx_getTotalItemsHeight(struct hg3dclass_struct * thisclass_c, float * result_c);

// Return the width of the widest item. 
void cegui_lstbx_getWidestItemWidth(struct hg3dclass_struct * thisclass_c, float * result_c);

// Constructor for Listbox
void cegui_lstbx_construct(char * type_c, char * name_c, struct hg3dclass_struct * result_c);

// Destructor for Listbox
void cegui_lstbx_destruct(struct hg3dclass_struct * thisclass_c);

#endif 
