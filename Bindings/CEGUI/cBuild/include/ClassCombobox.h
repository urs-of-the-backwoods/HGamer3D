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

// ClassCombobox.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassCombobox
#define _DEFINED_HG3D_ClassCombobox

#include "ClassPtr.h"
#include "ClassEditbox.h"
#include "ClassPushButton.h"
#include "ClassComboDropList.h"
#include "ClassListboxItem.h"


// returns the mode of operation for the combo box. 
void cegui_cmbbx_getSingleClickEnabled(struct hg3dclass_struct * thisclass_c, int * result_c);

// returns true if the drop down list is visible. 
void cegui_cmbbx_isDropDownListVisible(struct hg3dclass_struct * thisclass_c, int * result_c);

// Return a pointer to the EditboxCombobox
void cegui_cmbbx_getEditbox(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// Return a pointer to the PushButtonCombobox
void cegui_cmbbx_getPushButton(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// Return a pointer to the ComboDropListCombobox
void cegui_cmbbx_getDropList(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// return true if the Editbox
void cegui_cmbbx_hasInputFocus(struct hg3dclass_struct * thisclass_c, int * result_c);

// return true if the Editbox
void cegui_cmbbx_isReadOnly(struct hg3dclass_struct * thisclass_c, int * result_c);

// return true if the Editbox
void cegui_cmbbx_isTextValid(struct hg3dclass_struct * thisclass_c, int * result_c);

// return the currently set validation string 
void cegui_cmbbx_getValidationString(struct hg3dclass_struct * thisclass_c, char * result_c);

// return the current position of the carat. 
void cegui_cmbbx_getCaratIndex(struct hg3dclass_struct * thisclass_c, int * result_c);

// return the current selection start point. 
void cegui_cmbbx_getSelectionStartIndex(struct hg3dclass_struct * thisclass_c, int * result_c);

// return the current selection end point. 
void cegui_cmbbx_getSelectionEndIndex(struct hg3dclass_struct * thisclass_c, int * result_c);

// return the length of the current selection (in code points / characters). 
void cegui_cmbbx_getSelectionLength(struct hg3dclass_struct * thisclass_c, int * result_c);

// return the maximum text length set for this Editbox
void cegui_cmbbx_getMaxTextLength(struct hg3dclass_struct * thisclass_c, int * result_c);

// Return number of items attached to the list box. 
void cegui_cmbbx_getItemCount(struct hg3dclass_struct * thisclass_c, int * result_c);

// Return a pointer to the currently selected item. 
void cegui_cmbbx_getSelectedItem(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// Return the item at index position index
void cegui_cmbbx_getListboxItemFromIndex(struct hg3dclass_struct * thisclass_c, int index_c, struct hg3dclass_struct * result_c);

// Return the index of ListboxItemitem
void cegui_cmbbx_getItemIndex(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * item_c, int * result_c);

// return whether list sorting is enabled 
void cegui_cmbbx_isSortEnabled(struct hg3dclass_struct * thisclass_c, int * result_c);

// return whether the string at index position index
void cegui_cmbbx_isItemSelected(struct hg3dclass_struct * thisclass_c, int index_c, int * result_c);

// Search the list for an item with the specified text. 
void cegui_cmbbx_findItemWithText(struct hg3dclass_struct * thisclass_c, char * text_c, struct hg3dclass_struct * start_item_c, struct hg3dclass_struct * result_c);

// Return whether the specified ListboxItem
void cegui_cmbbx_isListboxItemInList(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * item_c, int * result_c);

// Return whether the vertical scroll bar is always shown. 
void cegui_cmbbx_isVertScrollbarAlwaysShown(struct hg3dclass_struct * thisclass_c, int * result_c);

// Return whether the horizontal scroll bar is always shown. 
void cegui_cmbbx_isHorzScrollbarAlwaysShown(struct hg3dclass_struct * thisclass_c, int * result_c);

// Initialise the Window
void cegui_cmbbx_initialiseComponents(struct hg3dclass_struct * thisclass_c);

// Show the drop-down list. 
void cegui_cmbbx_showDropList(struct hg3dclass_struct * thisclass_c);

// Hide the drop-down list. 
void cegui_cmbbx_hideDropList(struct hg3dclass_struct * thisclass_c);

// Set the mode of operation for the combo box. 
void cegui_cmbbx_setSingleClickEnabled(struct hg3dclass_struct * thisclass_c, int setting_c);

// Specify whether the Editbox
void cegui_cmbbx_setReadOnly(struct hg3dclass_struct * thisclass_c, int setting_c);

// Set the text validation string. 
void cegui_cmbbx_setValidationString(struct hg3dclass_struct * thisclass_c, char * validation_string_c);

// Set the current position of the carat. 
void cegui_cmbbx_setCaratIndex(struct hg3dclass_struct * thisclass_c, int carat_pos_c);

// Define the current selection for the Editbox
void cegui_cmbbx_setSelection(struct hg3dclass_struct * thisclass_c, int start_pos_c, int end_pos_c);

// set the maximum text length for this Editbox
void cegui_cmbbx_setMaxTextLength(struct hg3dclass_struct * thisclass_c, int max_len_c);

// Activate the edit box component of the Combobox
void cegui_cmbbx_activateEditbox(struct hg3dclass_struct * thisclass_c);

// Remove all items from the list. 
void cegui_cmbbx_resetList(struct hg3dclass_struct * thisclass_c);

// Add the given ListboxItem
void cegui_cmbbx_addItem(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * item_c);

// Insert an item into the list box after a specified item already in the list. 
void cegui_cmbbx_insertItem(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * item_c, struct hg3dclass_struct * position_c);

// Removes the given item from the list box. 
void cegui_cmbbx_removeItem(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * item_c);

// Clear the selected state for all items. 
void cegui_cmbbx_clearAllSelections(struct hg3dclass_struct * thisclass_c);

// Set whether the list should be sorted. 
void cegui_cmbbx_setSortingEnabled(struct hg3dclass_struct * thisclass_c, int setting_c);

// Set whether the vertical scroll bar should always be shown. 
void cegui_cmbbx_setShowVertScrollbar(struct hg3dclass_struct * thisclass_c, int setting_c);

// Set whether the horizontal scroll bar should always be shown. 
void cegui_cmbbx_setShowHorzScrollbar(struct hg3dclass_struct * thisclass_c, int setting_c);

// Set the select state of an attached ListboxItem
void cegui_cmbbx_setItemSelectState(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * item_c, int state_c);

// Set the select state of an attached ListboxItem
void cegui_cmbbx_setItemSelectState2(struct hg3dclass_struct * thisclass_c, int item_index_c, int state_c);

// Causes the list box to update it's internal state after changes have been made to one or more attached ListboxItem
void cegui_cmbbx_handleUpdatedListItemData(struct hg3dclass_struct * thisclass_c);

// Constructor for Combobox
void cegui_cmbbx_construct(char * type_c, char * name_c, struct hg3dclass_struct * result_c);

// Destructor for Combobox
void cegui_cmbbx_destruct(struct hg3dclass_struct * thisclass_c);

#endif 
