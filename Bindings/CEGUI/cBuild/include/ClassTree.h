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

// ClassTree.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassTree
#define _DEFINED_HG3D_ClassTree

#include "ClassPtr.h"
#include "ClassScrollbar.h"


// 
void cegui_tree_doTreeRender(struct hg3dclass_struct * thisclass_c);

// 
void cegui_tree_doScrollbars(struct hg3dclass_struct * thisclass_c);

// Return number of items attached to the tree. 
void cegui_tree_getItemCount(struct hg3dclass_struct * thisclass_c, int * result_c);

// Return the number of selected items in the tree. 
void cegui_tree_getSelectedCount(struct hg3dclass_struct * thisclass_c, int * result_c);

// return whether tree sorting is enabled 
void cegui_tree_isSortEnabled(struct hg3dclass_struct * thisclass_c, int * result_c);

// 
void cegui_tree_getVertScrollbar(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// 
void cegui_tree_getHorzScrollbar(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// return whether multi-select is enabled 
void cegui_tree_isMultiselectEnabled(struct hg3dclass_struct * thisclass_c, int * result_c);

// 
void cegui_tree_isItemTooltipsEnabled(struct hg3dclass_struct * thisclass_c, int * result_c);

// Return whether the vertical scroll bar is always shown. 
void cegui_tree_isVertScrollbarAlwaysShown(struct hg3dclass_struct * thisclass_c, int * result_c);

// Return whether the horizontal scroll bar is always shown. 
void cegui_tree_isHorzScrollbarAlwaysShown(struct hg3dclass_struct * thisclass_c, int * result_c);

// Initialise the Window
void cegui_tree_initialise(struct hg3dclass_struct * thisclass_c);

// Remove all items from the tree. 
void cegui_tree_resetList(struct hg3dclass_struct * thisclass_c);

// Clear the selected state for all items. 
void cegui_tree_clearAllSelections(struct hg3dclass_struct * thisclass_c);

// Set whether the tree should be sorted. 
void cegui_tree_setSortingEnabled(struct hg3dclass_struct * thisclass_c, int setting_c);

// Set whether the tree should allow multiple selections or just a single selection. 
void cegui_tree_setMultiselectEnabled(struct hg3dclass_struct * thisclass_c, int setting_c);

// Set whether the vertical scroll bar should always be shown. 
void cegui_tree_setShowVertScrollbar(struct hg3dclass_struct * thisclass_c, int setting_c);

// Set whether the horizontal scroll bar should always be shown. 
void cegui_tree_setShowHorzScrollbar(struct hg3dclass_struct * thisclass_c, int setting_c);

// 
void cegui_tree_setItemTooltipsEnabled(struct hg3dclass_struct * thisclass_c, int setting_c);

// Set the select state of an attached TreeItem. 
void cegui_tree_setItemSelectState2(struct hg3dclass_struct * thisclass_c, int item_index_c, int state_c);

// Set the LookNFeel that shoule be used for this window. 
void cegui_tree_setLookNFeel(struct hg3dclass_struct * thisclass_c, char * look_c);

// Causes the tree to update it's internal state after changes have been made to one or more attached TreeItem objects. 
void cegui_tree_handleUpdatedItemData(struct hg3dclass_struct * thisclass_c);

// Constructor for Tree
void cegui_tree_construct(char * type_c, char * name_c, struct hg3dclass_struct * result_c);

// Destructor for Tree
void cegui_tree_destruct(struct hg3dclass_struct * thisclass_c);

#endif 
