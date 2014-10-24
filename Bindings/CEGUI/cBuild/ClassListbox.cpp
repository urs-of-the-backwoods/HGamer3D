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

// ClassListbox.cpp

// 

#include <wchar.h>
#include <string>
#include <iostream>

#include <iostream>
	#include <typeinfo>
	#include <stdio.h>
	#include <cstring>
	#include <exception>
	#include "CEGUIDllDefines.h"
	#include "ClassPtr.h"
	#include "./CEGUI.h"
#include "./CEGUIString.h"
#include "RendererModules/Ogre/CEGUIOgreRenderer.h"
#include "./WindowManagerHG3D.h"
#include "./SystemHG3D.h"
#include "HG3DCommandHandler.h"
#include "HG3DEventStaticFunctions.h"
#include "HG3DListboxStaticFunctions.h"
#include "HG3DEventController.h"
#include "HG3DEventModule.h"
#include "HG3DWindowStaticFunctions.h"

using namespace CEGUI;



// Return number of items attached to the list box. 
extern "C" CEGUI_LIB_EXPORT void cegui_lstbx_getItemCount(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::Listbox * thisclass_cpp = static_cast<CEGUI::Listbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Listbox"));
  size_t result_cpp;
  result_cpp = (thisclass_cpp->getItemCount());
  *result_c = (int)result_cpp;
};

// Return the number of selected items in the list box. 
extern "C" CEGUI_LIB_EXPORT void cegui_lstbx_getSelectedCount(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::Listbox * thisclass_cpp = static_cast<CEGUI::Listbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Listbox"));
  size_t result_cpp;
  result_cpp = (thisclass_cpp->getSelectedCount());
  *result_c = (int)result_cpp;
};

// Return a pointer to the first selected item. 
extern "C" CEGUI_LIB_EXPORT void cegui_lstbx_getFirstSelectedItem(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  CEGUI::Listbox * thisclass_cpp = static_cast<CEGUI::Listbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Listbox"));
  CEGUI::ListboxItem * result_cpp;
  result_cpp = (thisclass_cpp->getFirstSelectedItem());
  *result_c = getHG3DClass_ListboxItem((void *) result_cpp);
;
};

// Return a pointer to the next selected item after item start_item
extern "C" CEGUI_LIB_EXPORT void cegui_lstbx_getNextSelected(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * start_item_c, struct hg3dclass_struct * result_c)
{
  CEGUI::Listbox * thisclass_cpp = static_cast<CEGUI::Listbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Listbox"));
  const CEGUI::ListboxItem * start_item_cpp = static_cast<CEGUI::ListboxItem*> (getHG3DClassPtr(*start_item_c, "CEGUI::ListboxItem"));
  CEGUI::ListboxItem * result_cpp;
  result_cpp = (thisclass_cpp->getNextSelected(start_item_cpp));
  *result_c = getHG3DClass_ListboxItem((void *) result_cpp);
;
};

// Return the item at index position index
extern "C" CEGUI_LIB_EXPORT void cegui_lstbx_getListboxItemFromIndex(struct hg3dclass_struct * thisclass_c, int index_c, struct hg3dclass_struct * result_c)
{
  CEGUI::Listbox * thisclass_cpp = static_cast<CEGUI::Listbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Listbox"));
  size_t index_cpp = (size_t)index_c;
  CEGUI::ListboxItem * result_cpp;
  result_cpp = (thisclass_cpp->getListboxItemFromIndex(index_cpp));
  *result_c = getHG3DClass_ListboxItem((void *) result_cpp);
;
};

// Return the index of ListboxItemitem
extern "C" CEGUI_LIB_EXPORT void cegui_lstbx_getItemIndex(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * item_c, int * result_c)
{
  CEGUI::Listbox * thisclass_cpp = static_cast<CEGUI::Listbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Listbox"));
  const CEGUI::ListboxItem * item_cpp = static_cast<CEGUI::ListboxItem*> (getHG3DClassPtr(*item_c, "CEGUI::ListboxItem"));
  size_t result_cpp;
  result_cpp = (thisclass_cpp->getItemIndex(item_cpp));
  *result_c = (int)result_cpp;
};

// return whether list sorting is enabled 
extern "C" CEGUI_LIB_EXPORT void cegui_lstbx_isSortEnabled(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::Listbox * thisclass_cpp = static_cast<CEGUI::Listbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Listbox"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isSortEnabled());
  *result_c = (int)result_cpp;
};

// return whether multi-select is enabled 
extern "C" CEGUI_LIB_EXPORT void cegui_lstbx_isMultiselectEnabled(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::Listbox * thisclass_cpp = static_cast<CEGUI::Listbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Listbox"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isMultiselectEnabled());
  *result_c = (int)result_cpp;
};

// 
extern "C" CEGUI_LIB_EXPORT void cegui_lstbx_isItemTooltipsEnabled(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::Listbox * thisclass_cpp = static_cast<CEGUI::Listbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Listbox"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isItemTooltipsEnabled());
  *result_c = (int)result_cpp;
};

// return whether the string at index position index
extern "C" CEGUI_LIB_EXPORT void cegui_lstbx_isItemSelected(struct hg3dclass_struct * thisclass_c, int index_c, int * result_c)
{
  CEGUI::Listbox * thisclass_cpp = static_cast<CEGUI::Listbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Listbox"));
  size_t index_cpp = (size_t)index_c;
  bool result_cpp;
  result_cpp = (thisclass_cpp->isItemSelected(index_cpp));
  *result_c = (int)result_cpp;
};

// Search the list for an item with the specified text. 
extern "C" CEGUI_LIB_EXPORT void cegui_lstbx_findItemWithText(struct hg3dclass_struct * thisclass_c, char * text_c, struct hg3dclass_struct * start_item_c, struct hg3dclass_struct * result_c)
{
  CEGUI::Listbox * thisclass_cpp = static_cast<CEGUI::Listbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Listbox"));
  CEGUI::String text_cpp = CEGUI::String((const char*) text_c);
  const CEGUI::ListboxItem * start_item_cpp = static_cast<CEGUI::ListboxItem*> (getHG3DClassPtr(*start_item_c, "CEGUI::ListboxItem"));
  CEGUI::ListboxItem * result_cpp;
  result_cpp = (thisclass_cpp->findItemWithText(text_cpp, start_item_cpp));
  *result_c = getHG3DClass_ListboxItem((void *) result_cpp);
;
};

// Return whether the specified ListboxItem
extern "C" CEGUI_LIB_EXPORT void cegui_lstbx_isListboxItemInList(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * item_c, int * result_c)
{
  CEGUI::Listbox * thisclass_cpp = static_cast<CEGUI::Listbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Listbox"));
  const CEGUI::ListboxItem * item_cpp = static_cast<CEGUI::ListboxItem*> (getHG3DClassPtr(*item_c, "CEGUI::ListboxItem"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isListboxItemInList(item_cpp));
  *result_c = (int)result_cpp;
};

// Return whether the vertical scroll bar is always shown. 
extern "C" CEGUI_LIB_EXPORT void cegui_lstbx_isVertScrollbarAlwaysShown(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::Listbox * thisclass_cpp = static_cast<CEGUI::Listbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Listbox"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isVertScrollbarAlwaysShown());
  *result_c = (int)result_cpp;
};

// Return whether the horizontal scroll bar is always shown. 
extern "C" CEGUI_LIB_EXPORT void cegui_lstbx_isHorzScrollbarAlwaysShown(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::Listbox * thisclass_cpp = static_cast<CEGUI::Listbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Listbox"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isHorzScrollbarAlwaysShown());
  *result_c = (int)result_cpp;
};

// Initialise the Window
extern "C" CEGUI_LIB_EXPORT void cegui_lstbx_initialiseComponents(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::Listbox * thisclass_cpp = static_cast<CEGUI::Listbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Listbox"));
  (thisclass_cpp->initialiseComponents());
};

// Remove all items from the list. 
extern "C" CEGUI_LIB_EXPORT void cegui_lstbx_resetList(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::Listbox * thisclass_cpp = static_cast<CEGUI::Listbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Listbox"));
  (thisclass_cpp->resetList());
};

// Add the given ListboxItem
extern "C" CEGUI_LIB_EXPORT void cegui_lstbx_addItem(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * item_c)
{
  CEGUI::Listbox * thisclass_cpp = static_cast<CEGUI::Listbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Listbox"));
  CEGUI::ListboxItem * item_cpp = static_cast<CEGUI::ListboxItem*> (getHG3DClassPtr(*item_c, "CEGUI::ListboxItem"));
  (thisclass_cpp->addItem(item_cpp));
};

// Insert an item into the list box before a specified item already in the list. 
extern "C" CEGUI_LIB_EXPORT void cegui_lstbx_insertItem(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * item_c, struct hg3dclass_struct * position_c)
{
  CEGUI::Listbox * thisclass_cpp = static_cast<CEGUI::Listbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Listbox"));
  CEGUI::ListboxItem * item_cpp = static_cast<CEGUI::ListboxItem*> (getHG3DClassPtr(*item_c, "CEGUI::ListboxItem"));
  const CEGUI::ListboxItem * position_cpp = static_cast<CEGUI::ListboxItem*> (getHG3DClassPtr(*position_c, "CEGUI::ListboxItem"));
  (thisclass_cpp->insertItem(item_cpp, position_cpp));
};

// Removes the given item from the list box. If the item is has the auto delete state set, the item will be deleted. 
extern "C" CEGUI_LIB_EXPORT void cegui_lstbx_removeItem(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * item_c)
{
  CEGUI::Listbox * thisclass_cpp = static_cast<CEGUI::Listbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Listbox"));
  const CEGUI::ListboxItem * item_cpp = static_cast<CEGUI::ListboxItem*> (getHG3DClassPtr(*item_c, "CEGUI::ListboxItem"));
  (thisclass_cpp->removeItem(item_cpp));
};

// Clear the selected state for all items. 
extern "C" CEGUI_LIB_EXPORT void cegui_lstbx_clearAllSelections(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::Listbox * thisclass_cpp = static_cast<CEGUI::Listbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Listbox"));
  (thisclass_cpp->clearAllSelections());
};

// Set whether the list should be sorted. 
extern "C" CEGUI_LIB_EXPORT void cegui_lstbx_setSortingEnabled(struct hg3dclass_struct * thisclass_c, int setting_c)
{
  CEGUI::Listbox * thisclass_cpp = static_cast<CEGUI::Listbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Listbox"));
  bool setting_cpp = (bool)setting_c;
  (thisclass_cpp->setSortingEnabled(setting_cpp));
};

// Set whether the list should allow multiple selections or just a single selection. 
extern "C" CEGUI_LIB_EXPORT void cegui_lstbx_setMultiselectEnabled(struct hg3dclass_struct * thisclass_c, int setting_c)
{
  CEGUI::Listbox * thisclass_cpp = static_cast<CEGUI::Listbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Listbox"));
  bool setting_cpp = (bool)setting_c;
  (thisclass_cpp->setMultiselectEnabled(setting_cpp));
};

// Set whether the vertical scroll bar should always be shown. 
extern "C" CEGUI_LIB_EXPORT void cegui_lstbx_setShowVertScrollbar(struct hg3dclass_struct * thisclass_c, int setting_c)
{
  CEGUI::Listbox * thisclass_cpp = static_cast<CEGUI::Listbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Listbox"));
  bool setting_cpp = (bool)setting_c;
  (thisclass_cpp->setShowVertScrollbar(setting_cpp));
};

// Set whether the horizontal scroll bar should always be shown. 
extern "C" CEGUI_LIB_EXPORT void cegui_lstbx_setShowHorzScrollbar(struct hg3dclass_struct * thisclass_c, int setting_c)
{
  CEGUI::Listbox * thisclass_cpp = static_cast<CEGUI::Listbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Listbox"));
  bool setting_cpp = (bool)setting_c;
  (thisclass_cpp->setShowHorzScrollbar(setting_cpp));
};

// 
extern "C" CEGUI_LIB_EXPORT void cegui_lstbx_setItemTooltipsEnabled(struct hg3dclass_struct * thisclass_c, int setting_c)
{
  CEGUI::Listbox * thisclass_cpp = static_cast<CEGUI::Listbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Listbox"));
  bool setting_cpp = (bool)setting_c;
  (thisclass_cpp->setItemTooltipsEnabled(setting_cpp));
};

// Set the select state of an attached ListboxItem
extern "C" CEGUI_LIB_EXPORT void cegui_lstbx_setItemSelectState(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * item_c, int state_c)
{
  CEGUI::Listbox * thisclass_cpp = static_cast<CEGUI::Listbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Listbox"));
  CEGUI::ListboxItem * item_cpp = static_cast<CEGUI::ListboxItem*> (getHG3DClassPtr(*item_c, "CEGUI::ListboxItem"));
  bool state_cpp = (bool)state_c;
  (thisclass_cpp->setItemSelectState(item_cpp, state_cpp));
};

// Set the select state of an attached ListboxItem
extern "C" CEGUI_LIB_EXPORT void cegui_lstbx_setItemSelectState2(struct hg3dclass_struct * thisclass_c, int item_index_c, int state_c)
{
  CEGUI::Listbox * thisclass_cpp = static_cast<CEGUI::Listbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Listbox"));
  size_t item_index_cpp = (size_t)item_index_c;
  bool state_cpp = (bool)state_c;
  (thisclass_cpp->setItemSelectState(item_index_cpp, state_cpp));
};

// Causes the list box to update it's internal state after changes have been made to one or more attached ListboxItem
extern "C" CEGUI_LIB_EXPORT void cegui_lstbx_handleUpdatedItemData(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::Listbox * thisclass_cpp = static_cast<CEGUI::Listbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Listbox"));
  (thisclass_cpp->handleUpdatedItemData());
};

// Ensure the item at the specified index is visible within the list box. 
extern "C" CEGUI_LIB_EXPORT void cegui_lstbx_ensureItemIsVisible(struct hg3dclass_struct * thisclass_c, int item_index_c)
{
  CEGUI::Listbox * thisclass_cpp = static_cast<CEGUI::Listbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Listbox"));
  size_t item_index_cpp = (size_t)item_index_c;
  (thisclass_cpp->ensureItemIsVisible(item_index_cpp));
};

// Ensure the item at the specified index is visible within the list box. 
extern "C" CEGUI_LIB_EXPORT void cegui_lstbx_ensureItemIsVisible2(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * item_c)
{
  CEGUI::Listbox * thisclass_cpp = static_cast<CEGUI::Listbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Listbox"));
  const CEGUI::ListboxItem * item_cpp = static_cast<CEGUI::ListboxItem*> (getHG3DClassPtr(*item_c, "CEGUI::ListboxItem"));
  (thisclass_cpp->ensureItemIsVisible(item_cpp));
};

// Return a pointer to the vertical scrollbar component widget for this Listbox
extern "C" CEGUI_LIB_EXPORT void cegui_lstbx_getVertScrollbar(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  CEGUI::Listbox * thisclass_cpp = static_cast<CEGUI::Listbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Listbox"));
  CEGUI::Scrollbar * result_cpp;
  result_cpp = (thisclass_cpp->getVertScrollbar());
  *result_c = getHG3DClass_Scrollbar((void *) result_cpp);
;
};

// Return a pointer to the horizontal scrollbar component widget for this Listbox
extern "C" CEGUI_LIB_EXPORT void cegui_lstbx_getHorzScrollbar(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  CEGUI::Listbox * thisclass_cpp = static_cast<CEGUI::Listbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Listbox"));
  CEGUI::Scrollbar * result_cpp;
  result_cpp = (thisclass_cpp->getHorzScrollbar());
  *result_c = getHG3DClass_Scrollbar((void *) result_cpp);
;
};

// Return the sum of all item heights. 
extern "C" CEGUI_LIB_EXPORT void cegui_lstbx_getTotalItemsHeight(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  CEGUI::Listbox * thisclass_cpp = static_cast<CEGUI::Listbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Listbox"));
  float result_cpp;
  result_cpp = (thisclass_cpp->getTotalItemsHeight());
  *result_c = (float)result_cpp;
};

// Return the width of the widest item. 
extern "C" CEGUI_LIB_EXPORT void cegui_lstbx_getWidestItemWidth(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  CEGUI::Listbox * thisclass_cpp = static_cast<CEGUI::Listbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Listbox"));
  float result_cpp;
  result_cpp = (thisclass_cpp->getWidestItemWidth());
  *result_c = (float)result_cpp;
};

// Constructor for Listbox
extern "C" CEGUI_LIB_EXPORT void cegui_lstbx_construct(char * type_c, char * name_c, struct hg3dclass_struct * result_c)
{
  CEGUI::String type_cpp = CEGUI::String((const char*) type_c);
  CEGUI::String name_cpp = CEGUI::String((const char*) name_c);
  CEGUI::Listbox * result_cpp;
  result_cpp = (new CEGUI::Listbox(type_cpp, name_cpp));
  *result_c = getHG3DClass_Listbox((void *) result_cpp);
;
};

// Destructor for Listbox
extern "C" CEGUI_LIB_EXPORT void cegui_lstbx_destruct(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::Listbox * thisclass_cpp = static_cast<CEGUI::Listbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Listbox"));
  (delete thisclass_cpp);
};

