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

// ClassCombobox.cpp

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



// returns the mode of operation for the combo box. 
extern "C" CEGUI_LIB_EXPORT void cegui_cmbbx_getSingleClickEnabled(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::Combobox * thisclass_cpp = static_cast<CEGUI::Combobox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Combobox"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->getSingleClickEnabled());
  *result_c = (int)result_cpp;
};

// returns true if the drop down list is visible. 
extern "C" CEGUI_LIB_EXPORT void cegui_cmbbx_isDropDownListVisible(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::Combobox * thisclass_cpp = static_cast<CEGUI::Combobox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Combobox"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isDropDownListVisible());
  *result_c = (int)result_cpp;
};

// Return a pointer to the EditboxCombobox
extern "C" CEGUI_LIB_EXPORT void cegui_cmbbx_getEditbox(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  CEGUI::Combobox * thisclass_cpp = static_cast<CEGUI::Combobox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Combobox"));
  CEGUI::Editbox * result_cpp;
  result_cpp = (thisclass_cpp->getEditbox());
  *result_c = getHG3DClass_Editbox((void *) result_cpp);
;
};

// Return a pointer to the PushButtonCombobox
extern "C" CEGUI_LIB_EXPORT void cegui_cmbbx_getPushButton(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  CEGUI::Combobox * thisclass_cpp = static_cast<CEGUI::Combobox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Combobox"));
  CEGUI::PushButton * result_cpp;
  result_cpp = (thisclass_cpp->getPushButton());
  *result_c = getHG3DClass_PushButton((void *) result_cpp);
;
};

// Return a pointer to the ComboDropListCombobox
extern "C" CEGUI_LIB_EXPORT void cegui_cmbbx_getDropList(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  CEGUI::Combobox * thisclass_cpp = static_cast<CEGUI::Combobox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Combobox"));
  CEGUI::ComboDropList * result_cpp;
  result_cpp = (thisclass_cpp->getDropList());
  *result_c = getHG3DClass_ComboDropList((void *) result_cpp);
;
};

// return true if the Editbox
extern "C" CEGUI_LIB_EXPORT void cegui_cmbbx_hasInputFocus(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::Combobox * thisclass_cpp = static_cast<CEGUI::Combobox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Combobox"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->hasInputFocus());
  *result_c = (int)result_cpp;
};

// return true if the Editbox
extern "C" CEGUI_LIB_EXPORT void cegui_cmbbx_isReadOnly(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::Combobox * thisclass_cpp = static_cast<CEGUI::Combobox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Combobox"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isReadOnly());
  *result_c = (int)result_cpp;
};

// return true if the Editbox
extern "C" CEGUI_LIB_EXPORT void cegui_cmbbx_isTextValid(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::Combobox * thisclass_cpp = static_cast<CEGUI::Combobox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Combobox"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isTextValid());
  *result_c = (int)result_cpp;
};

// return the currently set validation string 
extern "C" CEGUI_LIB_EXPORT void cegui_cmbbx_getValidationString(struct hg3dclass_struct * thisclass_c, char * result_c)
{
  CEGUI::Combobox * thisclass_cpp = static_cast<CEGUI::Combobox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Combobox"));
  CEGUI::String result_cpp;
  result_cpp = (thisclass_cpp->getValidationString());
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

// return the current position of the carat. 
extern "C" CEGUI_LIB_EXPORT void cegui_cmbbx_getCaratIndex(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::Combobox * thisclass_cpp = static_cast<CEGUI::Combobox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Combobox"));
  size_t result_cpp;
  result_cpp = (thisclass_cpp->getCaratIndex());
  *result_c = (int)result_cpp;
};

// return the current selection start point. 
extern "C" CEGUI_LIB_EXPORT void cegui_cmbbx_getSelectionStartIndex(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::Combobox * thisclass_cpp = static_cast<CEGUI::Combobox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Combobox"));
  size_t result_cpp;
  result_cpp = (thisclass_cpp->getSelectionStartIndex());
  *result_c = (int)result_cpp;
};

// return the current selection end point. 
extern "C" CEGUI_LIB_EXPORT void cegui_cmbbx_getSelectionEndIndex(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::Combobox * thisclass_cpp = static_cast<CEGUI::Combobox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Combobox"));
  size_t result_cpp;
  result_cpp = (thisclass_cpp->getSelectionEndIndex());
  *result_c = (int)result_cpp;
};

// return the length of the current selection (in code points / characters). 
extern "C" CEGUI_LIB_EXPORT void cegui_cmbbx_getSelectionLength(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::Combobox * thisclass_cpp = static_cast<CEGUI::Combobox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Combobox"));
  size_t result_cpp;
  result_cpp = (thisclass_cpp->getSelectionLength());
  *result_c = (int)result_cpp;
};

// return the maximum text length set for this Editbox
extern "C" CEGUI_LIB_EXPORT void cegui_cmbbx_getMaxTextLength(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::Combobox * thisclass_cpp = static_cast<CEGUI::Combobox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Combobox"));
  size_t result_cpp;
  result_cpp = (thisclass_cpp->getMaxTextLength());
  *result_c = (int)result_cpp;
};

// Return number of items attached to the list box. 
extern "C" CEGUI_LIB_EXPORT void cegui_cmbbx_getItemCount(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::Combobox * thisclass_cpp = static_cast<CEGUI::Combobox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Combobox"));
  size_t result_cpp;
  result_cpp = (thisclass_cpp->getItemCount());
  *result_c = (int)result_cpp;
};

// Return a pointer to the currently selected item. 
extern "C" CEGUI_LIB_EXPORT void cegui_cmbbx_getSelectedItem(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  CEGUI::Combobox * thisclass_cpp = static_cast<CEGUI::Combobox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Combobox"));
  CEGUI::ListboxItem * result_cpp;
  result_cpp = (thisclass_cpp->getSelectedItem());
  *result_c = getHG3DClass_ListboxItem((void *) result_cpp);
;
};

// Return the item at index position index
extern "C" CEGUI_LIB_EXPORT void cegui_cmbbx_getListboxItemFromIndex(struct hg3dclass_struct * thisclass_c, int index_c, struct hg3dclass_struct * result_c)
{
  CEGUI::Combobox * thisclass_cpp = static_cast<CEGUI::Combobox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Combobox"));
  size_t index_cpp = (size_t)index_c;
  CEGUI::ListboxItem * result_cpp;
  result_cpp = (thisclass_cpp->getListboxItemFromIndex(index_cpp));
  *result_c = getHG3DClass_ListboxItem((void *) result_cpp);
;
};

// Return the index of ListboxItemitem
extern "C" CEGUI_LIB_EXPORT void cegui_cmbbx_getItemIndex(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * item_c, int * result_c)
{
  CEGUI::Combobox * thisclass_cpp = static_cast<CEGUI::Combobox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Combobox"));
  const CEGUI::ListboxItem * item_cpp = static_cast<CEGUI::ListboxItem*> (getHG3DClassPtr(*item_c, "CEGUI::ListboxItem"));
  size_t result_cpp;
  result_cpp = (thisclass_cpp->getItemIndex(item_cpp));
  *result_c = (int)result_cpp;
};

// return whether list sorting is enabled 
extern "C" CEGUI_LIB_EXPORT void cegui_cmbbx_isSortEnabled(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::Combobox * thisclass_cpp = static_cast<CEGUI::Combobox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Combobox"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isSortEnabled());
  *result_c = (int)result_cpp;
};

// return whether the string at index position index
extern "C" CEGUI_LIB_EXPORT void cegui_cmbbx_isItemSelected(struct hg3dclass_struct * thisclass_c, int index_c, int * result_c)
{
  CEGUI::Combobox * thisclass_cpp = static_cast<CEGUI::Combobox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Combobox"));
  size_t index_cpp = (size_t)index_c;
  bool result_cpp;
  result_cpp = (thisclass_cpp->isItemSelected(index_cpp));
  *result_c = (int)result_cpp;
};

// Search the list for an item with the specified text. 
extern "C" CEGUI_LIB_EXPORT void cegui_cmbbx_findItemWithText(struct hg3dclass_struct * thisclass_c, char * text_c, struct hg3dclass_struct * start_item_c, struct hg3dclass_struct * result_c)
{
  CEGUI::Combobox * thisclass_cpp = static_cast<CEGUI::Combobox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Combobox"));
  CEGUI::String text_cpp = CEGUI::String((const char*) text_c);
  const CEGUI::ListboxItem * start_item_cpp = static_cast<CEGUI::ListboxItem*> (getHG3DClassPtr(*start_item_c, "CEGUI::ListboxItem"));
  CEGUI::ListboxItem * result_cpp;
  result_cpp = (thisclass_cpp->findItemWithText(text_cpp, start_item_cpp));
  *result_c = getHG3DClass_ListboxItem((void *) result_cpp);
;
};

// Return whether the specified ListboxItem
extern "C" CEGUI_LIB_EXPORT void cegui_cmbbx_isListboxItemInList(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * item_c, int * result_c)
{
  CEGUI::Combobox * thisclass_cpp = static_cast<CEGUI::Combobox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Combobox"));
  const CEGUI::ListboxItem * item_cpp = static_cast<CEGUI::ListboxItem*> (getHG3DClassPtr(*item_c, "CEGUI::ListboxItem"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isListboxItemInList(item_cpp));
  *result_c = (int)result_cpp;
};

// Return whether the vertical scroll bar is always shown. 
extern "C" CEGUI_LIB_EXPORT void cegui_cmbbx_isVertScrollbarAlwaysShown(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::Combobox * thisclass_cpp = static_cast<CEGUI::Combobox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Combobox"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isVertScrollbarAlwaysShown());
  *result_c = (int)result_cpp;
};

// Return whether the horizontal scroll bar is always shown. 
extern "C" CEGUI_LIB_EXPORT void cegui_cmbbx_isHorzScrollbarAlwaysShown(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::Combobox * thisclass_cpp = static_cast<CEGUI::Combobox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Combobox"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isHorzScrollbarAlwaysShown());
  *result_c = (int)result_cpp;
};

// Initialise the Window
extern "C" CEGUI_LIB_EXPORT void cegui_cmbbx_initialiseComponents(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::Combobox * thisclass_cpp = static_cast<CEGUI::Combobox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Combobox"));
  (thisclass_cpp->initialiseComponents());
};

// Show the drop-down list. 
extern "C" CEGUI_LIB_EXPORT void cegui_cmbbx_showDropList(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::Combobox * thisclass_cpp = static_cast<CEGUI::Combobox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Combobox"));
  (thisclass_cpp->showDropList());
};

// Hide the drop-down list. 
extern "C" CEGUI_LIB_EXPORT void cegui_cmbbx_hideDropList(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::Combobox * thisclass_cpp = static_cast<CEGUI::Combobox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Combobox"));
  (thisclass_cpp->hideDropList());
};

// Set the mode of operation for the combo box. 
extern "C" CEGUI_LIB_EXPORT void cegui_cmbbx_setSingleClickEnabled(struct hg3dclass_struct * thisclass_c, int setting_c)
{
  CEGUI::Combobox * thisclass_cpp = static_cast<CEGUI::Combobox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Combobox"));
  bool setting_cpp = (bool)setting_c;
  (thisclass_cpp->setSingleClickEnabled(setting_cpp));
};

// Specify whether the Editbox
extern "C" CEGUI_LIB_EXPORT void cegui_cmbbx_setReadOnly(struct hg3dclass_struct * thisclass_c, int setting_c)
{
  CEGUI::Combobox * thisclass_cpp = static_cast<CEGUI::Combobox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Combobox"));
  bool setting_cpp = (bool)setting_c;
  (thisclass_cpp->setReadOnly(setting_cpp));
};

// Set the text validation string. 
extern "C" CEGUI_LIB_EXPORT void cegui_cmbbx_setValidationString(struct hg3dclass_struct * thisclass_c, char * validation_string_c)
{
  CEGUI::Combobox * thisclass_cpp = static_cast<CEGUI::Combobox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Combobox"));
  CEGUI::String validation_string_cpp = CEGUI::String((const char*) validation_string_c);
  (thisclass_cpp->setValidationString(validation_string_cpp));
};

// Set the current position of the carat. 
extern "C" CEGUI_LIB_EXPORT void cegui_cmbbx_setCaratIndex(struct hg3dclass_struct * thisclass_c, int carat_pos_c)
{
  CEGUI::Combobox * thisclass_cpp = static_cast<CEGUI::Combobox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Combobox"));
  size_t carat_pos_cpp = (size_t)carat_pos_c;
  (thisclass_cpp->setCaratIndex(carat_pos_cpp));
};

// Define the current selection for the Editbox
extern "C" CEGUI_LIB_EXPORT void cegui_cmbbx_setSelection(struct hg3dclass_struct * thisclass_c, int start_pos_c, int end_pos_c)
{
  CEGUI::Combobox * thisclass_cpp = static_cast<CEGUI::Combobox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Combobox"));
  size_t start_pos_cpp = (size_t)start_pos_c;
  size_t end_pos_cpp = (size_t)end_pos_c;
  (thisclass_cpp->setSelection(start_pos_cpp, end_pos_cpp));
};

// set the maximum text length for this Editbox
extern "C" CEGUI_LIB_EXPORT void cegui_cmbbx_setMaxTextLength(struct hg3dclass_struct * thisclass_c, int max_len_c)
{
  CEGUI::Combobox * thisclass_cpp = static_cast<CEGUI::Combobox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Combobox"));
  size_t max_len_cpp = (size_t)max_len_c;
  (thisclass_cpp->setMaxTextLength(max_len_cpp));
};

// Activate the edit box component of the Combobox
extern "C" CEGUI_LIB_EXPORT void cegui_cmbbx_activateEditbox(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::Combobox * thisclass_cpp = static_cast<CEGUI::Combobox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Combobox"));
  (thisclass_cpp->activateEditbox());
};

// Remove all items from the list. 
extern "C" CEGUI_LIB_EXPORT void cegui_cmbbx_resetList(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::Combobox * thisclass_cpp = static_cast<CEGUI::Combobox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Combobox"));
  (thisclass_cpp->resetList());
};

// Add the given ListboxItem
extern "C" CEGUI_LIB_EXPORT void cegui_cmbbx_addItem(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * item_c)
{
  CEGUI::Combobox * thisclass_cpp = static_cast<CEGUI::Combobox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Combobox"));
  CEGUI::ListboxItem * item_cpp = static_cast<CEGUI::ListboxItem*> (getHG3DClassPtr(*item_c, "CEGUI::ListboxItem"));
  (thisclass_cpp->addItem(item_cpp));
};

// Insert an item into the list box after a specified item already in the list. 
extern "C" CEGUI_LIB_EXPORT void cegui_cmbbx_insertItem(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * item_c, struct hg3dclass_struct * position_c)
{
  CEGUI::Combobox * thisclass_cpp = static_cast<CEGUI::Combobox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Combobox"));
  CEGUI::ListboxItem * item_cpp = static_cast<CEGUI::ListboxItem*> (getHG3DClassPtr(*item_c, "CEGUI::ListboxItem"));
  const CEGUI::ListboxItem * position_cpp = static_cast<CEGUI::ListboxItem*> (getHG3DClassPtr(*position_c, "CEGUI::ListboxItem"));
  (thisclass_cpp->insertItem(item_cpp, position_cpp));
};

// Removes the given item from the list box. 
extern "C" CEGUI_LIB_EXPORT void cegui_cmbbx_removeItem(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * item_c)
{
  CEGUI::Combobox * thisclass_cpp = static_cast<CEGUI::Combobox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Combobox"));
  const CEGUI::ListboxItem * item_cpp = static_cast<CEGUI::ListboxItem*> (getHG3DClassPtr(*item_c, "CEGUI::ListboxItem"));
  (thisclass_cpp->removeItem(item_cpp));
};

// Clear the selected state for all items. 
extern "C" CEGUI_LIB_EXPORT void cegui_cmbbx_clearAllSelections(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::Combobox * thisclass_cpp = static_cast<CEGUI::Combobox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Combobox"));
  (thisclass_cpp->clearAllSelections());
};

// Set whether the list should be sorted. 
extern "C" CEGUI_LIB_EXPORT void cegui_cmbbx_setSortingEnabled(struct hg3dclass_struct * thisclass_c, int setting_c)
{
  CEGUI::Combobox * thisclass_cpp = static_cast<CEGUI::Combobox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Combobox"));
  bool setting_cpp = (bool)setting_c;
  (thisclass_cpp->setSortingEnabled(setting_cpp));
};

// Set whether the vertical scroll bar should always be shown. 
extern "C" CEGUI_LIB_EXPORT void cegui_cmbbx_setShowVertScrollbar(struct hg3dclass_struct * thisclass_c, int setting_c)
{
  CEGUI::Combobox * thisclass_cpp = static_cast<CEGUI::Combobox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Combobox"));
  bool setting_cpp = (bool)setting_c;
  (thisclass_cpp->setShowVertScrollbar(setting_cpp));
};

// Set whether the horizontal scroll bar should always be shown. 
extern "C" CEGUI_LIB_EXPORT void cegui_cmbbx_setShowHorzScrollbar(struct hg3dclass_struct * thisclass_c, int setting_c)
{
  CEGUI::Combobox * thisclass_cpp = static_cast<CEGUI::Combobox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Combobox"));
  bool setting_cpp = (bool)setting_c;
  (thisclass_cpp->setShowHorzScrollbar(setting_cpp));
};

// Set the select state of an attached ListboxItem
extern "C" CEGUI_LIB_EXPORT void cegui_cmbbx_setItemSelectState(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * item_c, int state_c)
{
  CEGUI::Combobox * thisclass_cpp = static_cast<CEGUI::Combobox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Combobox"));
  CEGUI::ListboxItem * item_cpp = static_cast<CEGUI::ListboxItem*> (getHG3DClassPtr(*item_c, "CEGUI::ListboxItem"));
  bool state_cpp = (bool)state_c;
  (thisclass_cpp->setItemSelectState(item_cpp, state_cpp));
};

// Set the select state of an attached ListboxItem
extern "C" CEGUI_LIB_EXPORT void cegui_cmbbx_setItemSelectState2(struct hg3dclass_struct * thisclass_c, int item_index_c, int state_c)
{
  CEGUI::Combobox * thisclass_cpp = static_cast<CEGUI::Combobox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Combobox"));
  size_t item_index_cpp = (size_t)item_index_c;
  bool state_cpp = (bool)state_c;
  (thisclass_cpp->setItemSelectState(item_index_cpp, state_cpp));
};

// Causes the list box to update it's internal state after changes have been made to one or more attached ListboxItem
extern "C" CEGUI_LIB_EXPORT void cegui_cmbbx_handleUpdatedListItemData(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::Combobox * thisclass_cpp = static_cast<CEGUI::Combobox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Combobox"));
  (thisclass_cpp->handleUpdatedListItemData());
};

// Constructor for Combobox
extern "C" CEGUI_LIB_EXPORT void cegui_cmbbx_construct(char * type_c, char * name_c, struct hg3dclass_struct * result_c)
{
  CEGUI::String type_cpp = CEGUI::String((const char*) type_c);
  CEGUI::String name_cpp = CEGUI::String((const char*) name_c);
  CEGUI::Combobox * result_cpp;
  result_cpp = (new CEGUI::Combobox(type_cpp, name_cpp));
  *result_c = getHG3DClass_Combobox((void *) result_cpp);
;
};

// Destructor for Combobox
extern "C" CEGUI_LIB_EXPORT void cegui_cmbbx_destruct(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::Combobox * thisclass_cpp = static_cast<CEGUI::Combobox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Combobox"));
  (delete thisclass_cpp);
};

