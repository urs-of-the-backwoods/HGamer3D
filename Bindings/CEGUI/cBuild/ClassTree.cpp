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

// ClassTree.cpp

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
#include "HG3DEventController.h"
#include "HG3DEventModule.h"
#include "HG3DEventStaticFunctions.h"
#include "HG3DListboxStaticFunctions.h"
#include "HG3DWindowStaticFunctions.h"

using namespace CEGUI;



// 
extern "C" CEGUI_LIB_EXPORT void cegui_tree_doTreeRender(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::Tree * thisclass_cpp = static_cast<CEGUI::Tree*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Tree"));
  (thisclass_cpp->doTreeRender());
};

// 
extern "C" CEGUI_LIB_EXPORT void cegui_tree_doScrollbars(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::Tree * thisclass_cpp = static_cast<CEGUI::Tree*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Tree"));
  (thisclass_cpp->doScrollbars());
};

// Return number of items attached to the tree. 
extern "C" CEGUI_LIB_EXPORT void cegui_tree_getItemCount(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::Tree * thisclass_cpp = static_cast<CEGUI::Tree*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Tree"));
  size_t result_cpp;
  result_cpp = (thisclass_cpp->getItemCount());
  *result_c = (int)result_cpp;
};

// Return the number of selected items in the tree. 
extern "C" CEGUI_LIB_EXPORT void cegui_tree_getSelectedCount(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::Tree * thisclass_cpp = static_cast<CEGUI::Tree*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Tree"));
  size_t result_cpp;
  result_cpp = (thisclass_cpp->getSelectedCount());
  *result_c = (int)result_cpp;
};

// return whether tree sorting is enabled 
extern "C" CEGUI_LIB_EXPORT void cegui_tree_isSortEnabled(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::Tree * thisclass_cpp = static_cast<CEGUI::Tree*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Tree"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isSortEnabled());
  *result_c = (int)result_cpp;
};

// 
extern "C" CEGUI_LIB_EXPORT void cegui_tree_getVertScrollbar(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  CEGUI::Tree * thisclass_cpp = static_cast<CEGUI::Tree*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Tree"));
  CEGUI::Scrollbar * result_cpp;
  result_cpp = (thisclass_cpp->getVertScrollbar());
  *result_c = getHG3DClass_Scrollbar((void *) result_cpp);
;
};

// 
extern "C" CEGUI_LIB_EXPORT void cegui_tree_getHorzScrollbar(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  CEGUI::Tree * thisclass_cpp = static_cast<CEGUI::Tree*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Tree"));
  CEGUI::Scrollbar * result_cpp;
  result_cpp = (thisclass_cpp->getHorzScrollbar());
  *result_c = getHG3DClass_Scrollbar((void *) result_cpp);
;
};

// return whether multi-select is enabled 
extern "C" CEGUI_LIB_EXPORT void cegui_tree_isMultiselectEnabled(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::Tree * thisclass_cpp = static_cast<CEGUI::Tree*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Tree"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isMultiselectEnabled());
  *result_c = (int)result_cpp;
};

// 
extern "C" CEGUI_LIB_EXPORT void cegui_tree_isItemTooltipsEnabled(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::Tree * thisclass_cpp = static_cast<CEGUI::Tree*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Tree"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isItemTooltipsEnabled());
  *result_c = (int)result_cpp;
};

// Return whether the vertical scroll bar is always shown. 
extern "C" CEGUI_LIB_EXPORT void cegui_tree_isVertScrollbarAlwaysShown(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::Tree * thisclass_cpp = static_cast<CEGUI::Tree*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Tree"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isVertScrollbarAlwaysShown());
  *result_c = (int)result_cpp;
};

// Return whether the horizontal scroll bar is always shown. 
extern "C" CEGUI_LIB_EXPORT void cegui_tree_isHorzScrollbarAlwaysShown(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::Tree * thisclass_cpp = static_cast<CEGUI::Tree*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Tree"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isHorzScrollbarAlwaysShown());
  *result_c = (int)result_cpp;
};

// Initialise the Window
extern "C" CEGUI_LIB_EXPORT void cegui_tree_initialise(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::Tree * thisclass_cpp = static_cast<CEGUI::Tree*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Tree"));
  (thisclass_cpp->initialise());
};

// Remove all items from the tree. 
extern "C" CEGUI_LIB_EXPORT void cegui_tree_resetList(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::Tree * thisclass_cpp = static_cast<CEGUI::Tree*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Tree"));
  (thisclass_cpp->resetList());
};

// Clear the selected state for all items. 
extern "C" CEGUI_LIB_EXPORT void cegui_tree_clearAllSelections(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::Tree * thisclass_cpp = static_cast<CEGUI::Tree*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Tree"));
  (thisclass_cpp->clearAllSelections());
};

// Set whether the tree should be sorted. 
extern "C" CEGUI_LIB_EXPORT void cegui_tree_setSortingEnabled(struct hg3dclass_struct * thisclass_c, int setting_c)
{
  CEGUI::Tree * thisclass_cpp = static_cast<CEGUI::Tree*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Tree"));
  bool setting_cpp = (bool)setting_c;
  (thisclass_cpp->setSortingEnabled(setting_cpp));
};

// Set whether the tree should allow multiple selections or just a single selection. 
extern "C" CEGUI_LIB_EXPORT void cegui_tree_setMultiselectEnabled(struct hg3dclass_struct * thisclass_c, int setting_c)
{
  CEGUI::Tree * thisclass_cpp = static_cast<CEGUI::Tree*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Tree"));
  bool setting_cpp = (bool)setting_c;
  (thisclass_cpp->setMultiselectEnabled(setting_cpp));
};

// Set whether the vertical scroll bar should always be shown. 
extern "C" CEGUI_LIB_EXPORT void cegui_tree_setShowVertScrollbar(struct hg3dclass_struct * thisclass_c, int setting_c)
{
  CEGUI::Tree * thisclass_cpp = static_cast<CEGUI::Tree*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Tree"));
  bool setting_cpp = (bool)setting_c;
  (thisclass_cpp->setShowVertScrollbar(setting_cpp));
};

// Set whether the horizontal scroll bar should always be shown. 
extern "C" CEGUI_LIB_EXPORT void cegui_tree_setShowHorzScrollbar(struct hg3dclass_struct * thisclass_c, int setting_c)
{
  CEGUI::Tree * thisclass_cpp = static_cast<CEGUI::Tree*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Tree"));
  bool setting_cpp = (bool)setting_c;
  (thisclass_cpp->setShowHorzScrollbar(setting_cpp));
};

// 
extern "C" CEGUI_LIB_EXPORT void cegui_tree_setItemTooltipsEnabled(struct hg3dclass_struct * thisclass_c, int setting_c)
{
  CEGUI::Tree * thisclass_cpp = static_cast<CEGUI::Tree*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Tree"));
  bool setting_cpp = (bool)setting_c;
  (thisclass_cpp->setItemTooltipsEnabled(setting_cpp));
};

// Set the select state of an attached TreeItem. 
extern "C" CEGUI_LIB_EXPORT void cegui_tree_setItemSelectState2(struct hg3dclass_struct * thisclass_c, int item_index_c, int state_c)
{
  CEGUI::Tree * thisclass_cpp = static_cast<CEGUI::Tree*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Tree"));
  size_t item_index_cpp = (size_t)item_index_c;
  bool state_cpp = (bool)state_c;
  (thisclass_cpp->setItemSelectState(item_index_cpp, state_cpp));
};

// Set the LookNFeel that shoule be used for this window. 
extern "C" CEGUI_LIB_EXPORT void cegui_tree_setLookNFeel(struct hg3dclass_struct * thisclass_c, char * look_c)
{
  CEGUI::Tree * thisclass_cpp = static_cast<CEGUI::Tree*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Tree"));
  CEGUI::String look_cpp = CEGUI::String((const char*) look_c);
  (thisclass_cpp->setLookNFeel(look_cpp));
};

// Causes the tree to update it's internal state after changes have been made to one or more attached TreeItem objects. 
extern "C" CEGUI_LIB_EXPORT void cegui_tree_handleUpdatedItemData(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::Tree * thisclass_cpp = static_cast<CEGUI::Tree*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Tree"));
  (thisclass_cpp->handleUpdatedItemData());
};

// Constructor for Tree
extern "C" CEGUI_LIB_EXPORT void cegui_tree_construct(char * type_c, char * name_c, struct hg3dclass_struct * result_c)
{
  CEGUI::String type_cpp = CEGUI::String((const char*) type_c);
  CEGUI::String name_cpp = CEGUI::String((const char*) name_c);
  CEGUI::Tree * result_cpp;
  result_cpp = (new CEGUI::Tree(type_cpp, name_cpp));
  *result_c = getHG3DClass_Tree((void *) result_cpp);
;
};

// Destructor for Tree
extern "C" CEGUI_LIB_EXPORT void cegui_tree_destruct(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::Tree * thisclass_cpp = static_cast<CEGUI::Tree*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Tree"));
  (delete thisclass_cpp);
};

