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

// ClassItemListbox.cpp

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



// Returns the number of selected items in this ItemListbox
extern "C" CEGUI_LIB_EXPORT void cegui_itmlstbx_getSelectedCount(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::ItemListbox * thisclass_cpp = static_cast<CEGUI::ItemListbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ItemListbox"));
  size_t result_cpp;
  result_cpp = (thisclass_cpp->getSelectedCount());
  *result_c = (int)result_cpp;
};

// Returns a pointer to the last selected item. 
extern "C" CEGUI_LIB_EXPORT void cegui_itmlstbx_getLastSelectedItem(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  CEGUI::ItemListbox * thisclass_cpp = static_cast<CEGUI::ItemListbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ItemListbox"));
  CEGUI::ItemEntry * result_cpp;
  result_cpp = (thisclass_cpp->getLastSelectedItem());
  *result_c = getHG3DClass_ItemEntry((void *) result_cpp);
;
};

// Returns a pointer to the first selected item. 
extern "C" CEGUI_LIB_EXPORT void cegui_itmlstbx_getFirstSelectedItem(struct hg3dclass_struct * thisclass_c, int start_index_c, struct hg3dclass_struct * result_c)
{
  CEGUI::ItemListbox * thisclass_cpp = static_cast<CEGUI::ItemListbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ItemListbox"));
  size_t start_index_cpp = (size_t)start_index_c;
  CEGUI::ItemEntry * result_cpp;
  result_cpp = (thisclass_cpp->getFirstSelectedItem(start_index_cpp));
  *result_c = getHG3DClass_ItemEntry((void *) result_cpp);
;
};

// Returns a pointer to the next seleced item relative to a previous call to getFirstSelectedItem or getNextSelectedItem. 
extern "C" CEGUI_LIB_EXPORT void cegui_itmlstbx_getNextSelectedItem(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  CEGUI::ItemListbox * thisclass_cpp = static_cast<CEGUI::ItemListbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ItemListbox"));
  CEGUI::ItemEntry * result_cpp;
  result_cpp = (thisclass_cpp->getNextSelectedItem());
  *result_c = getHG3DClass_ItemEntry((void *) result_cpp);
;
};

// Returns a pointer to the next selected item after the item 'start_item' given. 
extern "C" CEGUI_LIB_EXPORT void cegui_itmlstbx_getNextSelectedItemAfter(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * start_item_c, struct hg3dclass_struct * result_c)
{
  CEGUI::ItemListbox * thisclass_cpp = static_cast<CEGUI::ItemListbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ItemListbox"));
  const CEGUI::ItemEntry * start_item_cpp = static_cast<CEGUI::ItemEntry*> (getHG3DClassPtr(*start_item_c, "CEGUI::ItemEntry"));
  CEGUI::ItemEntry * result_cpp;
  result_cpp = (thisclass_cpp->getNextSelectedItemAfter(start_item_cpp));
  *result_c = getHG3DClass_ItemEntry((void *) result_cpp);
;
};

// Returns 'true' if multiple selections are allowed. 'false' if not. 
extern "C" CEGUI_LIB_EXPORT void cegui_itmlstbx_isMultiSelectEnabled(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::ItemListbox * thisclass_cpp = static_cast<CEGUI::ItemListbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ItemListbox"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isMultiSelectEnabled());
  *result_c = (int)result_cpp;
};

// Returns 'true' if the item at the given index is selectable and currently selected. 
extern "C" CEGUI_LIB_EXPORT void cegui_itmlstbx_isItemSelected(struct hg3dclass_struct * thisclass_c, int index_c, int * result_c)
{
  CEGUI::ItemListbox * thisclass_cpp = static_cast<CEGUI::ItemListbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ItemListbox"));
  size_t index_cpp = (size_t)index_c;
  bool result_cpp;
  result_cpp = (thisclass_cpp->isItemSelected(index_cpp));
  *result_c = (int)result_cpp;
};

// 
extern "C" CEGUI_LIB_EXPORT void cegui_itmlstbx_initialiseComponents(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::ItemListbox * thisclass_cpp = static_cast<CEGUI::ItemListbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ItemListbox"));
  (thisclass_cpp->initialiseComponents());
};

// Set whether or not multiple selections should be allowed. 
extern "C" CEGUI_LIB_EXPORT void cegui_itmlstbx_setMultiSelectEnabled(struct hg3dclass_struct * thisclass_c, int state_c)
{
  CEGUI::ItemListbox * thisclass_cpp = static_cast<CEGUI::ItemListbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ItemListbox"));
  bool state_cpp = (bool)state_c;
  (thisclass_cpp->setMultiSelectEnabled(state_cpp));
};

// Clears all selections. 
extern "C" CEGUI_LIB_EXPORT void cegui_itmlstbx_clearAllSelections(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::ItemListbox * thisclass_cpp = static_cast<CEGUI::ItemListbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ItemListbox"));
  (thisclass_cpp->clearAllSelections());
};

// Select a range of items. 
extern "C" CEGUI_LIB_EXPORT void cegui_itmlstbx_selectRange(struct hg3dclass_struct * thisclass_c, int a_c, int z_c)
{
  CEGUI::ItemListbox * thisclass_cpp = static_cast<CEGUI::ItemListbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ItemListbox"));
  size_t a_cpp = (size_t)a_c;
  size_t z_cpp = (size_t)z_c;
  (thisclass_cpp->selectRange(a_cpp, z_cpp));
};

// Select all items. Does nothing if multiselect is disabled. 
extern "C" CEGUI_LIB_EXPORT void cegui_itmlstbx_selectAllItems(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::ItemListbox * thisclass_cpp = static_cast<CEGUI::ItemListbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ItemListbox"));
  (thisclass_cpp->selectAllItems());
};

// Constructor for the ItemListbox
extern "C" CEGUI_LIB_EXPORT void cegui_itmlstbx_construct(char * type_c, char * name_c, struct hg3dclass_struct * result_c)
{
  CEGUI::String type_cpp = CEGUI::String((const char*) type_c);
  CEGUI::String name_cpp = CEGUI::String((const char*) name_c);
  CEGUI::ItemListbox * result_cpp;
  result_cpp = (new CEGUI::ItemListbox(type_cpp, name_cpp));
  *result_c = getHG3DClass_ItemListbox((void *) result_cpp);
;
};

// Destructor for the ItemListbox
extern "C" CEGUI_LIB_EXPORT void cegui_itmlstbx_destruct(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::ItemListbox * thisclass_cpp = static_cast<CEGUI::ItemListbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ItemListbox"));
  (delete thisclass_cpp);
};

// Setup size and position for the item widgets attached to this ItemListbox
extern "C" CEGUI_LIB_EXPORT void cegui_itmlstbx_layoutItemWidgets(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::ItemListbox * thisclass_cpp = static_cast<CEGUI::ItemListbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ItemListbox"));
  (thisclass_cpp->layoutItemWidgets());
};

// Return whether this window was inherited from the given class name at some point in the inheritance hierarchy. 
extern "C" CEGUI_LIB_EXPORT void cegui_itmlstbx_testClassName_impl(struct hg3dclass_struct * thisclass_c, char * class_name_c, int * result_c)
{
  CEGUI::ItemListbox * thisclass_cpp = static_cast<CEGUI::ItemListbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ItemListbox"));
  CEGUI::String class_name_cpp = CEGUI::String((const char*) class_name_c);
  bool result_cpp;
  result_cpp = (thisclass_cpp->testClassName_impl(class_name_cpp));
  *result_c = (int)result_cpp;
};

// Notify this ItemListbox
extern "C" CEGUI_LIB_EXPORT void cegui_itmlstbx_notifyItemClicked(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * li_c)
{
  CEGUI::ItemListbox * thisclass_cpp = static_cast<CEGUI::ItemListbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ItemListbox"));
  CEGUI::ItemEntry * li_cpp = static_cast<CEGUI::ItemEntry*> (getHG3DClassPtr(*li_c, "CEGUI::ItemEntry"));
  (thisclass_cpp->notifyItemClicked(li_cpp));
};

// Notify this ItemListbox
extern "C" CEGUI_LIB_EXPORT void cegui_itmlstbx_notifyItemSelectState(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * li_c, int state_c)
{
  CEGUI::ItemListbox * thisclass_cpp = static_cast<CEGUI::ItemListbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ItemListbox"));
  CEGUI::ItemEntry * li_cpp = static_cast<CEGUI::ItemEntry*> (getHG3DClassPtr(*li_c, "CEGUI::ItemEntry"));
  bool state_cpp = (bool)state_c;
  (thisclass_cpp->notifyItemSelectState(li_cpp, state_cpp));
};

