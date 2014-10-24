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

// ClassListboxItem.cpp

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



// base class destructor 
extern "C" CEGUI_LIB_EXPORT void cegui_lstbxitm_destruct(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::ListboxItem * thisclass_cpp = static_cast<CEGUI::ListboxItem*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ListboxItem"));
  (delete thisclass_cpp);
};

// return the text string set for this list box item. 
extern "C" CEGUI_LIB_EXPORT void cegui_lstbxitm_getTooltipText(struct hg3dclass_struct * thisclass_c, char * result_c)
{
  CEGUI::ListboxItem * thisclass_cpp = static_cast<CEGUI::ListboxItem*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ListboxItem"));
  CEGUI::String result_cpp;
  result_cpp = (thisclass_cpp->getTooltipText());
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

// 
extern "C" CEGUI_LIB_EXPORT void cegui_lstbxitm_getText(struct hg3dclass_struct * thisclass_c, char * result_c)
{
  CEGUI::ListboxItem * thisclass_cpp = static_cast<CEGUI::ListboxItem*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ListboxItem"));
  CEGUI::String result_cpp;
  result_cpp = (thisclass_cpp->getText());
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

// return text string with visual
extern "C" CEGUI_LIB_EXPORT void cegui_lstbxitm_getTextVisual(struct hg3dclass_struct * thisclass_c, char * result_c)
{
  CEGUI::ListboxItem * thisclass_cpp = static_cast<CEGUI::ListboxItem*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ListboxItem"));
  CEGUI::String result_cpp;
  result_cpp = (thisclass_cpp->getTextVisual());
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

// Return the current ID assigned to this list box item. 
extern "C" CEGUI_LIB_EXPORT void cegui_lstbxitm_getID(struct hg3dclass_struct * thisclass_c, unsigned int * result_c)
{
  CEGUI::ListboxItem * thisclass_cpp = static_cast<CEGUI::ListboxItem*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ListboxItem"));
  uint result_cpp;
  result_cpp = (thisclass_cpp->getID());
  *result_c = (unsigned int)result_cpp;
};

// return whether this item is selected. 
extern "C" CEGUI_LIB_EXPORT void cegui_lstbxitm_isSelected(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::ListboxItem * thisclass_cpp = static_cast<CEGUI::ListboxItem*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ListboxItem"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isSelected());
  *result_c = (int)result_cpp;
};

// return whether this item is disabled. 
extern "C" CEGUI_LIB_EXPORT void cegui_lstbxitm_isDisabled(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::ListboxItem * thisclass_cpp = static_cast<CEGUI::ListboxItem*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ListboxItem"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isDisabled());
  *result_c = (int)result_cpp;
};

// return whether this item will be automatically deleted when the list box it is attached to is destroyed, or when the item is removed from the list box. 
extern "C" CEGUI_LIB_EXPORT void cegui_lstbxitm_isAutoDeleted(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::ListboxItem * thisclass_cpp = static_cast<CEGUI::ListboxItem*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ListboxItem"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isAutoDeleted());
  *result_c = (int)result_cpp;
};

// Get the owner window for this ListboxItem
extern "C" CEGUI_LIB_EXPORT void cegui_lstbxitm_getOwnerWindow(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  CEGUI::ListboxItem * thisclass_cpp = static_cast<CEGUI::ListboxItem*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ListboxItem"));
  const CEGUI::Window * result_cpp;
  result_cpp = (thisclass_cpp->getOwnerWindow());
  *result_c = getHG3DClass_Window((void *) result_cpp);
;
};

// set the text string for this list box item. 
extern "C" CEGUI_LIB_EXPORT void cegui_lstbxitm_setText(struct hg3dclass_struct * thisclass_c, char * text_c)
{
  CEGUI::ListboxItem * thisclass_cpp = static_cast<CEGUI::ListboxItem*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ListboxItem"));
  CEGUI::String text_cpp = CEGUI::String((const char*) text_c);
  (thisclass_cpp->setText(text_cpp));
};

// 
extern "C" CEGUI_LIB_EXPORT void cegui_lstbxitm_setTooltipText(struct hg3dclass_struct * thisclass_c, char * text_c)
{
  CEGUI::ListboxItem * thisclass_cpp = static_cast<CEGUI::ListboxItem*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ListboxItem"));
  CEGUI::String text_cpp = CEGUI::String((const char*) text_c);
  (thisclass_cpp->setTooltipText(text_cpp));
};

// Set the ID assigned to this list box item. 
extern "C" CEGUI_LIB_EXPORT void cegui_lstbxitm_setID(struct hg3dclass_struct * thisclass_c, unsigned int item_id_c)
{
  CEGUI::ListboxItem * thisclass_cpp = static_cast<CEGUI::ListboxItem*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ListboxItem"));
  uint item_id_cpp = (uint)item_id_c;
  (thisclass_cpp->setID(item_id_cpp));
};

// set whether this item is selected. 
extern "C" CEGUI_LIB_EXPORT void cegui_lstbxitm_setSelected(struct hg3dclass_struct * thisclass_c, int setting_c)
{
  CEGUI::ListboxItem * thisclass_cpp = static_cast<CEGUI::ListboxItem*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ListboxItem"));
  bool setting_cpp = (bool)setting_c;
  (thisclass_cpp->setSelected(setting_cpp));
};

// set whether this item is disabled. 
extern "C" CEGUI_LIB_EXPORT void cegui_lstbxitm_setDisabled(struct hg3dclass_struct * thisclass_c, int setting_c)
{
  CEGUI::ListboxItem * thisclass_cpp = static_cast<CEGUI::ListboxItem*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ListboxItem"));
  bool setting_cpp = (bool)setting_c;
  (thisclass_cpp->setDisabled(setting_cpp));
};

// Set whether this item will be automatically deleted when the list box it is attached to is destroyed, or when the item is removed from the list box. 
extern "C" CEGUI_LIB_EXPORT void cegui_lstbxitm_setAutoDeleted(struct hg3dclass_struct * thisclass_c, int setting_c)
{
  CEGUI::ListboxItem * thisclass_cpp = static_cast<CEGUI::ListboxItem*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ListboxItem"));
  bool setting_cpp = (bool)setting_c;
  (thisclass_cpp->setAutoDeleted(setting_cpp));
};

// Set the owner window for this ListboxItem
extern "C" CEGUI_LIB_EXPORT void cegui_lstbxitm_setOwnerWindow(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * owner_c)
{
  CEGUI::ListboxItem * thisclass_cpp = static_cast<CEGUI::ListboxItem*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ListboxItem"));
  const CEGUI::Window * owner_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*owner_c, "CEGUI::Window"));
  (thisclass_cpp->setOwnerWindow(owner_cpp));
};

// Set the selection highlighting brush image. 
extern "C" CEGUI_LIB_EXPORT void cegui_lstbxitm_setSelectionBrushImage2(struct hg3dclass_struct * thisclass_c, char * imageset_c, char * image_c)
{
  CEGUI::ListboxItem * thisclass_cpp = static_cast<CEGUI::ListboxItem*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ListboxItem"));
  CEGUI::String imageset_cpp = CEGUI::String((const char*) imageset_c);
  CEGUI::String image_cpp = CEGUI::String((const char*) image_c);
  (thisclass_cpp->setSelectionBrushImage(imageset_cpp, image_cpp));
};

