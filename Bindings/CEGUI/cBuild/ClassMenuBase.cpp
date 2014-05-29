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

// ClassMenuBase.cpp

// abstrakte Klasse!

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



// Get the item spacing for this menu. 
extern "C" CEGUI_LIB_EXPORT void cegui_mnbs_getItemSpacing(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  CEGUI::MenuBase * thisclass_cpp = static_cast<CEGUI::MenuBase*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MenuBase"));
  float result_cpp;
  result_cpp = (thisclass_cpp->getItemSpacing());
  *result_c = (float)result_cpp;
};

// Return whether this menu allows multiple popup menus to open at the same time. 
extern "C" CEGUI_LIB_EXPORT void cegui_mnbs_isMultiplePopupsAllowed(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::MenuBase * thisclass_cpp = static_cast<CEGUI::MenuBase*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MenuBase"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isMultiplePopupsAllowed());
  *result_c = (int)result_cpp;
};

// Return whether this menu should close all its open child popups, when it gets hidden. 
extern "C" CEGUI_LIB_EXPORT void cegui_mnbs_getAutoCloseNestedPopups(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::MenuBase * thisclass_cpp = static_cast<CEGUI::MenuBase*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MenuBase"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->getAutoCloseNestedPopups());
  *result_c = (int)result_cpp;
};

// Get currently opened MenuItem
extern "C" CEGUI_LIB_EXPORT void cegui_mnbs_getPopupMenuItem(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  CEGUI::MenuBase * thisclass_cpp = static_cast<CEGUI::MenuBase*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MenuBase"));
  CEGUI::MenuItem * result_cpp;
  result_cpp = (thisclass_cpp->getPopupMenuItem());
  *result_c = getHG3DClass_MenuItem((void *) result_cpp);
;
};

// Set the item spacing for this menu. 
extern "C" CEGUI_LIB_EXPORT void cegui_mnbs_setItemSpacing(struct hg3dclass_struct * thisclass_c, float spacing_c)
{
  CEGUI::MenuBase * thisclass_cpp = static_cast<CEGUI::MenuBase*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MenuBase"));
  float spacing_cpp = (float)spacing_c;
  (thisclass_cpp->setItemSpacing(spacing_cpp));
};

// Change the currently open MenuItem
extern "C" CEGUI_LIB_EXPORT void cegui_mnbs_changePopupMenuItem(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * item_c)
{
  CEGUI::MenuBase * thisclass_cpp = static_cast<CEGUI::MenuBase*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MenuBase"));
  CEGUI::MenuItem * item_cpp = static_cast<CEGUI::MenuItem*> (getHG3DClassPtr(*item_c, "CEGUI::MenuItem"));
  (thisclass_cpp->changePopupMenuItem(item_cpp));
};

// Set whether this menu allows multiple popup menus to be opened simultaneously. 
extern "C" CEGUI_LIB_EXPORT void cegui_mnbs_setAllowMultiplePopups(struct hg3dclass_struct * thisclass_c, int setting_c)
{
  CEGUI::MenuBase * thisclass_cpp = static_cast<CEGUI::MenuBase*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MenuBase"));
  bool setting_cpp = (bool)setting_c;
  (thisclass_cpp->setAllowMultiplePopups(setting_cpp));
};

// Set whether the menu should close all its open child popups, when it gets hidden. 
extern "C" CEGUI_LIB_EXPORT void cegui_mnbs_setAutoCloseNestedPopups(struct hg3dclass_struct * thisclass_c, int setting_c)
{
  CEGUI::MenuBase * thisclass_cpp = static_cast<CEGUI::MenuBase*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MenuBase"));
  bool setting_cpp = (bool)setting_c;
  (thisclass_cpp->setAutoCloseNestedPopups(setting_cpp));
};

// tells the current popup that it should start its closing timer. 
extern "C" CEGUI_LIB_EXPORT void cegui_mnbs_setPopupMenuItemClosing(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::MenuBase * thisclass_cpp = static_cast<CEGUI::MenuBase*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MenuBase"));
  (thisclass_cpp->setPopupMenuItemClosing());
};

