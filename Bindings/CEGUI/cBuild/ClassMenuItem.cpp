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

// ClassMenuItem.cpp

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



// return true if user is hovering over this widget (or it's pushed and user is not over it for highlight) 
extern "C" CEGUI_LIB_EXPORT void cegui_mnitm_isHovering(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::MenuItem * thisclass_cpp = static_cast<CEGUI::MenuItem*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MenuItem"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isHovering());
  *result_c = (int)result_cpp;
};

// Return true if the button widget is in the pushed state. 
extern "C" CEGUI_LIB_EXPORT void cegui_mnitm_isPushed(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::MenuItem * thisclass_cpp = static_cast<CEGUI::MenuItem*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MenuItem"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isPushed());
  *result_c = (int)result_cpp;
};

// Returns true if the popup menu attached to the menu item is open. 
extern "C" CEGUI_LIB_EXPORT void cegui_mnitm_isOpened(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::MenuItem * thisclass_cpp = static_cast<CEGUI::MenuItem*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MenuItem"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isOpened());
  *result_c = (int)result_cpp;
};

// Returns true if the menu item popup is closing or not. 
extern "C" CEGUI_LIB_EXPORT void cegui_mnitm_isPopupClosing(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::MenuItem * thisclass_cpp = static_cast<CEGUI::MenuItem*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MenuItem"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isPopupClosing());
  *result_c = (int)result_cpp;
};

// Returns true if the menu item popup is closed or opened automatically if hovering with the mouse. 
extern "C" CEGUI_LIB_EXPORT void cegui_mnitm_hasAutoPopup(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::MenuItem * thisclass_cpp = static_cast<CEGUI::MenuItem*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MenuItem"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->hasAutoPopup());
  *result_c = (int)result_cpp;
};

// Returns the time, which has to elapse before the popup window is opened/closed if the hovering state changes. 
extern "C" CEGUI_LIB_EXPORT void cegui_mnitm_getAutoPopupTimeout(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  CEGUI::MenuItem * thisclass_cpp = static_cast<CEGUI::MenuItem*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MenuItem"));
  float result_cpp;
  result_cpp = (thisclass_cpp->getAutoPopupTimeout());
  *result_c = (float)result_cpp;
};

// Sets the time, which has to elapse before the popup window is opened/closed if the hovering state changes. 
extern "C" CEGUI_LIB_EXPORT void cegui_mnitm_setAutoPopupTimeout(struct hg3dclass_struct * thisclass_c, float time_c)
{
  CEGUI::MenuItem * thisclass_cpp = static_cast<CEGUI::MenuItem*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MenuItem"));
  float time_cpp = (float)time_c;
  (thisclass_cpp->setAutoPopupTimeout(time_cpp));
};

// Returns the current offset for popup placement. 
extern "C" CEGUI_LIB_EXPORT void cegui_mnitm_getPopupOffset(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  CEGUI::MenuItem * thisclass_cpp = static_cast<CEGUI::MenuItem*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MenuItem"));
  const CEGUI::UVector2 * result_cpp;
  result_cpp = &(thisclass_cpp->getPopupOffset());
  *result_c = getHG3DClass_UVector2((void *) result_cpp);
;
};

// sets the current offset for popup placement. 
extern "C" CEGUI_LIB_EXPORT void cegui_mnitm_setPopupOffset(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * popupOffset_c)
{
  CEGUI::MenuItem * thisclass_cpp = static_cast<CEGUI::MenuItem*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MenuItem"));
  const CEGUI::UVector2 * popupOffset_cpp = static_cast<CEGUI::UVector2*> (getHG3DClassPtr(*popupOffset_c, "CEGUI::UVector2"));
  (thisclass_cpp->setPopupOffset(*popupOffset_cpp));
};

// Opens the PopupMenu. 
extern "C" CEGUI_LIB_EXPORT void cegui_mnitm_openPopupMenu(struct hg3dclass_struct * thisclass_c, int notify_c)
{
  CEGUI::MenuItem * thisclass_cpp = static_cast<CEGUI::MenuItem*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MenuItem"));
  bool notify_cpp = (bool)notify_c;
  (thisclass_cpp->openPopupMenu(notify_cpp));
};

// Closes the PopupMenu. 
extern "C" CEGUI_LIB_EXPORT void cegui_mnitm_closePopupMenu(struct hg3dclass_struct * thisclass_c, int notify_c)
{
  CEGUI::MenuItem * thisclass_cpp = static_cast<CEGUI::MenuItem*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MenuItem"));
  bool notify_cpp = (bool)notify_c;
  (thisclass_cpp->closePopupMenu(notify_cpp));
};

// Toggles the PopupMenu. 
extern "C" CEGUI_LIB_EXPORT void cegui_mnitm_togglePopupMenu(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::MenuItem * thisclass_cpp = static_cast<CEGUI::MenuItem*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MenuItem"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->togglePopupMenu());
  *result_c = (int)result_cpp;
};

// starts the closing timer for the popup, which will close it if the timer is enabled. 
extern "C" CEGUI_LIB_EXPORT void cegui_mnitm_startPopupClosing(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::MenuItem * thisclass_cpp = static_cast<CEGUI::MenuItem*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MenuItem"));
  (thisclass_cpp->startPopupClosing());
};

// starts the opening timer for the popup, which will open it if the timer is enabled. 
extern "C" CEGUI_LIB_EXPORT void cegui_mnitm_startPopupOpening(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::MenuItem * thisclass_cpp = static_cast<CEGUI::MenuItem*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MenuItem"));
  (thisclass_cpp->startPopupOpening());
};

// Constructor for MenuItem
extern "C" CEGUI_LIB_EXPORT void cegui_mnitm_construct(char * type_c, char * name_c, struct hg3dclass_struct * result_c)
{
  CEGUI::String type_cpp = CEGUI::String((const char*) type_c);
  CEGUI::String name_cpp = CEGUI::String((const char*) name_c);
  CEGUI::MenuItem * result_cpp;
  result_cpp = (new CEGUI::MenuItem(type_cpp, name_cpp));
  *result_c = getHG3DClass_MenuItem((void *) result_cpp);
;
};

// Destructor for MenuItem
extern "C" CEGUI_LIB_EXPORT void cegui_mnitm_destruct(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::MenuItem * thisclass_cpp = static_cast<CEGUI::MenuItem*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MenuItem"));
  (delete thisclass_cpp);
};

