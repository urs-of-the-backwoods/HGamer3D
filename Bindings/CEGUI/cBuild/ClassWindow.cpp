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

// ClassWindow.cpp

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
	#include "EnumVerticalAlignment.h"
#include "EnumHorizontalAlignment.h"
#include "EnumWindowUpdateMode.h"
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



// Constructor for Window
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_construct(char * type_c, char * name_c, struct hg3dclass_struct * result_c)
{
  CEGUI::String type_cpp = CEGUI::String((const char*) type_c);
  CEGUI::String name_cpp = CEGUI::String((const char*) name_c);
  CEGUI::Window * result_cpp;
  result_cpp = (new CEGUI::Window(type_cpp, name_cpp));
  *result_c = getHG3DClass_Window((void *) result_cpp);
;
};

// Destructor for Window
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_destruct(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  (delete thisclass_cpp);
};

// return a String object holding the type name for this Window
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_getType(struct hg3dclass_struct * thisclass_c, char * result_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  CEGUI::String result_cpp;
  result_cpp = (thisclass_cpp->getType());
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

// return a String object holding the name of this Window
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_getName(struct hg3dclass_struct * thisclass_c, char * result_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  CEGUI::String result_cpp;
  result_cpp = (thisclass_cpp->getName());
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

// returns whether or not this Window
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_isDestroyedByParent(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isDestroyedByParent());
  *result_c = (int)result_cpp;
};

// returns whether or not this WindowWindow
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_isAlwaysOnTop(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isAlwaysOnTop());
  *result_c = (int)result_cpp;
};

// return whether the Window
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_isDisabled(struct hg3dclass_struct * thisclass_c, int localOnly_c, int * result_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  bool localOnly_cpp = (bool)localOnly_c;
  bool result_cpp;
  result_cpp = (thisclass_cpp->isDisabled(localOnly_cpp));
  *result_c = (int)result_cpp;
};

// return true if the Window
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_isVisible(struct hg3dclass_struct * thisclass_c, int localOnly_c, int * result_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  bool localOnly_cpp = (bool)localOnly_c;
  bool result_cpp;
  result_cpp = (thisclass_cpp->isVisible(localOnly_cpp));
  *result_c = (int)result_cpp;
};

// return true if this is the active Window
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_isActive(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isActive());
  *result_c = (int)result_cpp;
};

// return true if this WindowWindow
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_isClippedByParent(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isClippedByParent());
  *result_c = (int)result_cpp;
};

// return the ID code currently assigned to this Window
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_getID(struct hg3dclass_struct * thisclass_c, unsigned int * result_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  uint result_cpp;
  result_cpp = (thisclass_cpp->getID());
  *result_c = (unsigned int)result_cpp;
};

// return the number of child WindowWindow
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_getChildCount(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  size_t result_cpp;
  result_cpp = (thisclass_cpp->getChildCount());
  *result_c = (int)result_cpp;
};

// returns whether a WindowWindow
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_isChild(struct hg3dclass_struct * thisclass_c, char * name_c, int * result_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  CEGUI::String name_cpp = CEGUI::String((const char*) name_c);
  bool result_cpp;
  result_cpp = (thisclass_cpp->isChild(name_cpp));
  *result_c = (int)result_cpp;
};

// returns whether at least one window with the given ID code is attached to this Window
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_isChild2(struct hg3dclass_struct * thisclass_c, unsigned int ID_c, int * result_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  uint ID_cpp = (uint)ID_c;
  bool result_cpp;
  result_cpp = (thisclass_cpp->isChild(ID_cpp));
  *result_c = (int)result_cpp;
};

// returns whether at least one window with the given ID code is attached to this Window
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_isChildRecursive(struct hg3dclass_struct * thisclass_c, unsigned int ID_c, int * result_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  uint ID_cpp = (uint)ID_c;
  bool result_cpp;
  result_cpp = (thisclass_cpp->isChildRecursive(ID_cpp));
  *result_c = (int)result_cpp;
};

// return true if the given Window
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_isChild3(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * window_c, int * result_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  const CEGUI::Window * window_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*window_c, "CEGUI::Window"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isChild(window_cpp));
  *result_c = (int)result_cpp;
};

// return a pointer to the child window with the specified name. 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_getChild(struct hg3dclass_struct * thisclass_c, char * name_c, struct hg3dclass_struct * result_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  CEGUI::String name_cpp = CEGUI::String((const char*) name_c);
  CEGUI::Window * result_cpp;
  result_cpp = (thisclass_cpp->getChild(name_cpp));
  *result_c = getHG3DClass_Window((void *) result_cpp);
;
};

// return a pointer to the first attached child window with the specified ID value. 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_getChild2(struct hg3dclass_struct * thisclass_c, unsigned int ID_c, struct hg3dclass_struct * result_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  uint ID_cpp = (uint)ID_c;
  CEGUI::Window * result_cpp;
  result_cpp = (thisclass_cpp->getChild(ID_cpp));
  *result_c = getHG3DClass_Window((void *) result_cpp);
;
};

// return a pointer to the first attached child window with the specified name. Children are traversed recursively. 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_getChildRecursive(struct hg3dclass_struct * thisclass_c, char * name_c, struct hg3dclass_struct * result_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  CEGUI::String name_cpp = CEGUI::String((const char*) name_c);
  CEGUI::Window * result_cpp;
  result_cpp = (thisclass_cpp->getChildRecursive(name_cpp));
  *result_c = getHG3DClass_Window((void *) result_cpp);
;
};

// return a pointer to the first attached child window with the specified ID value. Children are traversed recursively. 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_getChildRecursive2(struct hg3dclass_struct * thisclass_c, unsigned int ID_c, struct hg3dclass_struct * result_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  uint ID_cpp = (uint)ID_c;
  CEGUI::Window * result_cpp;
  result_cpp = (thisclass_cpp->getChildRecursive(ID_cpp));
  *result_c = getHG3DClass_Window((void *) result_cpp);
;
};

// return a pointer to the child window that is attached to 'this' at the given index. 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_getChildAtIdx(struct hg3dclass_struct * thisclass_c, int idx_c, struct hg3dclass_struct * result_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  size_t idx_cpp = (size_t)idx_c;
  CEGUI::Window * result_cpp;
  result_cpp = (thisclass_cpp->getChildAtIdx(idx_cpp));
  *result_c = getHG3DClass_Window((void *) result_cpp);
;
};

// return a pointer to the WindowWindow
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_getActiveChild(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  CEGUI::Window * result_cpp;
  result_cpp = (thisclass_cpp->getActiveChild());
  *result_c = getHG3DClass_Window((void *) result_cpp);
;
};

// 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_getActiveChild2(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  const CEGUI::Window * result_cpp;
  result_cpp = (thisclass_cpp->getActiveChild());
  *result_c = getHG3DClass_Window((void *) result_cpp);
;
};

// return true if the specified WindowWindow
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_isAncestor(struct hg3dclass_struct * thisclass_c, char * name_c, int * result_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  CEGUI::String name_cpp = CEGUI::String((const char*) name_c);
  bool result_cpp;
  result_cpp = (thisclass_cpp->isAncestor(name_cpp));
  *result_c = (int)result_cpp;
};

// return true if any WindowWindow
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_isAncestor2(struct hg3dclass_struct * thisclass_c, unsigned int ID_c, int * result_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  uint ID_cpp = (uint)ID_c;
  bool result_cpp;
  result_cpp = (thisclass_cpp->isAncestor(ID_cpp));
  *result_c = (int)result_cpp;
};

// return true if the specified WindowWindow
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_isAncestor3(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * window_c, int * result_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  const CEGUI::Window * window_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*window_c, "CEGUI::Window"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isAncestor(window_cpp));
  *result_c = (int)result_cpp;
};

// return the active FontWindow
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_getFont(struct hg3dclass_struct * thisclass_c, int useDefault_c, struct hg3dclass_struct * result_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  bool useDefault_cpp = (bool)useDefault_c;
  CEGUI::Font * result_cpp;
  result_cpp = (thisclass_cpp->getFont(useDefault_cpp));
  *result_c = getHG3DClass_Font((void *) result_cpp);
;
};

// return the current text for the Window
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_getText(struct hg3dclass_struct * thisclass_c, char * result_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  CEGUI::String result_cpp;
  result_cpp = (thisclass_cpp->getText());
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

// return text string with visual
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_getTextVisual(struct hg3dclass_struct * thisclass_c, char * result_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  CEGUI::String result_cpp;
  result_cpp = (thisclass_cpp->getTextVisual());
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

// return true if the Window
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_inheritsAlpha(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->inheritsAlpha());
  *result_c = (int)result_cpp;
};

// return the current alpha value set for this Window
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_getAlpha(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  float result_cpp;
  result_cpp = (thisclass_cpp->getAlpha());
  *result_c = (float)result_cpp;
};

// return the effective alpha value that will be used when rendering this window, taking into account inheritance of parent window(s) alpha. 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_getEffectiveAlpha(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  float result_cpp;
  result_cpp = (thisclass_cpp->getEffectiveAlpha());
  *result_c = (float)result_cpp;
};

// return true if this Window
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_isCapturedByThis(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isCapturedByThis());
  *result_c = (int)result_cpp;
};

// return true if an ancestor window has captured inputs. 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_isCapturedByAncestor(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isCapturedByAncestor());
  *result_c = (int)result_cpp;
};

// return true if a child window has captured inputs. 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_isCapturedByChild(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isCapturedByChild());
  *result_c = (int)result_cpp;
};

// return the parent of this Window
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_getParent(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  CEGUI::Window * result_cpp;
  result_cpp = (thisclass_cpp->getParent());
  *result_c = getHG3DClass_Window((void *) result_cpp);
;
};

// Return whether this window is set to restore old input capture when it loses input capture. 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_restoresOldCapture(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->restoresOldCapture());
  *result_c = (int)result_cpp;
};

// Return whether z-order changes are enabled or disabled for this Window
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_isZOrderingEnabled(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isZOrderingEnabled());
  *result_c = (int)result_cpp;
};

// Return whether this window will receive multi-click events or multiple 'down' events instead. 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_wantsMultiClickEvents(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->wantsMultiClickEvents());
  *result_c = (int)result_cpp;
};

// Return whether mouse button down event autorepeat is enabled for this window. 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_isMouseAutoRepeatEnabled(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isMouseAutoRepeatEnabled());
  *result_c = (int)result_cpp;
};

// Return the current auto-repeat delay setting for this window. 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_getAutoRepeatDelay(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  float result_cpp;
  result_cpp = (thisclass_cpp->getAutoRepeatDelay());
  *result_c = (float)result_cpp;
};

// Return the current auto-repeat rate setting for this window. 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_getAutoRepeatRate(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  float result_cpp;
  result_cpp = (thisclass_cpp->getAutoRepeatRate());
  *result_c = (float)result_cpp;
};

// Return whether the window wants inputs passed to its attached child windows when the window has inputs captured. 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_distributesCapturedInputs(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->distributesCapturedInputs());
  *result_c = (int)result_cpp;
};

// Return whether this WindowTooltipTooltip
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_isUsingDefaultTooltip(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isUsingDefaultTooltip());
  *result_c = (int)result_cpp;
};

// Return a pointer to the TooltipWindowTooltipWindowTooltip
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_getTooltip(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  CEGUI::Tooltip * result_cpp;
  result_cpp = (thisclass_cpp->getTooltip());
  *result_c = getHG3DClass_Tooltip((void *) result_cpp);
;
};

// Return the custom tooltip type. 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_getTooltipType(struct hg3dclass_struct * thisclass_c, char * result_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  CEGUI::String result_cpp;
  result_cpp = (thisclass_cpp->getTooltipType());
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

// Return the current tooltip text set for this Window
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_getTooltipText(struct hg3dclass_struct * thisclass_c, char * result_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  CEGUI::String result_cpp;
  result_cpp = (thisclass_cpp->getTooltipText());
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

// Return whether this window inherits Tooltip
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_inheritsTooltipText(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->inheritsTooltipText());
  *result_c = (int)result_cpp;
};

// Return whether this window will rise to the top of the z-order when clicked with the left mouse button. 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_isRiseOnClickEnabled(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isRiseOnClickEnabled());
  *result_c = (int)result_cpp;
};

// Return whether this window was inherited from the given class name at some point in the inheritance hierarchy. 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_testClassName(struct hg3dclass_struct * thisclass_c, char * class_name_c, int * result_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  CEGUI::String class_name_cpp = CEGUI::String((const char*) class_name_c);
  bool result_cpp;
  result_cpp = (thisclass_cpp->testClassName(class_name_cpp));
  *result_c = (int)result_cpp;
};

// Get the vertical alignment. 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_getVerticalAlignment(struct hg3dclass_struct * thisclass_c, enum EnumVerticalAlignment * result_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  enum CEGUI::VerticalAlignment result_cpp;
  result_cpp = (thisclass_cpp->getVerticalAlignment());
  *result_c = (enum EnumVerticalAlignment) result_cpp;
};

// Get the horizontal alignment. 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_getHorizontalAlignment(struct hg3dclass_struct * thisclass_c, enum EnumHorizontalAlignment * result_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  enum CEGUI::HorizontalAlignment result_cpp;
  result_cpp = (thisclass_cpp->getHorizontalAlignment());
  *result_c = (enum EnumHorizontalAlignment) result_cpp;
};

// Get the name of the LookNFeel assigned to this window. 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_getLookNFeel(struct hg3dclass_struct * thisclass_c, char * result_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  CEGUI::String result_cpp;
  result_cpp = (thisclass_cpp->getLookNFeel());
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

// Get whether or not this Window
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_getModalState(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->getModalState());
  *result_c = (int)result_cpp;
};

// Returns a named user string. 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_getUserString(struct hg3dclass_struct * thisclass_c, char * name_c, char * result_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  CEGUI::String name_cpp = CEGUI::String((const char*) name_c);
  CEGUI::String result_cpp;
  result_cpp = (thisclass_cpp->getUserString(name_cpp));
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

// Return whether a user string with the specified name exists. 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_isUserStringDefined(struct hg3dclass_struct * thisclass_c, char * name_c, int * result_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  CEGUI::String name_cpp = CEGUI::String((const char*) name_c);
  bool result_cpp;
  result_cpp = (thisclass_cpp->isUserStringDefined(name_cpp));
  *result_c = (int)result_cpp;
};

// Returns the active sibling window. 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_getActiveSibling(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  CEGUI::Window * result_cpp;
  result_cpp = (thisclass_cpp->getActiveSibling());
  *result_c = getHG3DClass_Window((void *) result_cpp);
;
};

// Return the pixel Width of the parent element. This always returns a valid number. 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_getParentPixelWidth(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  float result_cpp;
  result_cpp = (thisclass_cpp->getParentPixelWidth());
  *result_c = (float)result_cpp;
};

// Return the pixel Height of the parent element. This always returns a valid number. 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_getParentPixelHeight(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  float result_cpp;
  result_cpp = (thisclass_cpp->getParentPixelHeight());
  *result_c = (float)result_cpp;
};

// Returns whether this window should ignore mouse event and pass them through to and other windows behind it. In effect making the window transparent to the mouse. 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_isMousePassThroughEnabled(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isMousePassThroughEnabled());
  *result_c = (int)result_cpp;
};

// Returns whether this window is an auto-child window. All auto-child windows have "__auto_" in their name, but this is faster. 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_isAutoWindow(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isAutoWindow());
  *result_c = (int)result_cpp;
};

// Returns whether this window is allowed to write XML. 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_isWritingXMLAllowed(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isWritingXMLAllowed());
  *result_c = (int)result_cpp;
};

// Returns whether this Window
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_isDragDropTarget(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isDragDropTarget());
  *result_c = (int)result_cpp;
};

// Returns whether automatic
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_isUsingAutoRenderingSurface(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isUsingAutoRenderingSurface());
  *result_c = (int)result_cpp;
};

// Returns the window at the root of the hierarchy starting at this Window
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_getRootWindow(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  const CEGUI::Window * result_cpp;
  result_cpp = (thisclass_cpp->getRootWindow());
  *result_c = getHG3DClass_Window((void *) result_cpp);
;
};

// 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_getRootWindow2(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  CEGUI::Window * result_cpp;
  result_cpp = (thisclass_cpp->getRootWindow());
  *result_c = getHG3DClass_Window((void *) result_cpp);
;
};

// Return whether the Window
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_isNonClientWindow(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isNonClientWindow());
  *result_c = (int)result_cpp;
};

// Renames the window. 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_rename(struct hg3dclass_struct * thisclass_c, char * new_name_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  CEGUI::String new_name_cpp = CEGUI::String((const char*) new_name_c);
  (thisclass_cpp->rename(new_name_cpp));
};

// Initialises the Window
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_initialiseComponents(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  (thisclass_cpp->initialiseComponents());
};

// Set whether or not this WindowWindow
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_setDestroyedByParent(struct hg3dclass_struct * thisclass_c, int setting_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  bool setting_cpp = (bool)setting_c;
  (thisclass_cpp->setDestroyedByParent(setting_cpp));
};

// Set whether this window is always on top, or not. 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_setAlwaysOnTop(struct hg3dclass_struct * thisclass_c, int setting_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  bool setting_cpp = (bool)setting_c;
  (thisclass_cpp->setAlwaysOnTop(setting_cpp));
};

// Set whether this window is enabled or disabled. A disabled window normally can not be interacted with, and may have different rendering. 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_setEnabled(struct hg3dclass_struct * thisclass_c, int setting_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  bool setting_cpp = (bool)setting_c;
  (thisclass_cpp->setEnabled(setting_cpp));
};

// enable the Window
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_enable(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  (thisclass_cpp->enable());
};

// disable the Window
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_disable(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  (thisclass_cpp->disable());
};

// Set whether the Window
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_setVisible(struct hg3dclass_struct * thisclass_c, int setting_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  bool setting_cpp = (bool)setting_c;
  (thisclass_cpp->setVisible(setting_cpp));
};

// show the Window
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_show(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  (thisclass_cpp->show());
};

// hide the Window
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_hide(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  (thisclass_cpp->hide());
};

// Activate the WindowWindow
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_activate(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  (thisclass_cpp->activate());
};

// Deactivate the window. No further inputs will be received by the window until it is re-activated either programmatically or by the user interacting with the gui. 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_deactivate(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  (thisclass_cpp->deactivate());
};

// Set whether this Window
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_setClippedByParent(struct hg3dclass_struct * thisclass_c, int setting_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  bool setting_cpp = (bool)setting_c;
  (thisclass_cpp->setClippedByParent(setting_cpp));
};

// Set the current ID for the Window
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_setID(struct hg3dclass_struct * thisclass_c, unsigned int ID_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  uint ID_cpp = (uint)ID_c;
  (thisclass_cpp->setID(ID_cpp));
};

// Set the current text string for the Window
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_setText(struct hg3dclass_struct * thisclass_c, char * text_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  CEGUI::String text_cpp = CEGUI::String((const char*) text_c);
  (thisclass_cpp->setText(text_cpp));
};

// Append the string textWindow
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_appendText(struct hg3dclass_struct * thisclass_c, char * text_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  CEGUI::String text_cpp = CEGUI::String((const char*) text_c);
  (thisclass_cpp->appendText(text_cpp));
};

// Set the font used by this Window
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_setFont(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * font_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  CEGUI::Font * font_cpp = static_cast<CEGUI::Font*> (getHG3DClassPtr(*font_c, "CEGUI::Font"));
  (thisclass_cpp->setFont(font_cpp));
};

// Set the font used by this Window
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_setFont2(struct hg3dclass_struct * thisclass_c, char * name_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  CEGUI::String name_cpp = CEGUI::String((const char*) name_c);
  (thisclass_cpp->setFont(name_cpp));
};

// Add the named WindowWindowWindownameWindowWindow
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_addChildWindow(struct hg3dclass_struct * thisclass_c, char * name_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  CEGUI::String name_cpp = CEGUI::String((const char*) name_c);
  (thisclass_cpp->addChildWindow(name_cpp));
};

// Add the specified WindowWindowWindowwindowWindowWindow
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_addChildWindow2(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * window_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  CEGUI::Window * window_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*window_c, "CEGUI::Window"));
  (thisclass_cpp->addChildWindow(window_cpp));
};

// Remove the named Window
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_removeChildWindow(struct hg3dclass_struct * thisclass_c, char * name_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  CEGUI::String name_cpp = CEGUI::String((const char*) name_c);
  (thisclass_cpp->removeChildWindow(name_cpp));
};

// Remove the specified Window
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_removeChildWindow2(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * window_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  CEGUI::Window * window_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*window_c, "CEGUI::Window"));
  (thisclass_cpp->removeChildWindow(window_cpp));
};

// Remove the first child WindowWindow
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_removeChildWindow3(struct hg3dclass_struct * thisclass_c, unsigned int ID_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  uint ID_cpp = (uint)ID_c;
  (thisclass_cpp->removeChildWindow(ID_cpp));
};

// Move the Window
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_moveToFront(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  (thisclass_cpp->moveToFront());
};

// Move the Window
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_moveToBack(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  (thisclass_cpp->moveToBack());
};

// Captures input to this window. 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_captureInput(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->captureInput());
  *result_c = (int)result_cpp;
};

// Releases input capture from this WindowWindow
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_releaseInput(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  (thisclass_cpp->releaseInput());
};

// Set whether this window will remember and restore the previous window that had inputs captured. 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_setRestoreCapture(struct hg3dclass_struct * thisclass_c, int setting_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  bool setting_cpp = (bool)setting_c;
  (thisclass_cpp->setRestoreCapture(setting_cpp));
};

// Set the current alpha value for this window. 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_setAlpha(struct hg3dclass_struct * thisclass_c, float alpha_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  float alpha_cpp = (float)alpha_c;
  (thisclass_cpp->setAlpha(alpha_cpp));
};

// Sets whether this Window
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_setInheritsAlpha(struct hg3dclass_struct * thisclass_c, int setting_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  bool setting_cpp = (bool)setting_c;
  (thisclass_cpp->setInheritsAlpha(setting_cpp));
};

// Invalidate this window causing at least this window to be redrawn during the next rendering pass. 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_invalidate(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  (thisclass_cpp->invalidate());
};

// Invalidate this window and - dependant upon recursive
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_invalidate2(struct hg3dclass_struct * thisclass_c, const int recursive_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  const bool recursive_cpp = (bool)recursive_c;
  (thisclass_cpp->invalidate(recursive_cpp));
};

// Set the mouse cursor image to be used when the mouse enters this window. 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_setMouseCursor3(struct hg3dclass_struct * thisclass_c, char * imageset_c, char * image_name_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  CEGUI::String imageset_cpp = CEGUI::String((const char*) imageset_c);
  CEGUI::String image_name_cpp = CEGUI::String((const char*) image_name_c);
  (thisclass_cpp->setMouseCursor(imageset_cpp, image_name_cpp));
};

// Set whether z-order changes are enabled or disabled for this Window
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_setZOrderingEnabled(struct hg3dclass_struct * thisclass_c, int setting_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  bool setting_cpp = (bool)setting_c;
  (thisclass_cpp->setZOrderingEnabled(setting_cpp));
};

// Set whether this window will receive multi-click events or multiple 'down' events instead. 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_setWantsMultiClickEvents(struct hg3dclass_struct * thisclass_c, int setting_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  bool setting_cpp = (bool)setting_c;
  (thisclass_cpp->setWantsMultiClickEvents(setting_cpp));
};

// Set whether mouse button down event autorepeat is enabled for this window. 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_setMouseAutoRepeatEnabled(struct hg3dclass_struct * thisclass_c, int setting_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  bool setting_cpp = (bool)setting_c;
  (thisclass_cpp->setMouseAutoRepeatEnabled(setting_cpp));
};

// Set the current auto-repeat delay setting for this window. 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_setAutoRepeatDelay(struct hg3dclass_struct * thisclass_c, float delay_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  float delay_cpp = (float)delay_c;
  (thisclass_cpp->setAutoRepeatDelay(delay_cpp));
};

// Set the current auto-repeat rate setting for this window. 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_setAutoRepeatRate(struct hg3dclass_struct * thisclass_c, float rate_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  float rate_cpp = (float)rate_c;
  (thisclass_cpp->setAutoRepeatRate(rate_cpp));
};

// Set whether the window wants inputs passed to its attached child windows when the window has inputs captured. 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_setDistributesCapturedInputs(struct hg3dclass_struct * thisclass_c, int setting_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  bool setting_cpp = (bool)setting_c;
  (thisclass_cpp->setDistributesCapturedInputs(setting_cpp));
};

// Internal support method for drag & drop. You do not normally call this directly from client code. See the DragContainer
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_notifyDragDropItemEnters(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * item_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  CEGUI::DragContainer * item_cpp = static_cast<CEGUI::DragContainer*> (getHG3DClassPtr(*item_c, "CEGUI::DragContainer"));
  (thisclass_cpp->notifyDragDropItemEnters(item_cpp));
};

// Internal support method for drag & drop. You do not normally call this directly from client code. See the DragContainer
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_notifyDragDropItemLeaves(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * item_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  CEGUI::DragContainer * item_cpp = static_cast<CEGUI::DragContainer*> (getHG3DClassPtr(*item_c, "CEGUI::DragContainer"));
  (thisclass_cpp->notifyDragDropItemLeaves(item_cpp));
};

// Internal support method for drag & drop. You do not normally call this directly from client code. See the DragContainer
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_notifyDragDropItemDropped(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * item_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  CEGUI::DragContainer * item_cpp = static_cast<CEGUI::DragContainer*> (getHG3DClassPtr(*item_c, "CEGUI::DragContainer"));
  (thisclass_cpp->notifyDragDropItemDropped(item_cpp));
};

// Internal destroy method which actually just adds the window and any parent destructed child windows to the dead pool. 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_destroy(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  (thisclass_cpp->destroy());
};

// Set the custom TooltipWindowWindowTooltip
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_setTooltip(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * tooltip_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  CEGUI::Tooltip * tooltip_cpp = static_cast<CEGUI::Tooltip*> (getHG3DClassPtr(*tooltip_c, "CEGUI::Tooltip"));
  (thisclass_cpp->setTooltip(tooltip_cpp));
};

// Set the custom TooltipWindowWindow
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_setTooltipType(struct hg3dclass_struct * thisclass_c, char * tooltipType_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  CEGUI::String tooltipType_cpp = CEGUI::String((const char*) tooltipType_c);
  (thisclass_cpp->setTooltipType(tooltipType_cpp));
};

// Set the tooltip text for this window. 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_setTooltipText(struct hg3dclass_struct * thisclass_c, char * tip_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  CEGUI::String tip_cpp = CEGUI::String((const char*) tip_c);
  (thisclass_cpp->setTooltipText(tip_cpp));
};

// Set whether this window inherits Tooltip
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_setInheritsTooltipText(struct hg3dclass_struct * thisclass_c, int setting_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  bool setting_cpp = (bool)setting_c;
  (thisclass_cpp->setInheritsTooltipText(setting_cpp));
};

// Set whether this window will rise to the top of the z-order when clicked with the left mouse button. 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_setRiseOnClickEnabled(struct hg3dclass_struct * thisclass_c, int setting_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  bool setting_cpp = (bool)setting_c;
  (thisclass_cpp->setRiseOnClickEnabled(setting_cpp));
};

// Set the vertical alignment. 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_setVerticalAlignment(struct hg3dclass_struct * thisclass_c, const enum EnumVerticalAlignment alignment_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  enum CEGUI::VerticalAlignment alignment_cpp = (enum CEGUI::VerticalAlignment)alignment_c;
  (thisclass_cpp->setVerticalAlignment(alignment_cpp));
};

// Set the horizontal alignment. 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_setHorizontalAlignment(struct hg3dclass_struct * thisclass_c, const enum EnumHorizontalAlignment alignment_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  enum CEGUI::HorizontalAlignment alignment_cpp = (enum CEGUI::HorizontalAlignment)alignment_c;
  (thisclass_cpp->setHorizontalAlignment(alignment_cpp));
};

// Set the LookNFeel that shoule be used for this window. 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_setLookNFeel(struct hg3dclass_struct * thisclass_c, char * look_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  CEGUI::String look_cpp = CEGUI::String((const char*) look_c);
  (thisclass_cpp->setLookNFeel(look_cpp));
};

// Set the modal state for this Window
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_setModalState(struct hg3dclass_struct * thisclass_c, int state_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  bool state_cpp = (bool)state_c;
  (thisclass_cpp->setModalState(state_cpp));
};

// method called to perform extended laying out of attached child windows. 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_performChildWindowLayout(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  (thisclass_cpp->performChildWindowLayout());
};

// Sets the value a named user string, creating it as required. 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_setUserString(struct hg3dclass_struct * thisclass_c, char * name_c, char * value_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  CEGUI::String name_cpp = CEGUI::String((const char*) name_c);
  CEGUI::String value_cpp = CEGUI::String((const char*) value_c);
  (thisclass_cpp->setUserString(name_cpp, value_cpp));
};

// Set the window area. 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_setArea(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * xpos_c, struct hg3dclass_struct * ypos_c, struct hg3dclass_struct * width_c, struct hg3dclass_struct * height_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  const CEGUI::UDim * xpos_cpp = static_cast<CEGUI::UDim*> (getHG3DClassPtr(*xpos_c, "CEGUI::UDim"));
  const CEGUI::UDim * ypos_cpp = static_cast<CEGUI::UDim*> (getHG3DClassPtr(*ypos_c, "CEGUI::UDim"));
  const CEGUI::UDim * width_cpp = static_cast<CEGUI::UDim*> (getHG3DClassPtr(*width_c, "CEGUI::UDim"));
  const CEGUI::UDim * height_cpp = static_cast<CEGUI::UDim*> (getHG3DClassPtr(*height_c, "CEGUI::UDim"));
  (thisclass_cpp->setArea(*xpos_cpp, *ypos_cpp, *width_cpp, *height_cpp));
};

// Set the window area. 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_setArea2(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * pos_c, struct hg3dclass_struct * size_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  const CEGUI::UVector2 * pos_cpp = static_cast<CEGUI::UVector2*> (getHG3DClassPtr(*pos_c, "CEGUI::UVector2"));
  const CEGUI::UVector2 * size_cpp = static_cast<CEGUI::UVector2*> (getHG3DClassPtr(*size_c, "CEGUI::UVector2"));
  (thisclass_cpp->setArea(*pos_cpp, *size_cpp));
};

// Set the window's position. 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_setPosition(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * pos_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  const CEGUI::UVector2 * pos_cpp = static_cast<CEGUI::UVector2*> (getHG3DClassPtr(*pos_c, "CEGUI::UVector2"));
  (thisclass_cpp->setPosition(*pos_cpp));
};

// Set the window's X position. 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_setXPosition(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * x_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  const CEGUI::UDim * x_cpp = static_cast<CEGUI::UDim*> (getHG3DClassPtr(*x_c, "CEGUI::UDim"));
  (thisclass_cpp->setXPosition(*x_cpp));
};

// Set the window's Y position. 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_setYPosition(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * y_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  const CEGUI::UDim * y_cpp = static_cast<CEGUI::UDim*> (getHG3DClassPtr(*y_c, "CEGUI::UDim"));
  (thisclass_cpp->setYPosition(*y_cpp));
};

// Set the window's size. 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_setSize(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * size_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  const CEGUI::UVector2 * size_cpp = static_cast<CEGUI::UVector2*> (getHG3DClassPtr(*size_c, "CEGUI::UVector2"));
  (thisclass_cpp->setSize(*size_cpp));
};

// Set the window's width. 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_setWidth(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * width_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  const CEGUI::UDim * width_cpp = static_cast<CEGUI::UDim*> (getHG3DClassPtr(*width_c, "CEGUI::UDim"));
  (thisclass_cpp->setWidth(*width_cpp));
};

// Set the window's height. 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_setHeight(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * height_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  const CEGUI::UDim * height_cpp = static_cast<CEGUI::UDim*> (getHG3DClassPtr(*height_c, "CEGUI::UDim"));
  (thisclass_cpp->setHeight(*height_cpp));
};

// Set the window's maximum size. 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_setMaxSize(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * size_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  const CEGUI::UVector2 * size_cpp = static_cast<CEGUI::UVector2*> (getHG3DClassPtr(*size_c, "CEGUI::UVector2"));
  (thisclass_cpp->setMaxSize(*size_cpp));
};

// Set the window's minimum size. 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_setMinSize(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * size_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  const CEGUI::UVector2 * size_cpp = static_cast<CEGUI::UVector2*> (getHG3DClassPtr(*size_c, "CEGUI::UVector2"));
  (thisclass_cpp->setMinSize(*size_cpp));
};

// Get the window's position. 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_getPosition(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  const CEGUI::UVector2 * result_cpp;
  result_cpp = &(thisclass_cpp->getPosition());
  *result_c = getHG3DClass_UVector2((void *) result_cpp);
;
};

// Get the window's X position. 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_getXPosition(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  const CEGUI::UDim * result_cpp;
  result_cpp = &(thisclass_cpp->getXPosition());
  *result_c = getHG3DClass_UDim((void *) result_cpp);
;
};

// Get the window's Y position. 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_getYPosition(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  const CEGUI::UDim * result_cpp;
  result_cpp = &(thisclass_cpp->getYPosition());
  *result_c = getHG3DClass_UDim((void *) result_cpp);
;
};

// Get the window's maximum size. 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_getMaxSize(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  const CEGUI::UVector2 * result_cpp;
  result_cpp = &(thisclass_cpp->getMaxSize());
  *result_c = getHG3DClass_UVector2((void *) result_cpp);
;
};

// Get the window's minimum size. 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_getMinSize(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  const CEGUI::UVector2 * result_cpp;
  result_cpp = &(thisclass_cpp->getMinSize());
  *result_c = getHG3DClass_UVector2((void *) result_cpp);
;
};

// Causes the Window
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_render(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  (thisclass_cpp->render());
};

// Cause window to update itself and any attached children. Client code does not need to call this method; to ensure full, and proper updates, call the injectTimePulse methodname method provided by the System
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_update(struct hg3dclass_struct * thisclass_c, float elapsed_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  float elapsed_cpp = (float)elapsed_c;
  (thisclass_cpp->update(elapsed_cpp));
};

// Sets the internal 'initialising' flag to true. This can be use to optimize initialisation of some widgets, and is called automatically by the layout XML handler when it has created a window. That is just after the window has been created, but before any children or properties are read. 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_beginInitialisation(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  (thisclass_cpp->beginInitialisation());
};

// Sets the internal 'initialising' flag to false. This is called automatically by the layout XML handler when it is done creating a window. That is after all properties and children have been loaded and just before the next sibling gets created. 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_endInitialisation(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  (thisclass_cpp->endInitialisation());
};

// Sets whether this window should ignore mouse events and pass them through to any windows behind it. In effect making the window transparent to the mouse. 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_setMousePassThroughEnabled(struct hg3dclass_struct * thisclass_c, int setting_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  bool setting_cpp = (bool)setting_c;
  (thisclass_cpp->setMousePassThroughEnabled(setting_cpp));
};

// Assign the WindowRenderer to specify the Look'N'Feel specification to be used. 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_setWindowRenderer(struct hg3dclass_struct * thisclass_c, char * name_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  CEGUI::String name_cpp = CEGUI::String((const char*) name_c);
  (thisclass_cpp->setWindowRenderer(name_cpp));
};

// Get the factory name of the currently assigned WindowRenderer. (Look'N'Feel specification). 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_getWindowRendererName(struct hg3dclass_struct * thisclass_c, char * result_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  CEGUI::String result_cpp;
  result_cpp = (thisclass_cpp->getWindowRendererName());
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

// Sets whether this window is allowed to write XML. 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_setWritingXMLAllowed(struct hg3dclass_struct * thisclass_c, int allow_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  bool allow_cpp = (bool)allow_c;
  (thisclass_cpp->setWritingXMLAllowed(allow_cpp));
};

// Inform the window, and optionally all children, that screen area rectangles have changed. 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_notifyScreenAreaChanged(struct hg3dclass_struct * thisclass_c, int recursive_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  bool recursive_cpp = (bool)recursive_c;
  (thisclass_cpp->notifyScreenAreaChanged(recursive_cpp));
};

// Changes the widget's falagard type, thus changing its look'n'feel and optionally its renderer in the process. 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_setFalagardType(struct hg3dclass_struct * thisclass_c, char * type_c, char * rendererType_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  CEGUI::String type_cpp = CEGUI::String((const char*) type_c);
  CEGUI::String rendererType_cpp = CEGUI::String((const char*) rendererType_c);
  (thisclass_cpp->setFalagardType(type_cpp, rendererType_cpp));
};

// Specifies whether this Window
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_setDragDropTarget(struct hg3dclass_struct * thisclass_c, int setting_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  bool setting_cpp = (bool)setting_c;
  (thisclass_cpp->setDragDropTarget(setting_cpp));
};

// Invalidate the chain of rendering surfaces from this window backwards to ensure they get properly redrawn - but doing the minimum amount of work possibe - next render. 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_invalidateRenderingSurface(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  (thisclass_cpp->invalidateRenderingSurface());
};

// Sets whether automatic
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_setUsingAutoRenderingSurface(struct hg3dclass_struct * thisclass_c, int setting_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  bool setting_cpp = (bool)setting_c;
  (thisclass_cpp->setUsingAutoRenderingSurface(setting_cpp));
};

// Set whether the Window
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_setNonClientWindow(struct hg3dclass_struct * thisclass_c, const int setting_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  const bool setting_cpp = (bool)setting_c;
  (thisclass_cpp->setNonClientWindow(setting_cpp));
};

// return whether text parsing is enabled for this window. 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_isTextParsingEnabled(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isTextParsingEnabled());
  *result_c = (int)result_cpp;
};

// set whether text parsing is enabled for this window. 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_setTextParsingEnabled(struct hg3dclass_struct * thisclass_c, const int setting_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  const bool setting_cpp = (bool)setting_c;
  (thisclass_cpp->setTextParsingEnabled(setting_cpp));
};

// Add the named property to the XML ban list for this window. 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_banPropertyFromXML(struct hg3dclass_struct * thisclass_c, char * property_name_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  CEGUI::String property_name_cpp = CEGUI::String((const char*) property_name_c);
  (thisclass_cpp->banPropertyFromXML(property_name_cpp));
};

// Remove the named property from the XML ban list for this window. 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_unbanPropertyFromXML(struct hg3dclass_struct * thisclass_c, char * property_name_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  CEGUI::String property_name_cpp = CEGUI::String((const char*) property_name_c);
  (thisclass_cpp->unbanPropertyFromXML(property_name_cpp));
};

// Return whether the named property is banned from XML. 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_isPropertyBannedFromXML(struct hg3dclass_struct * thisclass_c, char * property_name_c, int * result_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  CEGUI::String property_name_cpp = CEGUI::String((const char*) property_name_c);
  bool result_cpp;
  result_cpp = (thisclass_cpp->isPropertyBannedFromXML(property_name_cpp));
  *result_c = (int)result_cpp;
};

// Set the window update mode. This mode controls the behaviour of the Window::update
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_setUpdateMode(struct hg3dclass_struct * thisclass_c, const enum EnumWindowUpdateMode mode_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  enum CEGUI::WindowUpdateMode mode_cpp = (enum CEGUI::WindowUpdateMode)mode_c;
  (thisclass_cpp->setUpdateMode(mode_cpp));
};

// Return the current window update mode that is set for this WindowWindow::update
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_getUpdateMode(struct hg3dclass_struct * thisclass_c, enum EnumWindowUpdateMode * result_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  enum CEGUI::WindowUpdateMode result_cpp;
  result_cpp = (thisclass_cpp->getUpdateMode());
  *result_c = (enum EnumWindowUpdateMode) result_cpp;
};

// Set whether mouse input that is not directly handled by this WindowWindow
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_setMouseInputPropagationEnabled(struct hg3dclass_struct * thisclass_c, const int enabled_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  const bool enabled_cpp = (bool)enabled_c;
  (thisclass_cpp->setMouseInputPropagationEnabled(enabled_cpp));
};

// Return whether mouse input that is not directly handled by this WindowWindow
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_isMouseInputPropagationEnabled(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isMouseInputPropagationEnabled());
  *result_c = (int)result_cpp;
};

// Clones this Window
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_clone(struct hg3dclass_struct * thisclass_c, char * newName_c, const int deepCopy_c, struct hg3dclass_struct * result_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  CEGUI::String newName_cpp = CEGUI::String((const char*) newName_c);
  const bool deepCopy_cpp = (bool)deepCopy_c;
  CEGUI::Window * result_cpp;
  result_cpp = (thisclass_cpp->clone(newName_cpp, deepCopy_cpp));
  *result_c = getHG3DClass_Window((void *) result_cpp);
;
};

// copies this widget's properties to given target widget 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_clonePropertiesTo(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * target_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  CEGUI::Window * target_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*target_c, "CEGUI::Window"));
  (thisclass_cpp->clonePropertiesTo(*target_cpp));
};

// copies this widget's child widgets to given target widget 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_cloneChildWidgetsTo(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * target_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  CEGUI::Window * target_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*target_c, "CEGUI::Window"));
  (thisclass_cpp->cloneChildWidgetsTo(*target_cpp));
};

// Return the (visual) z index of the window on it's parent. 
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_getZIndex(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  size_t result_cpp;
  result_cpp = (thisclass_cpp->getZIndex());
  *result_c = (int)result_cpp;
};

// Return whether /a this Window
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_isInFront(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * wnd_c, int * result_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  const CEGUI::Window * wnd_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*wnd_c, "CEGUI::Window"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isInFront(*wnd_cpp));
  *result_c = (int)result_cpp;
};

// Return whether /a this Window
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_isBehind(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * wnd_c, int * result_c)
{
  CEGUI::Window * thisclass_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Window"));
  const CEGUI::Window * wnd_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*wnd_c, "CEGUI::Window"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isBehind(*wnd_cpp));
  *result_c = (int)result_cpp;
};

// return the Window
extern "C" CEGUI_LIB_EXPORT void cegui_wnd_getCaptureWindow(struct hg3dclass_struct * result_c)
{
  CEGUI::Window * result_cpp;
  result_cpp = (CEGUI::Window::getCaptureWindow());
  *result_c = getHG3DClass_Window((void *) result_cpp);
;
};

