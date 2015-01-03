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

// ClassWindowManager.cpp

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



// Constructs a new WindowManager
extern "C" CEGUI_LIB_EXPORT void cegui_wmgr_construct(struct hg3dclass_struct * result_c)
{
  CEGUI::WindowManager * result_cpp;
  result_cpp = (new CEGUI::WindowManager());
  *result_c = getHG3DClass_WindowManager((void *) result_cpp);
;
};

// Destructor for WindowManager
extern "C" CEGUI_LIB_EXPORT void cegui_wmgr_destruct(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::WindowManager * thisclass_cpp = static_cast<CEGUI::WindowManager*> (getHG3DClassPtr(*thisclass_c, "CEGUI::WindowManager"));
  (delete thisclass_cpp);
};

// Creates a new Window
extern "C" CEGUI_LIB_EXPORT void cegui_wmgr_createWindow(struct hg3dclass_struct * thisclass_c, char * type_c, char * name_c, struct hg3dclass_struct * result_c)
{
  CEGUI::WindowManager * thisclass_cpp = static_cast<CEGUI::WindowManager*> (getHG3DClassPtr(*thisclass_c, "CEGUI::WindowManager"));
  CEGUI::String type_cpp = CEGUI::String((const char*) type_c);
  CEGUI::String name_cpp = CEGUI::String((const char*) name_c);
  CEGUI::Window * result_cpp;
  result_cpp = (thisclass_cpp->createWindow(type_cpp, name_cpp));
  *result_c = getHG3DClass_Window((void *) result_cpp);
;
};

// Destroy the specified Window
extern "C" CEGUI_LIB_EXPORT void cegui_wmgr_destroyWindow(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * window_c)
{
  CEGUI::WindowManager * thisclass_cpp = static_cast<CEGUI::WindowManager*> (getHG3DClassPtr(*thisclass_c, "CEGUI::WindowManager"));
  CEGUI::Window * window_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*window_c, "CEGUI::Window"));
  (thisclass_cpp->destroyWindow(window_cpp));
};

// Destroy the specified Window
extern "C" CEGUI_LIB_EXPORT void cegui_wmgr_destroyWindow2(struct hg3dclass_struct * thisclass_c, char * window_c)
{
  CEGUI::WindowManager * thisclass_cpp = static_cast<CEGUI::WindowManager*> (getHG3DClassPtr(*thisclass_c, "CEGUI::WindowManager"));
  CEGUI::String window_cpp = CEGUI::String((const char*) window_c);
  (thisclass_cpp->destroyWindow(window_cpp));
};

// Return a pointer to the specified Window
extern "C" CEGUI_LIB_EXPORT void cegui_wmgr_getWindow(struct hg3dclass_struct * thisclass_c, char * name_c, struct hg3dclass_struct * result_c)
{
  CEGUI::WindowManager * thisclass_cpp = static_cast<CEGUI::WindowManager*> (getHG3DClassPtr(*thisclass_c, "CEGUI::WindowManager"));
  CEGUI::String name_cpp = CEGUI::String((const char*) name_c);
  CEGUI::Window * result_cpp;
  result_cpp = (thisclass_cpp->getWindow(name_cpp));
  *result_c = getHG3DClass_Window((void *) result_cpp);
;
};

// Examines the list of Window
extern "C" CEGUI_LIB_EXPORT void cegui_wmgr_isWindowPresent(struct hg3dclass_struct * thisclass_c, char * name_c, int * result_c)
{
  CEGUI::WindowManager * thisclass_cpp = static_cast<CEGUI::WindowManager*> (getHG3DClassPtr(*thisclass_c, "CEGUI::WindowManager"));
  CEGUI::String name_cpp = CEGUI::String((const char*) name_c);
  bool result_cpp;
  result_cpp = (thisclass_cpp->isWindowPresent(name_cpp));
  *result_c = (int)result_cpp;
};

// Destroys all Window
extern "C" CEGUI_LIB_EXPORT void cegui_wmgr_destroyAllWindows(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::WindowManager * thisclass_cpp = static_cast<CEGUI::WindowManager*> (getHG3DClassPtr(*thisclass_c, "CEGUI::WindowManager"));
  (thisclass_cpp->destroyAllWindows());
};

// Return whether the window dead pool is empty. 
extern "C" CEGUI_LIB_EXPORT void cegui_wmgr_isDeadPoolEmpty(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::WindowManager * thisclass_cpp = static_cast<CEGUI::WindowManager*> (getHG3DClassPtr(*thisclass_c, "CEGUI::WindowManager"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isDeadPoolEmpty());
  *result_c = (int)result_cpp;
};

// Permanently destroys any windows placed in the dead pool. 
extern "C" CEGUI_LIB_EXPORT void cegui_wmgr_cleanDeadPool(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::WindowManager * thisclass_cpp = static_cast<CEGUI::WindowManager*> (getHG3DClassPtr(*thisclass_c, "CEGUI::WindowManager"));
  (thisclass_cpp->cleanDeadPool());
};

// Save a full XML window layout, starting at the given Window
extern "C" CEGUI_LIB_EXPORT void cegui_wmgr_saveWindowLayout(struct hg3dclass_struct * thisclass_c, char * window_c, char * filename_c, const int writeParent_c)
{
  CEGUI::WindowManager * thisclass_cpp = static_cast<CEGUI::WindowManager*> (getHG3DClassPtr(*thisclass_c, "CEGUI::WindowManager"));
  CEGUI::String window_cpp = CEGUI::String((const char*) window_c);
  CEGUI::String filename_cpp = CEGUI::String((const char*) filename_c);
  const bool writeParent_cpp = (bool)writeParent_c;
  (thisclass_cpp->saveWindowLayout(window_cpp, filename_cpp, writeParent_cpp));
};

// Save a full XML window layout, starting at the given Window
extern "C" CEGUI_LIB_EXPORT void cegui_wmgr_saveWindowLayout2(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * window_c, char * filename_c, const int writeParent_c)
{
  CEGUI::WindowManager * thisclass_cpp = static_cast<CEGUI::WindowManager*> (getHG3DClassPtr(*thisclass_c, "CEGUI::WindowManager"));
  const CEGUI::Window * window_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*window_c, "CEGUI::Window"));
  CEGUI::String filename_cpp = CEGUI::String((const char*) filename_c);
  const bool writeParent_cpp = (bool)writeParent_c;
  (thisclass_cpp->saveWindowLayout(*window_cpp, filename_cpp, writeParent_cpp));
};

// Rename a window. 
extern "C" CEGUI_LIB_EXPORT void cegui_wmgr_renameWindow(struct hg3dclass_struct * thisclass_c, char * window_c, char * new_name_c)
{
  CEGUI::WindowManager * thisclass_cpp = static_cast<CEGUI::WindowManager*> (getHG3DClassPtr(*thisclass_c, "CEGUI::WindowManager"));
  CEGUI::String window_cpp = CEGUI::String((const char*) window_c);
  CEGUI::String new_name_cpp = CEGUI::String((const char*) new_name_c);
  (thisclass_cpp->renameWindow(window_cpp, new_name_cpp));
};

// Rename a window. 
extern "C" CEGUI_LIB_EXPORT void cegui_wmgr_renameWindow2(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * window_c, char * new_name_c)
{
  CEGUI::WindowManager * thisclass_cpp = static_cast<CEGUI::WindowManager*> (getHG3DClassPtr(*thisclass_c, "CEGUI::WindowManager"));
  CEGUI::Window * window_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*window_c, "CEGUI::Window"));
  CEGUI::String new_name_cpp = CEGUI::String((const char*) new_name_c);
  (thisclass_cpp->renameWindow(window_cpp, new_name_cpp));
};

// Put WindowManager
extern "C" CEGUI_LIB_EXPORT void cegui_wmgr_lock(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::WindowManager * thisclass_cpp = static_cast<CEGUI::WindowManager*> (getHG3DClassPtr(*thisclass_c, "CEGUI::WindowManager"));
  (thisclass_cpp->lock());
};

// Put WindowManager
extern "C" CEGUI_LIB_EXPORT void cegui_wmgr_unlock(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::WindowManager * thisclass_cpp = static_cast<CEGUI::WindowManager*> (getHG3DClassPtr(*thisclass_c, "CEGUI::WindowManager"));
  (thisclass_cpp->unlock());
};

// Returns whether WindowManager
extern "C" CEGUI_LIB_EXPORT void cegui_wmgr_isLocked(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::WindowManager * thisclass_cpp = static_cast<CEGUI::WindowManager*> (getHG3DClassPtr(*thisclass_c, "CEGUI::WindowManager"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isLocked());
  *result_c = (int)result_cpp;
};

// Returns the default resource group currently set for layouts. 
extern "C" CEGUI_LIB_EXPORT void cegui_wmgr_getDefaultResourceGroup(char * result_c)
{
  CEGUI::String result_cpp;
  result_cpp = (CEGUI::WindowManager::getDefaultResourceGroup());
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

// Sets the default resource group to be used when loading layouts. 
extern "C" CEGUI_LIB_EXPORT void cegui_wmgr_setDefaultResourceGroup(char * resourceGroup_c)
{
  CEGUI::String resourceGroup_cpp = CEGUI::String((const char*) resourceGroup_c);
  (CEGUI::WindowManager::setDefaultResourceGroup(resourceGroup_cpp));
};

