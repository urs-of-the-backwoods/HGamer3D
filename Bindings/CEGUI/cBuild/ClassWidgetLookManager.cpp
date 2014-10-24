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

// ClassWidgetLookManager.cpp

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



// Constructor. 
extern "C" CEGUI_LIB_EXPORT void cegui_wdgtlmgr_construct(struct hg3dclass_struct * result_c)
{
  CEGUI::WidgetLookManager * result_cpp;
  result_cpp = (new CEGUI::WidgetLookManager());
  *result_c = getHG3DClass_WidgetLookManager((void *) result_cpp);
;
};

// Destructor. 
extern "C" CEGUI_LIB_EXPORT void cegui_wdgtlmgr_destruct(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::WidgetLookManager * thisclass_cpp = static_cast<CEGUI::WidgetLookManager*> (getHG3DClassPtr(*thisclass_c, "CEGUI::WidgetLookManager"));
  (delete thisclass_cpp);
};

// Parses a file containing window look & feel specifications (in the form of XML). 
extern "C" CEGUI_LIB_EXPORT void cegui_wdgtlmgr_parseLookNFeelSpecification(struct hg3dclass_struct * thisclass_c, char * filename_c, char * resourceGroup_c)
{
  CEGUI::WidgetLookManager * thisclass_cpp = static_cast<CEGUI::WidgetLookManager*> (getHG3DClassPtr(*thisclass_c, "CEGUI::WidgetLookManager"));
  CEGUI::String filename_cpp = CEGUI::String((const char*) filename_c);
  CEGUI::String resourceGroup_cpp = CEGUI::String((const char*) resourceGroup_c);
  (thisclass_cpp->parseLookNFeelSpecification(filename_cpp, resourceGroup_cpp));
};

// Return whether a WidgetLookFeel has been created with the specified name. 
extern "C" CEGUI_LIB_EXPORT void cegui_wdgtlmgr_isWidgetLookAvailable(struct hg3dclass_struct * thisclass_c, char * widget_c, int * result_c)
{
  CEGUI::WidgetLookManager * thisclass_cpp = static_cast<CEGUI::WidgetLookManager*> (getHG3DClassPtr(*thisclass_c, "CEGUI::WidgetLookManager"));
  CEGUI::String widget_cpp = CEGUI::String((const char*) widget_c);
  bool result_cpp;
  result_cpp = (thisclass_cpp->isWidgetLookAvailable(widget_cpp));
  *result_c = (int)result_cpp;
};

// Erase the WidgetLookFeel that has the specified name. 
extern "C" CEGUI_LIB_EXPORT void cegui_wdgtlmgr_eraseWidgetLook(struct hg3dclass_struct * thisclass_c, char * widget_c)
{
  CEGUI::WidgetLookManager * thisclass_cpp = static_cast<CEGUI::WidgetLookManager*> (getHG3DClassPtr(*thisclass_c, "CEGUI::WidgetLookManager"));
  CEGUI::String widget_cpp = CEGUI::String((const char*) widget_c);
  (thisclass_cpp->eraseWidgetLook(widget_cpp));
};

// Return singleton WidgetLookManager
extern "C" CEGUI_LIB_EXPORT void cegui_wdgtlmgr_getSingleton(struct hg3dclass_struct * result_c)
{
  CEGUI::WidgetLookManager * result_cpp;
  result_cpp = &(CEGUI::WidgetLookManager::getSingleton());
  *result_c = getHG3DClass_WidgetLookManager((void *) result_cpp);
;
};

// Return pointer to singleton WidgetLookManager
extern "C" CEGUI_LIB_EXPORT void cegui_wdgtlmgr_getSingletonPtr(struct hg3dclass_struct * result_c)
{
  CEGUI::WidgetLookManager * result_cpp;
  result_cpp = (CEGUI::WidgetLookManager::getSingletonPtr());
  *result_c = getHG3DClass_WidgetLookManager((void *) result_cpp);
;
};

// Returns the default resource group currently set for LookNFeels. 
extern "C" CEGUI_LIB_EXPORT void cegui_wdgtlmgr_getDefaultResourceGroup(char * result_c)
{
  CEGUI::String result_cpp;
  result_cpp = (CEGUI::WidgetLookManager::getDefaultResourceGroup());
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

// Sets the default resource group to be used when loading LookNFeel data. 
extern "C" CEGUI_LIB_EXPORT void cegui_wdgtlmgr_setDefaultResourceGroup(char * resourceGroup_c)
{
  CEGUI::String resourceGroup_cpp = CEGUI::String((const char*) resourceGroup_c);
  (CEGUI::WidgetLookManager::setDefaultResourceGroup(resourceGroup_cpp));
};

