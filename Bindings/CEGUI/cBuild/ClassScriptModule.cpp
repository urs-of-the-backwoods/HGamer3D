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

// ClassScriptModule.cpp

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
#include "HG3DEventStaticFunctions.h"
#include "HG3DListboxStaticFunctions.h"
#include "HG3DEventController.h"
#include "HG3DCommandHandler.h"
#include "HG3DEventModule.h"
#include "HG3DWindowStaticFunctions.h"

using namespace CEGUI;



// Destructor for ScriptModule
extern "C" CEGUI_LIB_EXPORT void cegui_scrmd_destruct(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::ScriptModule * thisclass_cpp = static_cast<CEGUI::ScriptModule*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ScriptModule"));
  (delete thisclass_cpp);
};

// Execute a script file. 
extern "C" CEGUI_LIB_EXPORT void cegui_scrmd_executeScriptFile(struct hg3dclass_struct * thisclass_c, char * filename_c, char * resourceGroup_c)
{
  CEGUI::ScriptModule * thisclass_cpp = static_cast<CEGUI::ScriptModule*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ScriptModule"));
  CEGUI::String filename_cpp = CEGUI::String((const char*) filename_c);
  CEGUI::String resourceGroup_cpp = CEGUI::String((const char*) resourceGroup_c);
  (thisclass_cpp->executeScriptFile(filename_cpp, resourceGroup_cpp));
};

// Execute a scripted global function. The function should not take any parameters and should return an integer. 
extern "C" CEGUI_LIB_EXPORT void cegui_scrmd_executeScriptGlobal(struct hg3dclass_struct * thisclass_c, char * function_name_c, int * result_c)
{
  CEGUI::ScriptModule * thisclass_cpp = static_cast<CEGUI::ScriptModule*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ScriptModule"));
  CEGUI::String function_name_cpp = CEGUI::String((const char*) function_name_c);
  int result_cpp;
  result_cpp = (thisclass_cpp->executeScriptGlobal(function_name_cpp));
  *result_c = (int)result_cpp;
};

// Execute a scripted global 'event handler' function. The function should take some kind of EventArgsEventArgs
extern "C" CEGUI_LIB_EXPORT void cegui_scrmd_executeScriptedEventHandler(struct hg3dclass_struct * thisclass_c, char * handler_name_c, struct hg3dclass_struct * e_c, int * result_c)
{
  CEGUI::ScriptModule * thisclass_cpp = static_cast<CEGUI::ScriptModule*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ScriptModule"));
  CEGUI::String handler_name_cpp = CEGUI::String((const char*) handler_name_c);
  const CEGUI::EventArgs * e_cpp = static_cast<CEGUI::EventArgs*> (getHG3DClassPtr(*e_c, "CEGUI::EventArgs"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->executeScriptedEventHandler(handler_name_cpp, *e_cpp));
  *result_c = (int)result_cpp;
};

// Execute script code contained in the given CEGUI::String object. 
extern "C" CEGUI_LIB_EXPORT void cegui_scrmd_executeString(struct hg3dclass_struct * thisclass_c, char * str_c)
{
  CEGUI::ScriptModule * thisclass_cpp = static_cast<CEGUI::ScriptModule*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ScriptModule"));
  CEGUI::String str_cpp = CEGUI::String((const char*) str_c);
  (thisclass_cpp->executeString(str_cpp));
};

// Method called during system initialisation, prior to running any scripts via the ScriptModuleScriptModule
extern "C" CEGUI_LIB_EXPORT void cegui_scrmd_createBindings(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::ScriptModule * thisclass_cpp = static_cast<CEGUI::ScriptModule*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ScriptModule"));
  (thisclass_cpp->createBindings());
};

// Method called during system destruction, after all scripts have been run via the ScriptModuleScriptModule
extern "C" CEGUI_LIB_EXPORT void cegui_scrmd_destroyBindings(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::ScriptModule * thisclass_cpp = static_cast<CEGUI::ScriptModule*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ScriptModule"));
  (thisclass_cpp->destroyBindings());
};

// Return identification string for the ScriptModuleScriptModule
extern "C" CEGUI_LIB_EXPORT void cegui_scrmd_getIdentifierString(struct hg3dclass_struct * thisclass_c, char * result_c)
{
  CEGUI::ScriptModule * thisclass_cpp = static_cast<CEGUI::ScriptModule*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ScriptModule"));
  CEGUI::String result_cpp;
  result_cpp = (thisclass_cpp->getIdentifierString());
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

// Sets the default resource group to be used when loading script files. 
extern "C" CEGUI_LIB_EXPORT void cegui_scrmd_setDefaultResourceGroup(char * resourceGroup_c)
{
  CEGUI::String resourceGroup_cpp = CEGUI::String((const char*) resourceGroup_c);
  (CEGUI::ScriptModule::setDefaultResourceGroup(resourceGroup_cpp));
};

// Returns the default resource group used when loading script files. 
extern "C" CEGUI_LIB_EXPORT void cegui_scrmd_getDefaultResourceGroup(char * result_c)
{
  CEGUI::String result_cpp;
  result_cpp = (CEGUI::ScriptModule::getDefaultResourceGroup());
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

