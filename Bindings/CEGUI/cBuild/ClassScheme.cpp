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

// ClassScheme.cpp

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



// Loads all resources for this scheme. 
extern "C" CEGUI_LIB_EXPORT void cegui_schm_loadResources(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::Scheme * thisclass_cpp = static_cast<CEGUI::Scheme*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Scheme"));
  (thisclass_cpp->loadResources());
};

// Unloads all resources for this scheme. This should be used very carefully. 
extern "C" CEGUI_LIB_EXPORT void cegui_schm_unloadResources(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::Scheme * thisclass_cpp = static_cast<CEGUI::Scheme*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Scheme"));
  (thisclass_cpp->unloadResources());
};

// Return whether the resources for this Scheme
extern "C" CEGUI_LIB_EXPORT void cegui_schm_resourcesLoaded(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::Scheme * thisclass_cpp = static_cast<CEGUI::Scheme*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Scheme"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->resourcesLoaded());
  *result_c = (int)result_cpp;
};

// Return the name of this Scheme
extern "C" CEGUI_LIB_EXPORT void cegui_schm_getName(struct hg3dclass_struct * thisclass_c, char * result_c)
{
  CEGUI::Scheme * thisclass_cpp = static_cast<CEGUI::Scheme*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Scheme"));
  CEGUI::String result_cpp;
  result_cpp = (thisclass_cpp->getName());
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

// Destroys a Scheme
extern "C" CEGUI_LIB_EXPORT void cegui_schm_destruct(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::Scheme * thisclass_cpp = static_cast<CEGUI::Scheme*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Scheme"));
  (delete thisclass_cpp);
};

// Returns the default resource group currently set for Schemes. 
extern "C" CEGUI_LIB_EXPORT void cegui_schm_getDefaultResourceGroup(char * result_c)
{
  CEGUI::String result_cpp;
  result_cpp = (CEGUI::Scheme::getDefaultResourceGroup());
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

// Sets the default resource group to be used when loading scheme xml data. 
extern "C" CEGUI_LIB_EXPORT void cegui_schm_setDefaultResourceGroup(char * resourceGroup_c)
{
  CEGUI::String resourceGroup_cpp = CEGUI::String((const char*) resourceGroup_c);
  (CEGUI::Scheme::setDefaultResourceGroup(resourceGroup_cpp));
};

