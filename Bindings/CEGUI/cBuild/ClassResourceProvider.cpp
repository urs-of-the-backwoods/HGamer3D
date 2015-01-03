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

// ClassResourceProvider.cpp

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



// Destructor for the ResourceProvider
extern "C" CEGUI_LIB_EXPORT void cegui_rsrcpr_destruct(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::ResourceProvider * thisclass_cpp = static_cast<CEGUI::ResourceProvider*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ResourceProvider"));
  (delete thisclass_cpp);
};

// Return the current default resource group identifier. 
extern "C" CEGUI_LIB_EXPORT void cegui_rsrcpr_getDefaultResourceGroup(struct hg3dclass_struct * thisclass_c, char * result_c)
{
  CEGUI::ResourceProvider * thisclass_cpp = static_cast<CEGUI::ResourceProvider*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ResourceProvider"));
  CEGUI::String result_cpp;
  result_cpp = (thisclass_cpp->getDefaultResourceGroup());
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

// Set the default resource group identifier. 
extern "C" CEGUI_LIB_EXPORT void cegui_rsrcpr_setDefaultResourceGroup(struct hg3dclass_struct * thisclass_c, char * resourceGroup_c)
{
  CEGUI::ResourceProvider * thisclass_cpp = static_cast<CEGUI::ResourceProvider*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ResourceProvider"));
  CEGUI::String resourceGroup_cpp = CEGUI::String((const char*) resourceGroup_c);
  (thisclass_cpp->setDefaultResourceGroup(resourceGroup_cpp));
};

