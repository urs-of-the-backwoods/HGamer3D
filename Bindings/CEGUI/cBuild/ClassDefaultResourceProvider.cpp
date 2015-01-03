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

// ClassDefaultResourceProvider.cpp

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



// 
extern "C" CEGUI_LIB_EXPORT void cegui_drpr_construct(struct hg3dclass_struct * result_c)
{
  CEGUI::DefaultResourceProvider * result_cpp;
  result_cpp = (new CEGUI::DefaultResourceProvider());
  *result_c = getHG3DClass_DefaultResourceProvider((void *) result_cpp);
;
};

// 
extern "C" CEGUI_LIB_EXPORT void cegui_drpr_destruct(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::DefaultResourceProvider * thisclass_cpp = static_cast<CEGUI::DefaultResourceProvider*> (getHG3DClassPtr(*thisclass_c, "CEGUI::DefaultResourceProvider"));
  (delete thisclass_cpp);
};

// Set the directory associated with a given resource group identifier. 
extern "C" CEGUI_LIB_EXPORT void cegui_drpr_setResourceGroupDirectory(struct hg3dclass_struct * thisclass_c, char * resourceGroup_c, char * directory_c)
{
  CEGUI::DefaultResourceProvider * thisclass_cpp = static_cast<CEGUI::DefaultResourceProvider*> (getHG3DClassPtr(*thisclass_c, "CEGUI::DefaultResourceProvider"));
  CEGUI::String resourceGroup_cpp = CEGUI::String((const char*) resourceGroup_c);
  CEGUI::String directory_cpp = CEGUI::String((const char*) directory_c);
  (thisclass_cpp->setResourceGroupDirectory(resourceGroup_cpp, directory_cpp));
};

// Return the directory associated with the specified resource group identifier. 
extern "C" CEGUI_LIB_EXPORT void cegui_drpr_getResourceGroupDirectory(struct hg3dclass_struct * thisclass_c, char * resourceGroup_c, char * result_c)
{
  CEGUI::DefaultResourceProvider * thisclass_cpp = static_cast<CEGUI::DefaultResourceProvider*> (getHG3DClassPtr(*thisclass_c, "CEGUI::DefaultResourceProvider"));
  CEGUI::String resourceGroup_cpp = CEGUI::String((const char*) resourceGroup_c);
  CEGUI::String result_cpp;
  result_cpp = (thisclass_cpp->getResourceGroupDirectory(resourceGroup_cpp));
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

// clears any currently set directory for the specified resource group identifier. 
extern "C" CEGUI_LIB_EXPORT void cegui_drpr_clearResourceGroupDirectory(struct hg3dclass_struct * thisclass_c, char * resourceGroup_c)
{
  CEGUI::DefaultResourceProvider * thisclass_cpp = static_cast<CEGUI::DefaultResourceProvider*> (getHG3DClassPtr(*thisclass_c, "CEGUI::DefaultResourceProvider"));
  CEGUI::String resourceGroup_cpp = CEGUI::String((const char*) resourceGroup_c);
  (thisclass_cpp->clearResourceGroupDirectory(resourceGroup_cpp));
};

