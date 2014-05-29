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

// ClassScrolledContainer.cpp

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



// Constructor for ScrolledContainer
extern "C" CEGUI_LIB_EXPORT void cegui_scrlcnt_construct(char * type_c, char * name_c, struct hg3dclass_struct * result_c)
{
  CEGUI::String type_cpp = CEGUI::String((const char*) type_c);
  CEGUI::String name_cpp = CEGUI::String((const char*) name_c);
  CEGUI::ScrolledContainer * result_cpp;
  result_cpp = (new CEGUI::ScrolledContainer(type_cpp, name_cpp));
  *result_c = getHG3DClass_ScrolledContainer((void *) result_cpp);
;
};

// Destructor for ScrolledContainer
extern "C" CEGUI_LIB_EXPORT void cegui_scrlcnt_destruct(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::ScrolledContainer * thisclass_cpp = static_cast<CEGUI::ScrolledContainer*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ScrolledContainer"));
  (delete thisclass_cpp);
};

// Return whether the content pane is auto sized. 
extern "C" CEGUI_LIB_EXPORT void cegui_scrlcnt_isContentPaneAutoSized(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::ScrolledContainer * thisclass_cpp = static_cast<CEGUI::ScrolledContainer*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ScrolledContainer"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isContentPaneAutoSized());
  *result_c = (int)result_cpp;
};

// Set whether the content pane should be auto-sized. 
extern "C" CEGUI_LIB_EXPORT void cegui_scrlcnt_setContentPaneAutoSized(struct hg3dclass_struct * thisclass_c, int setting_c)
{
  CEGUI::ScrolledContainer * thisclass_cpp = static_cast<CEGUI::ScrolledContainer*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ScrolledContainer"));
  bool setting_cpp = (bool)setting_c;
  (thisclass_cpp->setContentPaneAutoSized(setting_cpp));
};

