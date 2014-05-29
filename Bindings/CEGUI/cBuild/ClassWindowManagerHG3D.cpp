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

// ClassWindowManagerHG3D.cpp

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



// 
extern "C" CEGUI_LIB_EXPORT void cegui_wmgr_hg3d_construct(struct hg3dclass_struct * result_c)
{
  WindowManagerHG3D * result_cpp;
  result_cpp = (new WindowManagerHG3D());
  *result_c = getHG3DClass_WindowManagerHG3D((void *) result_cpp);
;
};

// 
extern "C" CEGUI_LIB_EXPORT void cegui_wmgr_hg3d_destruct(struct hg3dclass_struct * thisclass_c)
{
  WindowManagerHG3D * thisclass_cpp = static_cast<WindowManagerHG3D*> (getHG3DClassPtr(*thisclass_c, "WindowManagerHG3D"));
  (delete thisclass_cpp);
};

// 
extern "C" CEGUI_LIB_EXPORT void cegui_wmgr_hg3d_loadWindowLayoutHG3D(struct hg3dclass_struct * thisclass_c, char * filename_c, char * name_prefix_c, struct hg3dclass_struct * result_c)
{
  WindowManagerHG3D * thisclass_cpp = static_cast<WindowManagerHG3D*> (getHG3DClassPtr(*thisclass_c, "WindowManagerHG3D"));
  CEGUI::String filename_cpp = CEGUI::String((const char*) filename_c);
  CEGUI::String name_prefix_cpp = CEGUI::String((const char*) name_prefix_c);
  CEGUI::Window * result_cpp;
  result_cpp = (thisclass_cpp->loadWindowLayoutHG3D(filename_cpp, name_prefix_cpp));
  *result_c = getHG3DClass_Window((void *) result_cpp);
;
};

// 
extern "C" CEGUI_LIB_EXPORT void cegui_wmgr_hg3d_getSingleton(struct hg3dclass_struct * result_c)
{
  CEGUI::WindowManager * result_cpp;
  result_cpp = (WindowManagerHG3D::getSingleton());
  *result_c = getHG3DClass_WindowManager((void *) result_cpp);
;
};

