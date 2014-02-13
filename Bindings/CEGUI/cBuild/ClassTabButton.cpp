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

// ClassTabButton.cpp

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



// Constructor for base TabButton
extern "C" CEGUI_LIB_EXPORT void cegui_tbbtn_construct(char * type_c, char * name_c, struct hg3dclass_struct * result_c)
{
  CEGUI::String type_cpp = CEGUI::String((const char*) type_c);
  CEGUI::String name_cpp = CEGUI::String((const char*) name_c);
  CEGUI::TabButton * result_cpp;
  result_cpp = (new CEGUI::TabButton(type_cpp, name_cpp));
  *result_c = getHG3DClass_TabButton((void *) result_cpp);
;
};

// Destructor for TabButton
extern "C" CEGUI_LIB_EXPORT void cegui_tbbtn_destruct(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::TabButton * thisclass_cpp = static_cast<CEGUI::TabButton*> (getHG3DClassPtr(*thisclass_c, "CEGUI::TabButton"));
  (delete thisclass_cpp);
};

// Set whether this tab button is selected or not. 
extern "C" CEGUI_LIB_EXPORT void cegui_tbbtn_setSelected(struct hg3dclass_struct * thisclass_c, int selected_c)
{
  CEGUI::TabButton * thisclass_cpp = static_cast<CEGUI::TabButton*> (getHG3DClassPtr(*thisclass_c, "CEGUI::TabButton"));
  bool selected_cpp = (bool)selected_c;
  (thisclass_cpp->setSelected(selected_cpp));
};

// Return whether this tab button is selected or not. 
extern "C" CEGUI_LIB_EXPORT void cegui_tbbtn_isSelected(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::TabButton * thisclass_cpp = static_cast<CEGUI::TabButton*> (getHG3DClassPtr(*thisclass_c, "CEGUI::TabButton"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isSelected());
  *result_c = (int)result_cpp;
};

// Set the target window which is the content pane which this button is covering. 
extern "C" CEGUI_LIB_EXPORT void cegui_tbbtn_setTargetWindow(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * wnd_c)
{
  CEGUI::TabButton * thisclass_cpp = static_cast<CEGUI::TabButton*> (getHG3DClassPtr(*thisclass_c, "CEGUI::TabButton"));
  CEGUI::Window * wnd_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*wnd_c, "CEGUI::Window"));
  (thisclass_cpp->setTargetWindow(wnd_cpp));
};

// Get the target window which is the content pane which this button is covering. 
extern "C" CEGUI_LIB_EXPORT void cegui_tbbtn_getTargetWindow(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  CEGUI::TabButton * thisclass_cpp = static_cast<CEGUI::TabButton*> (getHG3DClassPtr(*thisclass_c, "CEGUI::TabButton"));
  CEGUI::Window * result_cpp;
  result_cpp = (thisclass_cpp->getTargetWindow());
  *result_c = getHG3DClass_Window((void *) result_cpp);
;
};

