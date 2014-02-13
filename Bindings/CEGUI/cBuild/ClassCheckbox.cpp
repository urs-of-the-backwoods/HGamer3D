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

// ClassCheckbox.cpp

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



// return true if the check-box is selected (has the checkmark) 
extern "C" CEGUI_LIB_EXPORT void cegui_chkbx_isSelected(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::Checkbox * thisclass_cpp = static_cast<CEGUI::Checkbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Checkbox"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isSelected());
  *result_c = (int)result_cpp;
};

// set whether the check-box is selected or not 
extern "C" CEGUI_LIB_EXPORT void cegui_chkbx_setSelected(struct hg3dclass_struct * thisclass_c, int select_c)
{
  CEGUI::Checkbox * thisclass_cpp = static_cast<CEGUI::Checkbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Checkbox"));
  bool select_cpp = (bool)select_c;
  (thisclass_cpp->setSelected(select_cpp));
};

// Constructor for Checkbox
extern "C" CEGUI_LIB_EXPORT void cegui_chkbx_construct(char * type_c, char * name_c, struct hg3dclass_struct * result_c)
{
  CEGUI::String type_cpp = CEGUI::String((const char*) type_c);
  CEGUI::String name_cpp = CEGUI::String((const char*) name_c);
  CEGUI::Checkbox * result_cpp;
  result_cpp = (new CEGUI::Checkbox(type_cpp, name_cpp));
  *result_c = getHG3DClass_Checkbox((void *) result_cpp);
;
};

// Destructor for Checkbox
extern "C" CEGUI_LIB_EXPORT void cegui_chkbx_destruct(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::Checkbox * thisclass_cpp = static_cast<CEGUI::Checkbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Checkbox"));
  (delete thisclass_cpp);
};

