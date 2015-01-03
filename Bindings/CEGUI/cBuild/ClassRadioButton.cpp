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

// ClassRadioButton.cpp

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



// return true if the radio button is selected (has the checkmark) 
extern "C" CEGUI_LIB_EXPORT void cegui_rdbttn_isSelected(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::RadioButton * thisclass_cpp = static_cast<CEGUI::RadioButton*> (getHG3DClassPtr(*thisclass_c, "CEGUI::RadioButton"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isSelected());
  *result_c = (int)result_cpp;
};

// Return a pointer to the RadioButtonRadioButton
extern "C" CEGUI_LIB_EXPORT void cegui_rdbttn_getSelectedButtonInGroup(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  CEGUI::RadioButton * thisclass_cpp = static_cast<CEGUI::RadioButton*> (getHG3DClassPtr(*thisclass_c, "CEGUI::RadioButton"));
  CEGUI::RadioButton * result_cpp;
  result_cpp = (thisclass_cpp->getSelectedButtonInGroup());
  *result_c = getHG3DClass_RadioButton((void *) result_cpp);
;
};

// set whether the radio button is selected or not 
extern "C" CEGUI_LIB_EXPORT void cegui_rdbttn_setSelected(struct hg3dclass_struct * thisclass_c, int select_c)
{
  CEGUI::RadioButton * thisclass_cpp = static_cast<CEGUI::RadioButton*> (getHG3DClassPtr(*thisclass_c, "CEGUI::RadioButton"));
  bool select_cpp = (bool)select_c;
  (thisclass_cpp->setSelected(select_cpp));
};

// 
extern "C" CEGUI_LIB_EXPORT void cegui_rdbttn_construct(char * type_c, char * name_c, struct hg3dclass_struct * result_c)
{
  CEGUI::String type_cpp = CEGUI::String((const char*) type_c);
  CEGUI::String name_cpp = CEGUI::String((const char*) name_c);
  CEGUI::RadioButton * result_cpp;
  result_cpp = (new CEGUI::RadioButton(type_cpp, name_cpp));
  *result_c = getHG3DClass_RadioButton((void *) result_cpp);
;
};

// 
extern "C" CEGUI_LIB_EXPORT void cegui_rdbttn_destruct(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::RadioButton * thisclass_cpp = static_cast<CEGUI::RadioButton*> (getHG3DClassPtr(*thisclass_c, "CEGUI::RadioButton"));
  (delete thisclass_cpp);
};

