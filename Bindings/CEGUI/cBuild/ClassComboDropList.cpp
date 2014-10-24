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

// ClassComboDropList.cpp

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



// Initialise the Window
extern "C" CEGUI_LIB_EXPORT void cegui_cmbdrplst_initialiseComponents(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::ComboDropList * thisclass_cpp = static_cast<CEGUI::ComboDropList*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ComboDropList"));
  (thisclass_cpp->initialiseComponents());
};

// Set whether the drop-list is 'armed' for selection. 
extern "C" CEGUI_LIB_EXPORT void cegui_cmbdrplst_setArmed(struct hg3dclass_struct * thisclass_c, int setting_c)
{
  CEGUI::ComboDropList * thisclass_cpp = static_cast<CEGUI::ComboDropList*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ComboDropList"));
  bool setting_cpp = (bool)setting_c;
  (thisclass_cpp->setArmed(setting_cpp));
};

// Return the 'armed' state of the ComboDropList
extern "C" CEGUI_LIB_EXPORT void cegui_cmbdrplst_isArmed(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::ComboDropList * thisclass_cpp = static_cast<CEGUI::ComboDropList*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ComboDropList"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isArmed());
  *result_c = (int)result_cpp;
};

// Set the mode of operation for the ComboDropList
extern "C" CEGUI_LIB_EXPORT void cegui_cmbdrplst_setAutoArmEnabled(struct hg3dclass_struct * thisclass_c, int setting_c)
{
  CEGUI::ComboDropList * thisclass_cpp = static_cast<CEGUI::ComboDropList*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ComboDropList"));
  bool setting_cpp = (bool)setting_c;
  (thisclass_cpp->setAutoArmEnabled(setting_cpp));
};

// returns the mode of operation for the drop-list 
extern "C" CEGUI_LIB_EXPORT void cegui_cmbdrplst_isAutoArmEnabled(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::ComboDropList * thisclass_cpp = static_cast<CEGUI::ComboDropList*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ComboDropList"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isAutoArmEnabled());
  *result_c = (int)result_cpp;
};

// Constructor for ComboDropList
extern "C" CEGUI_LIB_EXPORT void cegui_cmbdrplst_construct(char * type_c, char * name_c, struct hg3dclass_struct * result_c)
{
  CEGUI::String type_cpp = CEGUI::String((const char*) type_c);
  CEGUI::String name_cpp = CEGUI::String((const char*) name_c);
  CEGUI::ComboDropList * result_cpp;
  result_cpp = (new CEGUI::ComboDropList(type_cpp, name_cpp));
  *result_c = getHG3DClass_ComboDropList((void *) result_cpp);
;
};

// Destructor for ComboDropList
extern "C" CEGUI_LIB_EXPORT void cegui_cmbdrplst_destruct(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::ComboDropList * thisclass_cpp = static_cast<CEGUI::ComboDropList*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ComboDropList"));
  (delete thisclass_cpp);
};

