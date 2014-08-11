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

// ClassSpinner.cpp

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
	#include "EnumTextInputMode.h"
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



// Constructor for Spinner
extern "C" CEGUI_LIB_EXPORT void cegui_spnnr_construct(char * type_c, char * name_c, struct hg3dclass_struct * result_c)
{
  CEGUI::String type_cpp = CEGUI::String((const char*) type_c);
  CEGUI::String name_cpp = CEGUI::String((const char*) name_c);
  CEGUI::Spinner * result_cpp;
  result_cpp = (new CEGUI::Spinner(type_cpp, name_cpp));
  *result_c = getHG3DClass_Spinner((void *) result_cpp);
;
};

// Destructor for Spinner
extern "C" CEGUI_LIB_EXPORT void cegui_spnnr_destruct(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::Spinner * thisclass_cpp = static_cast<CEGUI::Spinner*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Spinner"));
  (delete thisclass_cpp);
};

// Initialises the Window
extern "C" CEGUI_LIB_EXPORT void cegui_spnnr_initialiseComponents(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::Spinner * thisclass_cpp = static_cast<CEGUI::Spinner*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Spinner"));
  (thisclass_cpp->initialiseComponents());
};

// Return the current spinner value. 
extern "C" CEGUI_LIB_EXPORT void cegui_spnnr_getCurrentValue(struct hg3dclass_struct * thisclass_c, double * result_c)
{
  CEGUI::Spinner * thisclass_cpp = static_cast<CEGUI::Spinner*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Spinner"));
  double result_cpp;
  result_cpp = (thisclass_cpp->getCurrentValue());
  *result_c = (double)result_cpp;
};

// Return the current step value. 
extern "C" CEGUI_LIB_EXPORT void cegui_spnnr_getStepSize(struct hg3dclass_struct * thisclass_c, double * result_c)
{
  CEGUI::Spinner * thisclass_cpp = static_cast<CEGUI::Spinner*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Spinner"));
  double result_cpp;
  result_cpp = (thisclass_cpp->getStepSize());
  *result_c = (double)result_cpp;
};

// Return the current maximum limit value for the Spinner
extern "C" CEGUI_LIB_EXPORT void cegui_spnnr_getMaximumValue(struct hg3dclass_struct * thisclass_c, double * result_c)
{
  CEGUI::Spinner * thisclass_cpp = static_cast<CEGUI::Spinner*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Spinner"));
  double result_cpp;
  result_cpp = (thisclass_cpp->getMaximumValue());
  *result_c = (double)result_cpp;
};

// Return the current minimum limit value for the Spinner
extern "C" CEGUI_LIB_EXPORT void cegui_spnnr_getMinimumValue(struct hg3dclass_struct * thisclass_c, double * result_c)
{
  CEGUI::Spinner * thisclass_cpp = static_cast<CEGUI::Spinner*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Spinner"));
  double result_cpp;
  result_cpp = (thisclass_cpp->getMinimumValue());
  *result_c = (double)result_cpp;
};

// Return the current text input / display mode setting. 
extern "C" CEGUI_LIB_EXPORT void cegui_spnnr_getTextInputMode(struct hg3dclass_struct * thisclass_c, enum EnumTextInputMode * result_c)
{
  CEGUI::Spinner * thisclass_cpp = static_cast<CEGUI::Spinner*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Spinner"));
  enum CEGUI::Spinner::TextInputMode result_cpp;
  result_cpp = (thisclass_cpp->getTextInputMode());
  *result_c = (enum EnumTextInputMode) result_cpp;
};

// Set the current spinner value. 
extern "C" CEGUI_LIB_EXPORT void cegui_spnnr_setCurrentValue(struct hg3dclass_struct * thisclass_c, double value_c)
{
  CEGUI::Spinner * thisclass_cpp = static_cast<CEGUI::Spinner*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Spinner"));
  double value_cpp = (double)value_c;
  (thisclass_cpp->setCurrentValue(value_cpp));
};

// Set the current step value. 
extern "C" CEGUI_LIB_EXPORT void cegui_spnnr_setStepSize(struct hg3dclass_struct * thisclass_c, double step_c)
{
  CEGUI::Spinner * thisclass_cpp = static_cast<CEGUI::Spinner*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Spinner"));
  double step_cpp = (double)step_c;
  (thisclass_cpp->setStepSize(step_cpp));
};

// Set the spinner maximum value. 
extern "C" CEGUI_LIB_EXPORT void cegui_spnnr_setMaximumValue(struct hg3dclass_struct * thisclass_c, double maxValue_c)
{
  CEGUI::Spinner * thisclass_cpp = static_cast<CEGUI::Spinner*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Spinner"));
  double maxValue_cpp = (double)maxValue_c;
  (thisclass_cpp->setMaximumValue(maxValue_cpp));
};

// Set the spinner minimum value. 
extern "C" CEGUI_LIB_EXPORT void cegui_spnnr_setMinimumValue(struct hg3dclass_struct * thisclass_c, double minVaue_c)
{
  CEGUI::Spinner * thisclass_cpp = static_cast<CEGUI::Spinner*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Spinner"));
  double minVaue_cpp = (double)minVaue_c;
  (thisclass_cpp->setMinimumValue(minVaue_cpp));
};

// Set the spinner input / display mode. 
extern "C" CEGUI_LIB_EXPORT void cegui_spnnr_setTextInputMode(struct hg3dclass_struct * thisclass_c, enum EnumTextInputMode mode_c)
{
  CEGUI::Spinner * thisclass_cpp = static_cast<CEGUI::Spinner*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Spinner"));
  enum CEGUI::Spinner::TextInputMode mode_cpp = (enum CEGUI::Spinner::TextInputMode)mode_c;
  (thisclass_cpp->setTextInputMode(mode_cpp));
};

