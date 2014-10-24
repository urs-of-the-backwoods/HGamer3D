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

// ClassSlider.cpp

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



// return the current slider value. 
extern "C" CEGUI_LIB_EXPORT void cegui_sldr_getCurrentValue(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  CEGUI::Slider * thisclass_cpp = static_cast<CEGUI::Slider*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Slider"));
  float result_cpp;
  result_cpp = (thisclass_cpp->getCurrentValue());
  *result_c = (float)result_cpp;
};

// return the maximum value set for this widget 
extern "C" CEGUI_LIB_EXPORT void cegui_sldr_getMaxValue(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  CEGUI::Slider * thisclass_cpp = static_cast<CEGUI::Slider*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Slider"));
  float result_cpp;
  result_cpp = (thisclass_cpp->getMaxValue());
  *result_c = (float)result_cpp;
};

// return the current click step setting for the slider. 
extern "C" CEGUI_LIB_EXPORT void cegui_sldr_getClickStep(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  CEGUI::Slider * thisclass_cpp = static_cast<CEGUI::Slider*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Slider"));
  float result_cpp;
  result_cpp = (thisclass_cpp->getClickStep());
  *result_c = (float)result_cpp;
};

// Return a pointer to the ThumbSlider
extern "C" CEGUI_LIB_EXPORT void cegui_sldr_getThumb(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  CEGUI::Slider * thisclass_cpp = static_cast<CEGUI::Slider*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Slider"));
  CEGUI::Thumb * result_cpp;
  result_cpp = (thisclass_cpp->getThumb());
  *result_c = getHG3DClass_Thumb((void *) result_cpp);
;
};

// Initialises the Window
extern "C" CEGUI_LIB_EXPORT void cegui_sldr_initialiseComponents(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::Slider * thisclass_cpp = static_cast<CEGUI::Slider*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Slider"));
  (thisclass_cpp->initialiseComponents());
};

// set the maximum value for the slider. Note that the minimum value is fixed at 0. 
extern "C" CEGUI_LIB_EXPORT void cegui_sldr_setMaxValue(struct hg3dclass_struct * thisclass_c, float maxVal_c)
{
  CEGUI::Slider * thisclass_cpp = static_cast<CEGUI::Slider*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Slider"));
  float maxVal_cpp = (float)maxVal_c;
  (thisclass_cpp->setMaxValue(maxVal_cpp));
};

// set the current slider value. 
extern "C" CEGUI_LIB_EXPORT void cegui_sldr_setCurrentValue(struct hg3dclass_struct * thisclass_c, float value_c)
{
  CEGUI::Slider * thisclass_cpp = static_cast<CEGUI::Slider*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Slider"));
  float value_cpp = (float)value_c;
  (thisclass_cpp->setCurrentValue(value_cpp));
};

// set the current click step setting for the slider. 
extern "C" CEGUI_LIB_EXPORT void cegui_sldr_setClickStep(struct hg3dclass_struct * thisclass_c, float step_c)
{
  CEGUI::Slider * thisclass_cpp = static_cast<CEGUI::Slider*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Slider"));
  float step_cpp = (float)step_c;
  (thisclass_cpp->setClickStep(step_cpp));
};

// Slider
extern "C" CEGUI_LIB_EXPORT void cegui_sldr_construct(char * type_c, char * name_c, struct hg3dclass_struct * result_c)
{
  CEGUI::String type_cpp = CEGUI::String((const char*) type_c);
  CEGUI::String name_cpp = CEGUI::String((const char*) name_c);
  CEGUI::Slider * result_cpp;
  result_cpp = (new CEGUI::Slider(type_cpp, name_cpp));
  *result_c = getHG3DClass_Slider((void *) result_cpp);
;
};

// Slider
extern "C" CEGUI_LIB_EXPORT void cegui_sldr_destruct(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::Slider * thisclass_cpp = static_cast<CEGUI::Slider*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Slider"));
  (delete thisclass_cpp);
};

