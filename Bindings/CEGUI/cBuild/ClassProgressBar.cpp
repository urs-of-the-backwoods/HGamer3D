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

// ClassProgressBar.cpp

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



// return the current progress value 
extern "C" CEGUI_LIB_EXPORT void cegui_prgbr_getProgress(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  CEGUI::ProgressBar * thisclass_cpp = static_cast<CEGUI::ProgressBar*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ProgressBar"));
  float result_cpp;
  result_cpp = (thisclass_cpp->getProgress());
  *result_c = (float)result_cpp;
};

// return the current step size 
extern "C" CEGUI_LIB_EXPORT void cegui_prgbr_getStep(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  CEGUI::ProgressBar * thisclass_cpp = static_cast<CEGUI::ProgressBar*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ProgressBar"));
  float result_cpp;
  result_cpp = (thisclass_cpp->getStep());
  *result_c = (float)result_cpp;
};

// set the current progress. 
extern "C" CEGUI_LIB_EXPORT void cegui_prgbr_setProgress(struct hg3dclass_struct * thisclass_c, float progress_c)
{
  CEGUI::ProgressBar * thisclass_cpp = static_cast<CEGUI::ProgressBar*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ProgressBar"));
  float progress_cpp = (float)progress_c;
  (thisclass_cpp->setProgress(progress_cpp));
};

// set the size of the 'step' in percentage points (default is 0.01f or 1%). 
extern "C" CEGUI_LIB_EXPORT void cegui_prgbr_setStepSize(struct hg3dclass_struct * thisclass_c, float step_val_c)
{
  CEGUI::ProgressBar * thisclass_cpp = static_cast<CEGUI::ProgressBar*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ProgressBar"));
  float step_val_cpp = (float)step_val_c;
  (thisclass_cpp->setStepSize(step_val_cpp));
};

// cause the progress to step 
extern "C" CEGUI_LIB_EXPORT void cegui_prgbr_step(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::ProgressBar * thisclass_cpp = static_cast<CEGUI::ProgressBar*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ProgressBar"));
  (thisclass_cpp->step());
};

// Modify the progress level by a specified delta. 
extern "C" CEGUI_LIB_EXPORT void cegui_prgbr_adjustProgress(struct hg3dclass_struct * thisclass_c, float delta_c)
{
  CEGUI::ProgressBar * thisclass_cpp = static_cast<CEGUI::ProgressBar*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ProgressBar"));
  float delta_cpp = (float)delta_c;
  (thisclass_cpp->adjustProgress(delta_cpp));
};

// Constructor for ProgressBar
extern "C" CEGUI_LIB_EXPORT void cegui_prgbr_construct(char * type_c, char * name_c, struct hg3dclass_struct * result_c)
{
  CEGUI::String type_cpp = CEGUI::String((const char*) type_c);
  CEGUI::String name_cpp = CEGUI::String((const char*) name_c);
  CEGUI::ProgressBar * result_cpp;
  result_cpp = (new CEGUI::ProgressBar(type_cpp, name_cpp));
  *result_c = getHG3DClass_ProgressBar((void *) result_cpp);
;
};

// Destructor for ProgressBar
extern "C" CEGUI_LIB_EXPORT void cegui_prgbr_destruct(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::ProgressBar * thisclass_cpp = static_cast<CEGUI::ProgressBar*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ProgressBar"));
  (delete thisclass_cpp);
};

