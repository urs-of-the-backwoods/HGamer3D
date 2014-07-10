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

// ClassHG3DWindowStaticFunctions.cpp

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
extern "C" CEGUI_LIB_EXPORT void cegui_hg3dwsfs_castWindowToPushButton(struct hg3dclass_struct * window_c, struct hg3dclass_struct * result_c)
{
  CEGUI::Window * window_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*window_c, "CEGUI::Window"));
  CEGUI::PushButton * result_cpp;
  result_cpp = (HG3DWindowStaticFunctions::castWindowToPushButton(window_cpp));
  *result_c = getHG3DClass_PushButton((void *) result_cpp);
;
};

// 
extern "C" CEGUI_LIB_EXPORT void cegui_hg3dwsfs_castWindowToListbox(struct hg3dclass_struct * window_c, struct hg3dclass_struct * result_c)
{
  CEGUI::Window * window_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*window_c, "CEGUI::Window"));
  CEGUI::Listbox * result_cpp;
  result_cpp = (HG3DWindowStaticFunctions::castWindowToListbox(window_cpp));
  *result_c = getHG3DClass_Listbox((void *) result_cpp);
;
};

// 
extern "C" CEGUI_LIB_EXPORT void cegui_hg3dwsfs_castWindowToCombobox(struct hg3dclass_struct * window_c, struct hg3dclass_struct * result_c)
{
  CEGUI::Window * window_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*window_c, "CEGUI::Window"));
  CEGUI::Combobox * result_cpp;
  result_cpp = (HG3DWindowStaticFunctions::castWindowToCombobox(window_cpp));
  *result_c = getHG3DClass_Combobox((void *) result_cpp);
;
};

// 
extern "C" CEGUI_LIB_EXPORT void cegui_hg3dwsfs_castWindowToRadioButton(struct hg3dclass_struct * window_c, struct hg3dclass_struct * result_c)
{
  CEGUI::Window * window_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*window_c, "CEGUI::Window"));
  CEGUI::RadioButton * result_cpp;
  result_cpp = (HG3DWindowStaticFunctions::castWindowToRadioButton(window_cpp));
  *result_c = getHG3DClass_RadioButton((void *) result_cpp);
;
};

// 
extern "C" CEGUI_LIB_EXPORT void cegui_hg3dwsfs_castWindowToEditbox(struct hg3dclass_struct * window_c, struct hg3dclass_struct * result_c)
{
  CEGUI::Window * window_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*window_c, "CEGUI::Window"));
  CEGUI::Editbox * result_cpp;
  result_cpp = (HG3DWindowStaticFunctions::castWindowToEditbox(window_cpp));
  *result_c = getHG3DClass_Editbox((void *) result_cpp);
;
};

// 
extern "C" CEGUI_LIB_EXPORT void cegui_hg3dwsfs_castWindowToMultiLineEditbox(struct hg3dclass_struct * window_c, struct hg3dclass_struct * result_c)
{
  CEGUI::Window * window_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*window_c, "CEGUI::Window"));
  CEGUI::MultiLineEditbox * result_cpp;
  result_cpp = (HG3DWindowStaticFunctions::castWindowToMultiLineEditbox(window_cpp));
  *result_c = getHG3DClass_MultiLineEditbox((void *) result_cpp);
;
};

// 
extern "C" CEGUI_LIB_EXPORT void cegui_hg3dwsfs_castWindowToFrameWindow(struct hg3dclass_struct * window_c, struct hg3dclass_struct * result_c)
{
  CEGUI::Window * window_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*window_c, "CEGUI::Window"));
  CEGUI::FrameWindow * result_cpp;
  result_cpp = (HG3DWindowStaticFunctions::castWindowToFrameWindow(window_cpp));
  *result_c = getHG3DClass_FrameWindow((void *) result_cpp);
;
};

// 
extern "C" CEGUI_LIB_EXPORT void cegui_hg3dwsfs_castWindowToProgressBar(struct hg3dclass_struct * window_c, struct hg3dclass_struct * result_c)
{
  CEGUI::Window * window_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*window_c, "CEGUI::Window"));
  CEGUI::ProgressBar * result_cpp;
  result_cpp = (HG3DWindowStaticFunctions::castWindowToProgressBar(window_cpp));
  *result_c = getHG3DClass_ProgressBar((void *) result_cpp);
;
};

// 
extern "C" CEGUI_LIB_EXPORT void cegui_hg3dwsfs_castWindowToSlider(struct hg3dclass_struct * window_c, struct hg3dclass_struct * result_c)
{
  CEGUI::Window * window_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*window_c, "CEGUI::Window"));
  CEGUI::Slider * result_cpp;
  result_cpp = (HG3DWindowStaticFunctions::castWindowToSlider(window_cpp));
  *result_c = getHG3DClass_Slider((void *) result_cpp);
;
};

// 
extern "C" CEGUI_LIB_EXPORT void cegui_hg3dwsfs_castWindowToSpinner(struct hg3dclass_struct * window_c, struct hg3dclass_struct * result_c)
{
  CEGUI::Window * window_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*window_c, "CEGUI::Window"));
  CEGUI::Spinner * result_cpp;
  result_cpp = (HG3DWindowStaticFunctions::castWindowToSpinner(window_cpp));
  *result_c = getHG3DClass_Spinner((void *) result_cpp);
;
};

// 
extern "C" CEGUI_LIB_EXPORT void cegui_hg3dwsfs_castWindowToMultiColumnList(struct hg3dclass_struct * window_c, struct hg3dclass_struct * result_c)
{
  CEGUI::Window * window_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*window_c, "CEGUI::Window"));
  CEGUI::MultiColumnList * result_cpp;
  result_cpp = (HG3DWindowStaticFunctions::castWindowToMultiColumnList(window_cpp));
  *result_c = getHG3DClass_MultiColumnList((void *) result_cpp);
;
};

// 
extern "C" CEGUI_LIB_EXPORT void cegui_hg3dwsfs_udScale(struct hg3dclass_struct * ud_c, float * result_c)
{
  const CEGUI::UDim * ud_cpp = static_cast<CEGUI::UDim*> (getHG3DClassPtr(*ud_c, "CEGUI::UDim"));
  float result_cpp;
  result_cpp = (HG3DWindowStaticFunctions::udScale(ud_cpp));
  *result_c = (float)result_cpp;
};

// 
extern "C" CEGUI_LIB_EXPORT void cegui_hg3dwsfs_udOffset(struct hg3dclass_struct * ud_c, float * result_c)
{
  const CEGUI::UDim * ud_cpp = static_cast<CEGUI::UDim*> (getHG3DClassPtr(*ud_c, "CEGUI::UDim"));
  float result_cpp;
  result_cpp = (HG3DWindowStaticFunctions::udOffset(ud_cpp));
  *result_c = (float)result_cpp;
};

// 
extern "C" CEGUI_LIB_EXPORT void cegui_hg3dwsfs_v2X(struct hg3dclass_struct * uv2_c, struct hg3dclass_struct * result_c)
{
  const CEGUI::UVector2 * uv2_cpp = static_cast<CEGUI::UVector2*> (getHG3DClassPtr(*uv2_c, "CEGUI::UVector2"));
  CEGUI::UDim * result_cpp;
  result_cpp = (HG3DWindowStaticFunctions::v2X(uv2_cpp));
  *result_c = getHG3DClass_UDim((void *) result_cpp);
;
};

// 
extern "C" CEGUI_LIB_EXPORT void cegui_hg3dwsfs_v2Y(struct hg3dclass_struct * uv2_c, struct hg3dclass_struct * result_c)
{
  const CEGUI::UVector2 * uv2_cpp = static_cast<CEGUI::UVector2*> (getHG3DClassPtr(*uv2_c, "CEGUI::UVector2"));
  CEGUI::UDim * result_cpp;
  result_cpp = (HG3DWindowStaticFunctions::v2Y(uv2_cpp));
  *result_c = getHG3DClass_UDim((void *) result_cpp);
;
};

// 
extern "C" CEGUI_LIB_EXPORT void cegui_hg3dwsfs_getWindowWidth(struct hg3dclass_struct * window_c, struct hg3dclass_struct * result_c)
{
  const CEGUI::Window * window_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*window_c, "CEGUI::Window"));
  CEGUI::UDim * result_cpp;
  result_cpp = (HG3DWindowStaticFunctions::getWindowWidth(window_cpp));
  *result_c = getHG3DClass_UDim((void *) result_cpp);
;
};

// 
extern "C" CEGUI_LIB_EXPORT void cegui_hg3dwsfs_getWindowHeight(struct hg3dclass_struct * window_c, struct hg3dclass_struct * result_c)
{
  const CEGUI::Window * window_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*window_c, "CEGUI::Window"));
  CEGUI::UDim * result_cpp;
  result_cpp = (HG3DWindowStaticFunctions::getWindowHeight(window_cpp));
  *result_c = getHG3DClass_UDim((void *) result_cpp);
;
};

