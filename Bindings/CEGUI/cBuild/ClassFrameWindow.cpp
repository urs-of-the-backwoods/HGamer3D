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

// ClassFrameWindow.cpp

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



// Initialises the Window
extern "C" CEGUI_LIB_EXPORT void cegui_frmwndw_initialiseComponents(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::FrameWindow * thisclass_cpp = static_cast<CEGUI::FrameWindow*> (getHG3DClassPtr(*thisclass_c, "CEGUI::FrameWindow"));
  (thisclass_cpp->initialiseComponents());
};

// Return whether this window is sizable. Note that this requires that the window have an enabled frame and that sizing itself is enabled. 
extern "C" CEGUI_LIB_EXPORT void cegui_frmwndw_isSizingEnabled(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::FrameWindow * thisclass_cpp = static_cast<CEGUI::FrameWindow*> (getHG3DClassPtr(*thisclass_c, "CEGUI::FrameWindow"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isSizingEnabled());
  *result_c = (int)result_cpp;
};

// Return whether the frame for this window is enabled. 
extern "C" CEGUI_LIB_EXPORT void cegui_frmwndw_isFrameEnabled(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::FrameWindow * thisclass_cpp = static_cast<CEGUI::FrameWindow*> (getHG3DClassPtr(*thisclass_c, "CEGUI::FrameWindow"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isFrameEnabled());
  *result_c = (int)result_cpp;
};

// Return whether the title bar for this window is enabled. 
extern "C" CEGUI_LIB_EXPORT void cegui_frmwndw_isTitleBarEnabled(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::FrameWindow * thisclass_cpp = static_cast<CEGUI::FrameWindow*> (getHG3DClassPtr(*thisclass_c, "CEGUI::FrameWindow"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isTitleBarEnabled());
  *result_c = (int)result_cpp;
};

// Return whether this close button for this window is enabled. 
extern "C" CEGUI_LIB_EXPORT void cegui_frmwndw_isCloseButtonEnabled(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::FrameWindow * thisclass_cpp = static_cast<CEGUI::FrameWindow*> (getHG3DClassPtr(*thisclass_c, "CEGUI::FrameWindow"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isCloseButtonEnabled());
  *result_c = (int)result_cpp;
};

// Return whether roll up (a.k.a shading) is enabled for this window. 
extern "C" CEGUI_LIB_EXPORT void cegui_frmwndw_isRollupEnabled(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::FrameWindow * thisclass_cpp = static_cast<CEGUI::FrameWindow*> (getHG3DClassPtr(*thisclass_c, "CEGUI::FrameWindow"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isRollupEnabled());
  *result_c = (int)result_cpp;
};

// Return whether the window is currently rolled up (a.k.a shaded). 
extern "C" CEGUI_LIB_EXPORT void cegui_frmwndw_isRolledup(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::FrameWindow * thisclass_cpp = static_cast<CEGUI::FrameWindow*> (getHG3DClassPtr(*thisclass_c, "CEGUI::FrameWindow"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isRolledup());
  *result_c = (int)result_cpp;
};

// Return the thickness of the sizing border. 
extern "C" CEGUI_LIB_EXPORT void cegui_frmwndw_getSizingBorderThickness(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  CEGUI::FrameWindow * thisclass_cpp = static_cast<CEGUI::FrameWindow*> (getHG3DClassPtr(*thisclass_c, "CEGUI::FrameWindow"));
  float result_cpp;
  result_cpp = (thisclass_cpp->getSizingBorderThickness());
  *result_c = (float)result_cpp;
};

// Enables or disables sizing for this window. 
extern "C" CEGUI_LIB_EXPORT void cegui_frmwndw_setSizingEnabled(struct hg3dclass_struct * thisclass_c, int setting_c)
{
  CEGUI::FrameWindow * thisclass_cpp = static_cast<CEGUI::FrameWindow*> (getHG3DClassPtr(*thisclass_c, "CEGUI::FrameWindow"));
  bool setting_cpp = (bool)setting_c;
  (thisclass_cpp->setSizingEnabled(setting_cpp));
};

// Enables or disables the frame for this window. 
extern "C" CEGUI_LIB_EXPORT void cegui_frmwndw_setFrameEnabled(struct hg3dclass_struct * thisclass_c, int setting_c)
{
  CEGUI::FrameWindow * thisclass_cpp = static_cast<CEGUI::FrameWindow*> (getHG3DClassPtr(*thisclass_c, "CEGUI::FrameWindow"));
  bool setting_cpp = (bool)setting_c;
  (thisclass_cpp->setFrameEnabled(setting_cpp));
};

// Enables or disables the title bar for the frame window. 
extern "C" CEGUI_LIB_EXPORT void cegui_frmwndw_setTitleBarEnabled(struct hg3dclass_struct * thisclass_c, int setting_c)
{
  CEGUI::FrameWindow * thisclass_cpp = static_cast<CEGUI::FrameWindow*> (getHG3DClassPtr(*thisclass_c, "CEGUI::FrameWindow"));
  bool setting_cpp = (bool)setting_c;
  (thisclass_cpp->setTitleBarEnabled(setting_cpp));
};

// Enables or disables the close button for the frame window. 
extern "C" CEGUI_LIB_EXPORT void cegui_frmwndw_setCloseButtonEnabled(struct hg3dclass_struct * thisclass_c, int setting_c)
{
  CEGUI::FrameWindow * thisclass_cpp = static_cast<CEGUI::FrameWindow*> (getHG3DClassPtr(*thisclass_c, "CEGUI::FrameWindow"));
  bool setting_cpp = (bool)setting_c;
  (thisclass_cpp->setCloseButtonEnabled(setting_cpp));
};

// Enables or disables roll-up (shading) for this window. 
extern "C" CEGUI_LIB_EXPORT void cegui_frmwndw_setRollupEnabled(struct hg3dclass_struct * thisclass_c, int setting_c)
{
  CEGUI::FrameWindow * thisclass_cpp = static_cast<CEGUI::FrameWindow*> (getHG3DClassPtr(*thisclass_c, "CEGUI::FrameWindow"));
  bool setting_cpp = (bool)setting_c;
  (thisclass_cpp->setRollupEnabled(setting_cpp));
};

// Toggles the state of the window between rolled-up (shaded) and normal sizes. This requires roll-up to be enabled. 
extern "C" CEGUI_LIB_EXPORT void cegui_frmwndw_toggleRollup(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::FrameWindow * thisclass_cpp = static_cast<CEGUI::FrameWindow*> (getHG3DClassPtr(*thisclass_c, "CEGUI::FrameWindow"));
  (thisclass_cpp->toggleRollup());
};

// Set the size of the sizing border for this window. 
extern "C" CEGUI_LIB_EXPORT void cegui_frmwndw_setSizingBorderThickness(struct hg3dclass_struct * thisclass_c, float pixels_c)
{
  CEGUI::FrameWindow * thisclass_cpp = static_cast<CEGUI::FrameWindow*> (getHG3DClassPtr(*thisclass_c, "CEGUI::FrameWindow"));
  float pixels_cpp = (float)pixels_c;
  (thisclass_cpp->setSizingBorderThickness(pixels_cpp));
};

// Return whether this FrameWindow
extern "C" CEGUI_LIB_EXPORT void cegui_frmwndw_isDragMovingEnabled(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::FrameWindow * thisclass_cpp = static_cast<CEGUI::FrameWindow*> (getHG3DClassPtr(*thisclass_c, "CEGUI::FrameWindow"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isDragMovingEnabled());
  *result_c = (int)result_cpp;
};

// Set whether this FrameWindow
extern "C" CEGUI_LIB_EXPORT void cegui_frmwndw_setDragMovingEnabled(struct hg3dclass_struct * thisclass_c, int setting_c)
{
  CEGUI::FrameWindow * thisclass_cpp = static_cast<CEGUI::FrameWindow*> (getHG3DClassPtr(*thisclass_c, "CEGUI::FrameWindow"));
  bool setting_cpp = (bool)setting_c;
  (thisclass_cpp->setDragMovingEnabled(setting_cpp));
};

// Set the image to be used for the north-south sizing mouse cursor. 
extern "C" CEGUI_LIB_EXPORT void cegui_frmwndw_setNSSizingCursorImage2(struct hg3dclass_struct * thisclass_c, char * imageset_c, char * image_c)
{
  CEGUI::FrameWindow * thisclass_cpp = static_cast<CEGUI::FrameWindow*> (getHG3DClassPtr(*thisclass_c, "CEGUI::FrameWindow"));
  CEGUI::String imageset_cpp = CEGUI::String((const char*) imageset_c);
  CEGUI::String image_cpp = CEGUI::String((const char*) image_c);
  (thisclass_cpp->setNSSizingCursorImage(imageset_cpp, image_cpp));
};

// Set the image to be used for the east-west sizing mouse cursor. 
extern "C" CEGUI_LIB_EXPORT void cegui_frmwndw_setEWSizingCursorImage2(struct hg3dclass_struct * thisclass_c, char * imageset_c, char * image_c)
{
  CEGUI::FrameWindow * thisclass_cpp = static_cast<CEGUI::FrameWindow*> (getHG3DClassPtr(*thisclass_c, "CEGUI::FrameWindow"));
  CEGUI::String imageset_cpp = CEGUI::String((const char*) imageset_c);
  CEGUI::String image_cpp = CEGUI::String((const char*) image_c);
  (thisclass_cpp->setEWSizingCursorImage(imageset_cpp, image_cpp));
};

// Set the image to be used for the northwest-southeast sizing mouse cursor. 
extern "C" CEGUI_LIB_EXPORT void cegui_frmwndw_setNWSESizingCursorImage2(struct hg3dclass_struct * thisclass_c, char * imageset_c, char * image_c)
{
  CEGUI::FrameWindow * thisclass_cpp = static_cast<CEGUI::FrameWindow*> (getHG3DClassPtr(*thisclass_c, "CEGUI::FrameWindow"));
  CEGUI::String imageset_cpp = CEGUI::String((const char*) imageset_c);
  CEGUI::String image_cpp = CEGUI::String((const char*) image_c);
  (thisclass_cpp->setNWSESizingCursorImage(imageset_cpp, image_cpp));
};

// Set the image to be used for the northeast-southwest sizing mouse cursor. 
extern "C" CEGUI_LIB_EXPORT void cegui_frmwndw_setNESWSizingCursorImage2(struct hg3dclass_struct * thisclass_c, char * imageset_c, char * image_c)
{
  CEGUI::FrameWindow * thisclass_cpp = static_cast<CEGUI::FrameWindow*> (getHG3DClassPtr(*thisclass_c, "CEGUI::FrameWindow"));
  CEGUI::String imageset_cpp = CEGUI::String((const char*) imageset_c);
  CEGUI::String image_cpp = CEGUI::String((const char*) image_c);
  (thisclass_cpp->setNESWSizingCursorImage(imageset_cpp, image_cpp));
};

// Return a pointer to the close button component widget for this FrameWindow
extern "C" CEGUI_LIB_EXPORT void cegui_frmwndw_getCloseButton(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  CEGUI::FrameWindow * thisclass_cpp = static_cast<CEGUI::FrameWindow*> (getHG3DClassPtr(*thisclass_c, "CEGUI::FrameWindow"));
  CEGUI::PushButton * result_cpp;
  result_cpp = (thisclass_cpp->getCloseButton());
  *result_c = getHG3DClass_PushButton((void *) result_cpp);
;
};

// Constructor for FrameWindow
extern "C" CEGUI_LIB_EXPORT void cegui_frmwndw_construct(char * type_c, char * name_c, struct hg3dclass_struct * result_c)
{
  CEGUI::String type_cpp = CEGUI::String((const char*) type_c);
  CEGUI::String name_cpp = CEGUI::String((const char*) name_c);
  CEGUI::FrameWindow * result_cpp;
  result_cpp = (new CEGUI::FrameWindow(type_cpp, name_cpp));
  *result_c = getHG3DClass_FrameWindow((void *) result_cpp);
;
};

// Destructor for FramwWindow objects. 
extern "C" CEGUI_LIB_EXPORT void cegui_frmwndw_destruct(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::FrameWindow * thisclass_cpp = static_cast<CEGUI::FrameWindow*> (getHG3DClassPtr(*thisclass_c, "CEGUI::FrameWindow"));
  (delete thisclass_cpp);
};

