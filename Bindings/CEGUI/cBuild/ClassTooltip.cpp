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

// ClassTooltip.cpp

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



// Constructor for the Tooltip
extern "C" CEGUI_LIB_EXPORT void cegui_tltp_construct(char * type_c, char * name_c, struct hg3dclass_struct * result_c)
{
  CEGUI::String type_cpp = CEGUI::String((const char*) type_c);
  CEGUI::String name_cpp = CEGUI::String((const char*) name_c);
  CEGUI::Tooltip * result_cpp;
  result_cpp = (new CEGUI::Tooltip(type_cpp, name_cpp));
  *result_c = getHG3DClass_Tooltip((void *) result_cpp);
;
};

// Destructor for the Tooltip
extern "C" CEGUI_LIB_EXPORT void cegui_tltp_destruct(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::Tooltip * thisclass_cpp = static_cast<CEGUI::Tooltip*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Tooltip"));
  (delete thisclass_cpp);
};

// Sets the target window for the tooltip. This used internally to manage tooltips, you should not have to call this yourself. 
extern "C" CEGUI_LIB_EXPORT void cegui_tltp_setTargetWindow(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * wnd_c)
{
  CEGUI::Tooltip * thisclass_cpp = static_cast<CEGUI::Tooltip*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Tooltip"));
  CEGUI::Window * wnd_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*wnd_c, "CEGUI::Window"));
  (thisclass_cpp->setTargetWindow(wnd_cpp));
};

// return the current target window for this Tooltip
extern "C" CEGUI_LIB_EXPORT void cegui_tltp_getTargetWindow(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  CEGUI::Tooltip * thisclass_cpp = static_cast<CEGUI::Tooltip*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Tooltip"));
  const CEGUI::Window * result_cpp;
  result_cpp = (thisclass_cpp->getTargetWindow());
  *result_c = getHG3DClass_Window((void *) result_cpp);
;
};

// Resets the timer on the tooltip when in the Active / Inactive states. This is used internally to control the tooltip, it is not normally necessary to call this method yourself. 
extern "C" CEGUI_LIB_EXPORT void cegui_tltp_resetTimer(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::Tooltip * thisclass_cpp = static_cast<CEGUI::Tooltip*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Tooltip"));
  (thisclass_cpp->resetTimer());
};

// Return the number of seconds the mouse should hover stationary over the target window before the tooltip gets activated. 
extern "C" CEGUI_LIB_EXPORT void cegui_tltp_getHoverTime(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  CEGUI::Tooltip * thisclass_cpp = static_cast<CEGUI::Tooltip*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Tooltip"));
  float result_cpp;
  result_cpp = (thisclass_cpp->getHoverTime());
  *result_c = (float)result_cpp;
};

// Set the number of seconds the tooltip should be displayed for before it automatically de-activates itself. 0 indicates that the tooltip should never timesout and auto-deactivate. 
extern "C" CEGUI_LIB_EXPORT void cegui_tltp_setDisplayTime(struct hg3dclass_struct * thisclass_c, float seconds_c)
{
  CEGUI::Tooltip * thisclass_cpp = static_cast<CEGUI::Tooltip*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Tooltip"));
  float seconds_cpp = (float)seconds_c;
  (thisclass_cpp->setDisplayTime(seconds_cpp));
};

// Return the number of seconds that should be taken to fade the tooltip into and out of visibility. 
extern "C" CEGUI_LIB_EXPORT void cegui_tltp_getFadeTime(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  CEGUI::Tooltip * thisclass_cpp = static_cast<CEGUI::Tooltip*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Tooltip"));
  float result_cpp;
  result_cpp = (thisclass_cpp->getFadeTime());
  *result_c = (float)result_cpp;
};

// Set the number of seconds the mouse should hover stationary over the target window before the tooltip gets activated. 
extern "C" CEGUI_LIB_EXPORT void cegui_tltp_setHoverTime(struct hg3dclass_struct * thisclass_c, float seconds_c)
{
  CEGUI::Tooltip * thisclass_cpp = static_cast<CEGUI::Tooltip*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Tooltip"));
  float seconds_cpp = (float)seconds_c;
  (thisclass_cpp->setHoverTime(seconds_cpp));
};

// Return the number of seconds the tooltip should be displayed for before it automatically de-activates itself. 0 indicates that the tooltip never timesout and auto-deactivates. 
extern "C" CEGUI_LIB_EXPORT void cegui_tltp_getDisplayTime(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  CEGUI::Tooltip * thisclass_cpp = static_cast<CEGUI::Tooltip*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Tooltip"));
  float result_cpp;
  result_cpp = (thisclass_cpp->getDisplayTime());
  *result_c = (float)result_cpp;
};

// Set the number of seconds that should be taken to fade the tooltip into and out of visibility. 
extern "C" CEGUI_LIB_EXPORT void cegui_tltp_setFadeTime(struct hg3dclass_struct * thisclass_c, float seconds_c)
{
  CEGUI::Tooltip * thisclass_cpp = static_cast<CEGUI::Tooltip*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Tooltip"));
  float seconds_cpp = (float)seconds_c;
  (thisclass_cpp->setFadeTime(seconds_cpp));
};

// Causes the tooltip to position itself appropriately. 
extern "C" CEGUI_LIB_EXPORT void cegui_tltp_positionSelf(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::Tooltip * thisclass_cpp = static_cast<CEGUI::Tooltip*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Tooltip"));
  (thisclass_cpp->positionSelf());
};

// Causes the tooltip to resize itself appropriately. 
extern "C" CEGUI_LIB_EXPORT void cegui_tltp_sizeSelf(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::Tooltip * thisclass_cpp = static_cast<CEGUI::Tooltip*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Tooltip"));
  (thisclass_cpp->sizeSelf());
};

