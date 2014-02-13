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

// ClassThumb.cpp

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



// return whether hot-tracking is enabled or not. 
extern "C" CEGUI_LIB_EXPORT void cegui_thmb_isHotTracked(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::Thumb * thisclass_cpp = static_cast<CEGUI::Thumb*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Thumb"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isHotTracked());
  *result_c = (int)result_cpp;
};

// return whether the thumb is movable on the vertical axis. 
extern "C" CEGUI_LIB_EXPORT void cegui_thmb_isVertFree(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::Thumb * thisclass_cpp = static_cast<CEGUI::Thumb*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Thumb"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isVertFree());
  *result_c = (int)result_cpp;
};

// return whether the thumb is movable on the horizontal axis. 
extern "C" CEGUI_LIB_EXPORT void cegui_thmb_isHorzFree(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::Thumb * thisclass_cpp = static_cast<CEGUI::Thumb*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Thumb"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isHorzFree());
  *result_c = (int)result_cpp;
};

// set whether the thumb uses hot-tracking. 
extern "C" CEGUI_LIB_EXPORT void cegui_thmb_setHotTracked(struct hg3dclass_struct * thisclass_c, int setting_c)
{
  CEGUI::Thumb * thisclass_cpp = static_cast<CEGUI::Thumb*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Thumb"));
  bool setting_cpp = (bool)setting_c;
  (thisclass_cpp->setHotTracked(setting_cpp));
};

// set whether thumb is movable on the vertical axis. 
extern "C" CEGUI_LIB_EXPORT void cegui_thmb_setVertFree(struct hg3dclass_struct * thisclass_c, int setting_c)
{
  CEGUI::Thumb * thisclass_cpp = static_cast<CEGUI::Thumb*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Thumb"));
  bool setting_cpp = (bool)setting_c;
  (thisclass_cpp->setVertFree(setting_cpp));
};

// set whether thumb is movable on the horizontal axis. 
extern "C" CEGUI_LIB_EXPORT void cegui_thmb_setHorzFree(struct hg3dclass_struct * thisclass_c, int setting_c)
{
  CEGUI::Thumb * thisclass_cpp = static_cast<CEGUI::Thumb*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Thumb"));
  bool setting_cpp = (bool)setting_c;
  (thisclass_cpp->setHorzFree(setting_cpp));
};

// set the movement range of the thumb for the vertical axis. 
extern "C" CEGUI_LIB_EXPORT void cegui_thmb_setVertRange(struct hg3dclass_struct * thisclass_c, float min_c, float max_c)
{
  CEGUI::Thumb * thisclass_cpp = static_cast<CEGUI::Thumb*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Thumb"));
  float min_cpp = (float)min_c;
  float max_cpp = (float)max_c;
  (thisclass_cpp->setVertRange(min_cpp, max_cpp));
};

// set the movement range of the thumb for the horizontal axis. 
extern "C" CEGUI_LIB_EXPORT void cegui_thmb_setHorzRange(struct hg3dclass_struct * thisclass_c, float min_c, float max_c)
{
  CEGUI::Thumb * thisclass_cpp = static_cast<CEGUI::Thumb*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Thumb"));
  float min_cpp = (float)min_c;
  float max_cpp = (float)max_c;
  (thisclass_cpp->setHorzRange(min_cpp, max_cpp));
};

// Constructor for Thumb
extern "C" CEGUI_LIB_EXPORT void cegui_thmb_construct(char * type_c, char * name_c, struct hg3dclass_struct * result_c)
{
  CEGUI::String type_cpp = CEGUI::String((const char*) type_c);
  CEGUI::String name_cpp = CEGUI::String((const char*) name_c);
  CEGUI::Thumb * result_cpp;
  result_cpp = (new CEGUI::Thumb(type_cpp, name_cpp));
  *result_c = getHG3DClass_Thumb((void *) result_cpp);
;
};

// Destructor for Thumb
extern "C" CEGUI_LIB_EXPORT void cegui_thmb_destruct(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::Thumb * thisclass_cpp = static_cast<CEGUI::Thumb*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Thumb"));
  (delete thisclass_cpp);
};

