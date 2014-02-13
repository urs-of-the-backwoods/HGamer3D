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

// ClassHG3DEventController.cpp

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



// 
extern "C" CEGUI_LIB_EXPORT void construct(struct hg3dclass_struct * result_c)
{
  HG3DEventController * result_cpp;
  result_cpp = (new HG3DEventController());
  *result_c = getHG3DClass_HG3DEventController((void *) result_cpp);
;
};

// 
extern "C" CEGUI_LIB_EXPORT void destruct(struct hg3dclass_struct * thisclass_c)
{
  HG3DEventController * thisclass_cpp = static_cast<HG3DEventController*> (getHG3DClassPtr(*thisclass_c, "HG3DEventController"));
  (delete thisclass_cpp);
};

// 
extern "C" CEGUI_LIB_EXPORT void pushEvent(struct hg3dclass_struct * thisclass_c, char * name_c, char * sender_c, struct hg3dclass_struct * window_c)
{
  HG3DEventController * thisclass_cpp = static_cast<HG3DEventController*> (getHG3DClassPtr(*thisclass_c, "HG3DEventController"));
  CEGUI::String name_cpp = CEGUI::String((const char*) name_c);
  CEGUI::String sender_cpp = CEGUI::String((const char*) sender_c);
  CEGUI::Window * window_cpp = static_cast<CEGUI::Window*> (getHG3DClassPtr(*window_c, "CEGUI::Window"));
  (thisclass_cpp->pushEvent(name_cpp, sender_cpp, window_cpp));
};

// 
extern "C" CEGUI_LIB_EXPORT void eventsAvailable(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  HG3DEventController * thisclass_cpp = static_cast<HG3DEventController*> (getHG3DClassPtr(*thisclass_c, "HG3DEventController"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->eventsAvailable());
  *result_c = (int)result_cpp;
};

// 
extern "C" CEGUI_LIB_EXPORT void popEvent(struct hg3dclass_struct * thisclass_c, char * name_c, char * sender_c, struct hg3dclass_struct * window_c)
{
  HG3DEventController * thisclass_cpp = static_cast<HG3DEventController*> (getHG3DClassPtr(*thisclass_c, "HG3DEventController"));
  CEGUI::String name_cpp;
  CEGUI::String sender_cpp;
  CEGUI::Window * window_cpp;
  (thisclass_cpp->popEvent(name_cpp, sender_cpp, &window_cpp));
  if (strlen( (char *) name_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(name_c, (char *) name_cpp.c_str()); 
  } else {
    strcpy(name_c, "error: outstring larger then 64k");
  };
  if (strlen( (char *) sender_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(sender_c, (char *) sender_cpp.c_str()); 
  } else {
    strcpy(sender_c, "error: outstring larger then 64k");
  };
  *window_c = getHG3DClass_Window((void *) window_cpp);
;
};

