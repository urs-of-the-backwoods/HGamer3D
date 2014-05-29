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

// ClassEventSet.cpp

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



// Constructor for EventSet
extern "C" CEGUI_LIB_EXPORT void cegui_evtst_construct(struct hg3dclass_struct * result_c)
{
  CEGUI::EventSet * result_cpp;
  result_cpp = (new CEGUI::EventSet());
  *result_c = getHG3DClass_EventSet((void *) result_cpp);
;
};

// Destructor for EventSet
extern "C" CEGUI_LIB_EXPORT void cegui_evtst_destruct(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::EventSet * thisclass_cpp = static_cast<CEGUI::EventSet*> (getHG3DClassPtr(*thisclass_c, "CEGUI::EventSet"));
  (delete thisclass_cpp);
};

// Add a new EventEventSet
extern "C" CEGUI_LIB_EXPORT void cegui_evtst_addEvent(struct hg3dclass_struct * thisclass_c, char * name_c)
{
  CEGUI::EventSet * thisclass_cpp = static_cast<CEGUI::EventSet*> (getHG3DClassPtr(*thisclass_c, "CEGUI::EventSet"));
  CEGUI::String name_cpp = CEGUI::String((const char*) name_c);
  (thisclass_cpp->addEvent(name_cpp));
};

// Removes the Event
extern "C" CEGUI_LIB_EXPORT void cegui_evtst_removeEvent(struct hg3dclass_struct * thisclass_c, char * name_c)
{
  CEGUI::EventSet * thisclass_cpp = static_cast<CEGUI::EventSet*> (getHG3DClassPtr(*thisclass_c, "CEGUI::EventSet"));
  CEGUI::String name_cpp = CEGUI::String((const char*) name_c);
  (thisclass_cpp->removeEvent(name_cpp));
};

// Remove all EventEventSet
extern "C" CEGUI_LIB_EXPORT void cegui_evtst_removeAllEvents(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::EventSet * thisclass_cpp = static_cast<CEGUI::EventSet*> (getHG3DClassPtr(*thisclass_c, "CEGUI::EventSet"));
  (thisclass_cpp->removeAllEvents());
};

// Checks to see if an EventEventSet
extern "C" CEGUI_LIB_EXPORT void cegui_evtst_isEventPresent(struct hg3dclass_struct * thisclass_c, char * name_c, int * result_c)
{
  CEGUI::EventSet * thisclass_cpp = static_cast<CEGUI::EventSet*> (getHG3DClassPtr(*thisclass_c, "CEGUI::EventSet"));
  CEGUI::String name_cpp = CEGUI::String((const char*) name_c);
  bool result_cpp;
  result_cpp = (thisclass_cpp->isEventPresent(name_cpp));
  *result_c = (int)result_cpp;
};

// Fires the named event passing the given EventArgs
extern "C" CEGUI_LIB_EXPORT void cegui_evtst_fireEvent(struct hg3dclass_struct * thisclass_c, char * name_c, struct hg3dclass_struct * args_c, char * eventNamespace_c)
{
  CEGUI::EventSet * thisclass_cpp = static_cast<CEGUI::EventSet*> (getHG3DClassPtr(*thisclass_c, "CEGUI::EventSet"));
  CEGUI::String name_cpp = CEGUI::String((const char*) name_c);
  CEGUI::EventArgs * args_cpp = static_cast<CEGUI::EventArgs*> (getHG3DClassPtr(*args_c, "CEGUI::EventArgs"));
  CEGUI::String eventNamespace_cpp = CEGUI::String((const char*) eventNamespace_c);
  (thisclass_cpp->fireEvent(name_cpp, *args_cpp, eventNamespace_cpp));
};

// Return whether the EventSet
extern "C" CEGUI_LIB_EXPORT void cegui_evtst_isMuted(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::EventSet * thisclass_cpp = static_cast<CEGUI::EventSet*> (getHG3DClassPtr(*thisclass_c, "CEGUI::EventSet"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isMuted());
  *result_c = (int)result_cpp;
};

// Set the mute state for this EventSet
extern "C" CEGUI_LIB_EXPORT void cegui_evtst_setMutedState(struct hg3dclass_struct * thisclass_c, int setting_c)
{
  CEGUI::EventSet * thisclass_cpp = static_cast<CEGUI::EventSet*> (getHG3DClassPtr(*thisclass_c, "CEGUI::EventSet"));
  bool setting_cpp = (bool)setting_c;
  (thisclass_cpp->setMutedState(setting_cpp));
};

