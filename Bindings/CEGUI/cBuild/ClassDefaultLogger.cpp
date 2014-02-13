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

// ClassDefaultLogger.cpp

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
	#include "EnumLoggingLevel.h"
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



// Constructor for DefaultLogger
extern "C" CEGUI_LIB_EXPORT void cegui_dfltlgr_construct(struct hg3dclass_struct * result_c)
{
  CEGUI::DefaultLogger * result_cpp;
  result_cpp = (new CEGUI::DefaultLogger());
  *result_c = getHG3DClass_DefaultLogger((void *) result_cpp);
;
};

// Destructor for DefaultLogger
extern "C" CEGUI_LIB_EXPORT void cegui_dfltlgr_destruct(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::DefaultLogger * thisclass_cpp = static_cast<CEGUI::DefaultLogger*> (getHG3DClassPtr(*thisclass_c, "CEGUI::DefaultLogger"));
  (delete thisclass_cpp);
};

// Add an event to the log. 
extern "C" CEGUI_LIB_EXPORT void cegui_dfltlgr_logEvent(struct hg3dclass_struct * thisclass_c, char * message_c, enum EnumLoggingLevel level_c)
{
  CEGUI::DefaultLogger * thisclass_cpp = static_cast<CEGUI::DefaultLogger*> (getHG3DClassPtr(*thisclass_c, "CEGUI::DefaultLogger"));
  CEGUI::String message_cpp = CEGUI::String((const char*) message_c);
  enum CEGUI::LoggingLevel level_cpp = (enum CEGUI::LoggingLevel)level_c;
  (thisclass_cpp->logEvent(message_cpp, level_cpp));
};

// Set the name of the log file where all subsequent log entries should be written. 
extern "C" CEGUI_LIB_EXPORT void cegui_dfltlgr_setLogFilename(struct hg3dclass_struct * thisclass_c, char * filename_c, int append_c)
{
  CEGUI::DefaultLogger * thisclass_cpp = static_cast<CEGUI::DefaultLogger*> (getHG3DClassPtr(*thisclass_c, "CEGUI::DefaultLogger"));
  CEGUI::String filename_cpp = CEGUI::String((const char*) filename_c);
  bool append_cpp = (bool)append_c;
  (thisclass_cpp->setLogFilename(filename_cpp, append_cpp));
};

