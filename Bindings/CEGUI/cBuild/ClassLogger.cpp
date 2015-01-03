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

// ClassLogger.cpp

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
#include "HG3DCommandHandler.h"
#include "HG3DEventController.h"
#include "HG3DEventModule.h"
#include "HG3DEventStaticFunctions.h"
#include "HG3DListboxStaticFunctions.h"
#include "HG3DWindowStaticFunctions.h"

using namespace CEGUI;



// Destructor for Logger
extern "C" CEGUI_LIB_EXPORT void cegui_lggr_destruct(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::Logger * thisclass_cpp = static_cast<CEGUI::Logger*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Logger"));
  (delete thisclass_cpp);
};

// Set the level of logging information that will get out to the log file. 
extern "C" CEGUI_LIB_EXPORT void cegui_lggr_setLoggingLevel(struct hg3dclass_struct * thisclass_c, enum EnumLoggingLevel level_c)
{
  CEGUI::Logger * thisclass_cpp = static_cast<CEGUI::Logger*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Logger"));
  enum CEGUI::LoggingLevel level_cpp = (enum CEGUI::LoggingLevel)level_c;
  (thisclass_cpp->setLoggingLevel(level_cpp));
};

// return the current logging level setting 
extern "C" CEGUI_LIB_EXPORT void cegui_lggr_getLoggingLevel(struct hg3dclass_struct * thisclass_c, enum EnumLoggingLevel * result_c)
{
  CEGUI::Logger * thisclass_cpp = static_cast<CEGUI::Logger*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Logger"));
  enum CEGUI::LoggingLevel result_cpp;
  result_cpp = (thisclass_cpp->getLoggingLevel());
  *result_c = (enum EnumLoggingLevel) result_cpp;
};

// Add an event to the log. 
extern "C" CEGUI_LIB_EXPORT void cegui_lggr_logEvent(struct hg3dclass_struct * thisclass_c, char * message_c, enum EnumLoggingLevel level_c)
{
  CEGUI::Logger * thisclass_cpp = static_cast<CEGUI::Logger*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Logger"));
  CEGUI::String message_cpp = CEGUI::String((const char*) message_c);
  enum CEGUI::LoggingLevel level_cpp = (enum CEGUI::LoggingLevel)level_c;
  (thisclass_cpp->logEvent(message_cpp, level_cpp));
};

// Set the name of the log file where all subsequent log entries should be written. The interpretation of file name may differ depending on the concrete logger implementation. 
extern "C" CEGUI_LIB_EXPORT void cegui_lggr_setLogFilename(struct hg3dclass_struct * thisclass_c, char * filename_c, int append_c)
{
  CEGUI::Logger * thisclass_cpp = static_cast<CEGUI::Logger*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Logger"));
  CEGUI::String filename_cpp = CEGUI::String((const char*) filename_c);
  bool append_cpp = (bool)append_c;
  (thisclass_cpp->setLogFilename(filename_cpp, append_cpp));
};

