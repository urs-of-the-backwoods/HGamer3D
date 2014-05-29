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

// ClassLog.cpp

// 

#include <wchar.h>
#include <string>
#include <iostream>

#include <iostream>
	#include <typeinfo>
	#include <stdio.h>
	#include <cstring>
	#include <exception>
	#include "OgreDllDefines.h"
	#include "ClassPtr.h"
	#include "EnumLogMessageLevel.h"
#include "EnumLoggingLevel.h"
#include "./Ogre.h"
#include "./OgreString.h"
#include "./WindowUtilsHG3D.h"
#include "HG3DUtilities.h"

using namespace Ogre;
using Ogre::uint;



// 
extern "C" Ogre_LIB_EXPORT void ogre_lg_destruct(struct hg3dclass_struct * thisclass_c)
{
  Ogre::Log * thisclass_cpp = static_cast<Ogre::Log*> (getHG3DClassPtr(*thisclass_c, "Ogre::Log"));
  (delete thisclass_cpp);
};

// Return the name of the log. 
extern "C" Ogre_LIB_EXPORT void ogre_lg_getName(struct hg3dclass_struct * thisclass_c, char * result_c)
{
  Ogre::Log * thisclass_cpp = static_cast<Ogre::Log*> (getHG3DClassPtr(*thisclass_c, "Ogre::Log"));
  Ogre::String result_cpp;
  result_cpp = (thisclass_cpp->getName());
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

// Get whether debug output is enabled for this log. 
extern "C" Ogre_LIB_EXPORT void ogre_lg_isDebugOutputEnabled(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::Log * thisclass_cpp = static_cast<Ogre::Log*> (getHG3DClassPtr(*thisclass_c, "Ogre::Log"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isDebugOutputEnabled());
  *result_c = (long)result_cpp;
};

// Get whether file output is suppressed for this log. 
extern "C" Ogre_LIB_EXPORT void ogre_lg_isFileOutputSuppressed(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::Log * thisclass_cpp = static_cast<Ogre::Log*> (getHG3DClassPtr(*thisclass_c, "Ogre::Log"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isFileOutputSuppressed());
  *result_c = (long)result_cpp;
};

// Get whether time stamps are printed for this log. 
extern "C" Ogre_LIB_EXPORT void ogre_lg_isTimeStampEnabled(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::Log * thisclass_cpp = static_cast<Ogre::Log*> (getHG3DClassPtr(*thisclass_c, "Ogre::Log"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isTimeStampEnabled());
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_lg_logMessage(struct hg3dclass_struct * thisclass_c, char * message_c, enum EnumLogMessageLevel lml_c, long maskDebug_c)
{
  Ogre::Log * thisclass_cpp = static_cast<Ogre::Log*> (getHG3DClassPtr(*thisclass_c, "Ogre::Log"));
  Ogre::String message_cpp = Ogre::String((const char*) message_c);
  enum Ogre::LogMessageLevel lml_cpp = (enum Ogre::LogMessageLevel)lml_c;
  bool maskDebug_cpp = (bool)maskDebug_c;
  (thisclass_cpp->logMessage(message_cpp, lml_cpp, maskDebug_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_lg_setDebugOutputEnabled(struct hg3dclass_struct * thisclass_c, long debugOutput_c)
{
  Ogre::Log * thisclass_cpp = static_cast<Ogre::Log*> (getHG3DClassPtr(*thisclass_c, "Ogre::Log"));
  bool debugOutput_cpp = (bool)debugOutput_c;
  (thisclass_cpp->setDebugOutputEnabled(debugOutput_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_lg_setLogDetail(struct hg3dclass_struct * thisclass_c, enum EnumLoggingLevel ll_c)
{
  Ogre::Log * thisclass_cpp = static_cast<Ogre::Log*> (getHG3DClassPtr(*thisclass_c, "Ogre::Log"));
  enum Ogre::LoggingLevel ll_cpp = (enum Ogre::LoggingLevel)ll_c;
  (thisclass_cpp->setLogDetail(ll_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_lg_setTimeStampEnabled(struct hg3dclass_struct * thisclass_c, long timeStamp_c)
{
  Ogre::Log * thisclass_cpp = static_cast<Ogre::Log*> (getHG3DClassPtr(*thisclass_c, "Ogre::Log"));
  bool timeStamp_cpp = (bool)timeStamp_c;
  (thisclass_cpp->setTimeStampEnabled(timeStamp_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_lg_getLogDetail(struct hg3dclass_struct * thisclass_c, enum EnumLoggingLevel * result_c)
{
  Ogre::Log * thisclass_cpp = static_cast<Ogre::Log*> (getHG3DClassPtr(*thisclass_c, "Ogre::Log"));
  enum Ogre::LoggingLevel result_cpp;
  result_cpp = (thisclass_cpp->getLogDetail());
  *result_c = (enum EnumLoggingLevel) result_cpp;
};

