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

// ClassLogManager.cpp

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
extern "C" Ogre_LIB_EXPORT void ogre_lmgr_destruct(struct hg3dclass_struct * thisclass_c)
{
  Ogre::LogManager * thisclass_cpp = static_cast<Ogre::LogManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::LogManager"));
  (delete thisclass_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_lmgr_createLog(struct hg3dclass_struct * thisclass_c, char * name_c, long defaultLog_c, long debuggerOutput_c, long suppressFileOutput_c, struct hg3dclass_struct * result_c)
{
  Ogre::LogManager * thisclass_cpp = static_cast<Ogre::LogManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::LogManager"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  bool defaultLog_cpp = (bool)defaultLog_c;
  bool debuggerOutput_cpp = (bool)debuggerOutput_c;
  bool suppressFileOutput_cpp = (bool)suppressFileOutput_c;
  Ogre::Log * result_cpp;
  result_cpp = (thisclass_cpp->createLog(name_cpp, defaultLog_cpp, debuggerOutput_cpp, suppressFileOutput_cpp));
  *result_c = getHG3DClass_Log((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_lmgr_getLog(struct hg3dclass_struct * thisclass_c, char * name_c, struct hg3dclass_struct * result_c)
{
  Ogre::LogManager * thisclass_cpp = static_cast<Ogre::LogManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::LogManager"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  Ogre::Log * result_cpp;
  result_cpp = (thisclass_cpp->getLog(name_cpp));
  *result_c = getHG3DClass_Log((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_lmgr_getDefaultLog(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  Ogre::LogManager * thisclass_cpp = static_cast<Ogre::LogManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::LogManager"));
  Ogre::Log * result_cpp;
  result_cpp = (thisclass_cpp->getDefaultLog());
  *result_c = getHG3DClass_Log((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_lmgr_destroyLog(struct hg3dclass_struct * thisclass_c, char * name_c)
{
  Ogre::LogManager * thisclass_cpp = static_cast<Ogre::LogManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::LogManager"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  (thisclass_cpp->destroyLog(name_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_lmgr_destroyLog2(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * log_c)
{
  Ogre::LogManager * thisclass_cpp = static_cast<Ogre::LogManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::LogManager"));
  Ogre::Log * log_cpp = static_cast<Ogre::Log*> (getHG3DClassPtr(*log_c, "Ogre::Log"));
  (thisclass_cpp->destroyLog(log_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_lmgr_setDefaultLog(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * newLog_c, struct hg3dclass_struct * result_c)
{
  Ogre::LogManager * thisclass_cpp = static_cast<Ogre::LogManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::LogManager"));
  Ogre::Log * newLog_cpp = static_cast<Ogre::Log*> (getHG3DClassPtr(*newLog_c, "Ogre::Log"));
  Ogre::Log * result_cpp;
  result_cpp = (thisclass_cpp->setDefaultLog(newLog_cpp));
  *result_c = getHG3DClass_Log((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_lmgr_logMessage(struct hg3dclass_struct * thisclass_c, char * message_c, enum EnumLogMessageLevel lml_c, long maskDebug_c)
{
  Ogre::LogManager * thisclass_cpp = static_cast<Ogre::LogManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::LogManager"));
  Ogre::String message_cpp = Ogre::String((const char*) message_c);
  enum Ogre::LogMessageLevel lml_cpp = (enum Ogre::LogMessageLevel)lml_c;
  bool maskDebug_cpp = (bool)maskDebug_c;
  (thisclass_cpp->logMessage(message_cpp, lml_cpp, maskDebug_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_lmgr_logMessage2(struct hg3dclass_struct * thisclass_c, enum EnumLogMessageLevel lml_c, char * message_c, long maskDebug_c)
{
  Ogre::LogManager * thisclass_cpp = static_cast<Ogre::LogManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::LogManager"));
  enum Ogre::LogMessageLevel lml_cpp = (enum Ogre::LogMessageLevel)lml_c;
  Ogre::String message_cpp = Ogre::String((const char*) message_c);
  bool maskDebug_cpp = (bool)maskDebug_c;
  (thisclass_cpp->logMessage(lml_cpp, message_cpp, maskDebug_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_lmgr_setLogDetail(struct hg3dclass_struct * thisclass_c, enum EnumLoggingLevel ll_c)
{
  Ogre::LogManager * thisclass_cpp = static_cast<Ogre::LogManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::LogManager"));
  enum Ogre::LoggingLevel ll_cpp = (enum Ogre::LoggingLevel)ll_c;
  (thisclass_cpp->setLogDetail(ll_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_lmgr_getSingleton(struct hg3dclass_struct * result_c)
{
  Ogre::LogManager * result_cpp;
  result_cpp = &(Ogre::LogManager::getSingleton());
  *result_c = getHG3DClass_LogManager((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_lmgr_getSingletonPtr(struct hg3dclass_struct * result_c)
{
  Ogre::LogManager * result_cpp;
  result_cpp = (Ogre::LogManager::getSingletonPtr());
  *result_c = getHG3DClass_LogManager((void *) result_cpp);
;
};

