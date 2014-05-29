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

// ClassLogManager.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassLogManager
#define _DEFINED_HG3D_ClassLogManager

#include "ClassPtr.h"
#include "ClassLog.h"
#include "EnumLogMessageLevel.h"
#include "EnumLoggingLevel.h"


// 
void ogre_lmgr_destruct(struct hg3dclass_struct * thisclass_c);

// 
void ogre_lmgr_createLog(struct hg3dclass_struct * thisclass_c, char * name_c, long defaultLog_c, long debuggerOutput_c, long suppressFileOutput_c, struct hg3dclass_struct * result_c);

// 
void ogre_lmgr_getLog(struct hg3dclass_struct * thisclass_c, char * name_c, struct hg3dclass_struct * result_c);

// 
void ogre_lmgr_getDefaultLog(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// 
void ogre_lmgr_destroyLog(struct hg3dclass_struct * thisclass_c, char * name_c);

// 
void ogre_lmgr_destroyLog2(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * log_c);

// 
void ogre_lmgr_setDefaultLog(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * newLog_c, struct hg3dclass_struct * result_c);

// 
void ogre_lmgr_logMessage(struct hg3dclass_struct * thisclass_c, char * message_c, enum EnumLogMessageLevel lml_c, long maskDebug_c);

// 
void ogre_lmgr_logMessage2(struct hg3dclass_struct * thisclass_c, enum EnumLogMessageLevel lml_c, char * message_c, long maskDebug_c);

// 
void ogre_lmgr_setLogDetail(struct hg3dclass_struct * thisclass_c, enum EnumLoggingLevel ll_c);

// 
void ogre_lmgr_getSingleton(struct hg3dclass_struct * result_c);

// 
void ogre_lmgr_getSingletonPtr(struct hg3dclass_struct * result_c);

#endif 
