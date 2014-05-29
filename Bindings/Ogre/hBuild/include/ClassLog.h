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

// ClassLog.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassLog
#define _DEFINED_HG3D_ClassLog

#include "ClassPtr.h"
#include "EnumLogMessageLevel.h"
#include "EnumLoggingLevel.h"


// 
void ogre_lg_destruct(struct hg3dclass_struct * thisclass_c);

// Return the name of the log. 
void ogre_lg_getName(struct hg3dclass_struct * thisclass_c, char * result_c);

// Get whether debug output is enabled for this log. 
void ogre_lg_isDebugOutputEnabled(struct hg3dclass_struct * thisclass_c, long * result_c);

// Get whether file output is suppressed for this log. 
void ogre_lg_isFileOutputSuppressed(struct hg3dclass_struct * thisclass_c, long * result_c);

// Get whether time stamps are printed for this log. 
void ogre_lg_isTimeStampEnabled(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_lg_logMessage(struct hg3dclass_struct * thisclass_c, char * message_c, enum EnumLogMessageLevel lml_c, long maskDebug_c);

// 
void ogre_lg_setDebugOutputEnabled(struct hg3dclass_struct * thisclass_c, long debugOutput_c);

// 
void ogre_lg_setLogDetail(struct hg3dclass_struct * thisclass_c, enum EnumLoggingLevel ll_c);

// 
void ogre_lg_setTimeStampEnabled(struct hg3dclass_struct * thisclass_c, long timeStamp_c);

// 
void ogre_lg_getLogDetail(struct hg3dclass_struct * thisclass_c, enum EnumLoggingLevel * result_c);

#endif 
