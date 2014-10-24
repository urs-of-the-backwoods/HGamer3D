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

// ClassLogger.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassLogger
#define _DEFINED_HG3D_ClassLogger

#include "ClassPtr.h"
#include "EnumLoggingLevel.h"


// Destructor for Logger
void cegui_lggr_destruct(struct hg3dclass_struct * thisclass_c);

// Set the level of logging information that will get out to the log file. 
void cegui_lggr_setLoggingLevel(struct hg3dclass_struct * thisclass_c, enum EnumLoggingLevel level_c);

// return the current logging level setting 
void cegui_lggr_getLoggingLevel(struct hg3dclass_struct * thisclass_c, enum EnumLoggingLevel * result_c);

// Add an event to the log. 
void cegui_lggr_logEvent(struct hg3dclass_struct * thisclass_c, char * message_c, enum EnumLoggingLevel level_c);

// Set the name of the log file where all subsequent log entries should be written. The interpretation of file name may differ depending on the concrete logger implementation. 
void cegui_lggr_setLogFilename(struct hg3dclass_struct * thisclass_c, char * filename_c, int append_c);

#endif 
