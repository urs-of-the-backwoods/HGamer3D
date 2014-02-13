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

// EnumLoggingLevel.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_EnumLoggingLevel
#define _DEFINED_HG3D_EnumLoggingLevel


enum EnumLoggingLevel
{
  LoggingLevelErrors, // Only actual error conditions will be logged. 
  LoggingLevelWarnings, // Warnings will be logged as well. 
  LoggingLevelStandard, // Basic events will be logged (default level). 
  LoggingLevelInformative, // Useful tracing (object creations etc) information will be logged. 
  LoggingLevelInsane // Mostly everything gets logged (use for heavy tracing only, log WILL be big). 
};
#endif 
