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

// EnumSystemKey.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_EnumSystemKey
#define _DEFINED_HG3D_EnumSystemKey


enum EnumSystemKey
{
  SystemKeyLeftMouse =  0x0001, // The left mouse button. 
  SystemKeyRightMouse =  0x0002, // The right mouse button. 
  SystemKeyShift =  0x0004, // Either shift key. 
  SystemKeyControl =  0x0008, // Either control key. 
  SystemKeyMiddleMouse =  0x0010, // The middle mouse button. 
  SystemKeyX1Mouse =  0x0020, // The first 'extra' mouse button. 
  SystemKeyX2Mouse =  0x0040, // The second 'extra' mouse button. 
  SystemKeyAlt =  0x0080 // Either alt key. 
};
#endif 
