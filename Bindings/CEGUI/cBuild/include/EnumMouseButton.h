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

// EnumMouseButton.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_EnumMouseButton
#define _DEFINED_HG3D_EnumMouseButton


enum EnumMouseButton
{
  MouseLeftButton, // The left mouse button. 
  MouseRightButton, // The right mouse button. 
  MouseMiddleButton, // The middle mouse button. 
  MouseX1Button, // The first 'extra' mouse button. 
  MouseX2Button, // The second 'extra' mouse button. 
  MouseMouseButtonCount, // Value that equals the number of mouse buttons supported by CEGUI
  MouseNoButton // Value set for no mouse button. NB: This is not 0, do not assume! 
};
#endif 
