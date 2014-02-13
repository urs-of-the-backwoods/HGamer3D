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

// EnumSDLKeymod.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_EnumSDLKeymod
#define _DEFINED_HG3D_EnumSDLKeymod


enum EnumSDLKeymod
{
  KMOD_NONE =  0x0000, // 
  KMOD_LSHIFT =  0x0001, // 
  KMOD_RSHIFT =  0x0002, // 
  KMOD_LCTRL =  0x0040, // 
  KMOD_RCTRL =  0x0080, // 
  KMOD_LALT =  0x0100, // 
  KMOD_RALT =  0x0200, // 
  KMOD_LGUI =  0x0400, // 
  KMOD_RGUI =  0x0800, // 
  KMOD_NUM =  0x1000, // 
  KMOD_CAPS =  0x2000, // 
  KMOD_MODE =  0x4000, // 
  KMOD_RESERVED =  0x8000 // 
};
#endif 
