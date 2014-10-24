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

// EnumSDLEventType.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_EnumSDLEventType
#define _DEFINED_HG3D_EnumSDLEventType


enum EnumSDLEventType
{
  SDL_FIRSTEVENT =  0, // 
  SDL_QUIT =  0x100, // 
  SDL_APP_TERMINATING, // 
  SDL_APP_LOWMEMORY, // 
  SDL_APP_WILLENTERBACKGROUND, // 
  SDL_APP_DIDENTERBACKGROUND, // 
  SDL_APP_WILLENTERFOREGROUND, // 
  SDL_APP_DIDENTERFOREGROUND, // 
  SDL_WINDOWEVENT =  0x200, // 
  SDL_SYSWMEVENT, // 
  SDL_KEYDOWN =  0x300, // 
  SDL_KEYUP, // 
  SDL_TEXTEDITING, // 
  SDL_TEXTINPUT, // 
  SDL_MOUSEMOTION =  0x400, // 
  SDL_MOUSEBUTTONDOWN, // 
  SDL_MOUSEBUTTONUP, // 
  SDL_MOUSEWHEEL, // 
  SDL_JOYAXISMOTION =  0x600, // 
  SDL_JOYBALLMOTION, // 
  SDL_JOYHATMOTION, // 
  SDL_JOYBUTTONDOWN, // 
  SDL_JOYBUTTONUP, // 
  SDL_JOYDEVICEADDED, // 
  SDL_JOYDEVICEREMOVED, // 
  SDL_CONTROLLERAXISMOTION =  0x650, // 
  SDL_CONTROLLERBUTTONDOWN, // 
  SDL_CONTROLLERBUTTONUP, // 
  SDL_CONTROLLERDEVICEADDED, // 
  SDL_CONTROLLERDEVICEREMOVED, // 
  SDL_CONTROLLERDEVICEREMAPPED, // 
  SDL_FINGERDOWN =  0x700, // 
  SDL_FINGERUP, // 
  SDL_FINGERMOTION, // 
  SDL_DOLLARGESTURE =  0x800, // 
  SDL_DOLLARRECORD, // 
  SDL_MULTIGESTURE, // 
  SDL_CLIPBOARDUPDATE =  0x900, // 
  SDL_DROPFILE =  0x1000, // 
  SDL_USEREVENT =  0x8000, // 
  SDL_LASTEVENT =  0xFFFF // 
};
#endif 
