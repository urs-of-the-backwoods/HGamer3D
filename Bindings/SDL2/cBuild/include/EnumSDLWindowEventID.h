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

// EnumSDLWindowEventID.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_EnumSDLWindowEventID
#define _DEFINED_HG3D_EnumSDLWindowEventID


enum EnumSDLWindowEventID
{
  SDL_WINDOWEVENT_NONE, // 
  SDL_WINDOWEVENT_SHOWN, // 
  SDL_WINDOWEVENT_HIDDEN, // 
  SDL_WINDOWEVENT_EXPOSED, // 
  SDL_WINDOWEVENT_MOVED, // 
  SDL_WINDOWEVENT_RESIZED, // 
  SDL_WINDOWEVENT_SIZE_CHANGED, // 
  SDL_WINDOWEVENT_MINIMIZED, // 
  SDL_WINDOWEVENT_MAXIMIZED, // 
  SDL_WINDOWEVENT_RESTORED, // 
  SDL_WINDOWEVENT_ENTER, // 
  SDL_WINDOWEVENT_LEAVE, // 
  SDL_WINDOWEVENT_FOCUS_GAINED, // 
  SDL_WINDOWEVENT_FOCUS_LOST, // 
  SDL_WINDOWEVENT_CLOSE // 
};
#endif 
