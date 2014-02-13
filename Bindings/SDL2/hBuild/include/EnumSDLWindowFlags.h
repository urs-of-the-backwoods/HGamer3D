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

// EnumSDLWindowFlags.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_EnumSDLWindowFlags
#define _DEFINED_HG3D_EnumSDLWindowFlags


enum EnumSDLWindowFlags
{
  SDL_WINDOW_FULLSCREEN =  0x00000001, // 
  SDL_WINDOW_OPENGL =  0x00000002, // 
  SDL_WINDOW_SHOWN =  0x00000004, // 
  SDL_WINDOW_HIDDEN =  0x00000008, // 
  SDL_WINDOW_BORDERLESS =  0x00000010, // 
  SDL_WINDOW_RESIZABLE =  0x00000020, // 
  SDL_WINDOW_MINIMIZED =  0x00000040, // 
  SDL_WINDOW_MAXIMIZED =  0x00000080, // 
  SDL_WINDOW_INPUT_GRABBED =  0x00000100, // 
  SDL_WINDOW_INPUT_FOCUS =  0x00000200, // 
  SDL_WINDOW_MOUSE_FOCUS =  0x00000400, // 
  SDL_WINDOW_FULLSCREEN_DESKTOP =  ( SDL_WINDOW_FULLSCREEN | 0x00001000 ), // 
  SDL_WINDOW_FOREIGN =  0x00000800, // 
  SDL_WINDOW_ALLOW_HIGHDPI =  0x00002000 // 
};
#endif 
