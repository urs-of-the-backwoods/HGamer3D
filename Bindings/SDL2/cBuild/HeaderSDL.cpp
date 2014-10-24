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

// HeaderSDL.cpp

// 

#include <wchar.h>
#include <string>
#include <iostream>

#include <iostream>
	#include <typeinfo>
	#include <stdio.h>
	#include <cstring>
	#include <exception>
	#include "SDL2DllDefines.h"
	#include "ClassPtr.h"
	#include "./SDL.h"
#include "HG3DUtilities.h"




// 
extern "C" SDL2_LIB_EXPORT void fsdl_sdl_Init(unsigned int flags_c, int * result_c)
{
  Uint32 flags_cpp = (Uint32)flags_c;
  int result_cpp;
  result_cpp = (SDL_Init(flags_cpp));
  *result_c = (int)result_cpp;
};

// 
extern "C" SDL2_LIB_EXPORT void fsdl_sdl_InitSubSystem(unsigned int flags_c, int * result_c)
{
  Uint32 flags_cpp = (Uint32)flags_c;
  int result_cpp;
  result_cpp = (SDL_InitSubSystem(flags_cpp));
  *result_c = (int)result_cpp;
};

// 
extern "C" SDL2_LIB_EXPORT void fsdl_sdl_QuitSubSystem(unsigned int flags_c)
{
  Uint32 flags_cpp = (Uint32)flags_c;
  (SDL_QuitSubSystem(flags_cpp));
};

// 
extern "C" SDL2_LIB_EXPORT void fsdl_sdl_WasInit(unsigned int flags_c, unsigned int * result_c)
{
  Uint32 flags_cpp = (Uint32)flags_c;
  Uint32 result_cpp;
  result_cpp = (SDL_WasInit(flags_cpp));
  *result_c = (unsigned int)result_cpp;
};

// 
extern "C" SDL2_LIB_EXPORT void fsdl_sdl_Quit()
{
  (SDL_Quit());
};

