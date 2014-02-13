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

// HeaderSDLMouse.cpp

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




// Get the window which currently has mouse focus. 
extern "C" SDL2_LIB_EXPORT void sdlms_sdl_GetMouseFocus(void * * result_c)
{
  SDL_Window * result_cpp;
  result_cpp = (SDL_GetMouseFocus());
  *result_c = (void *)result_cpp;
};

// Retrieve the current state of the mouse. 
extern "C" SDL2_LIB_EXPORT void sdlms_sdl_GetMouseState(int * x_c, int * y_c, unsigned int * result_c)
{
  int x_cpp;
  int y_cpp;
  Uint32 result_cpp;
  result_cpp = (SDL_GetMouseState(&x_cpp, &y_cpp));
  *x_c = (int)x_cpp;
  *y_c = (int)y_cpp;
  *result_c = (unsigned int)result_cpp;
};

// Retrieve the relative state of the mouse. 
extern "C" SDL2_LIB_EXPORT void sdlms_sdl_GetRelativeMouseState(int * x_c, int * y_c, unsigned int * result_c)
{
  int x_cpp;
  int y_cpp;
  Uint32 result_cpp;
  result_cpp = (SDL_GetRelativeMouseState(&x_cpp, &y_cpp));
  *x_c = (int)x_cpp;
  *y_c = (int)y_cpp;
  *result_c = (unsigned int)result_cpp;
};

// Moves the mouse to the given position within the window. 
extern "C" SDL2_LIB_EXPORT void sdlms_sdl_WarpMouseInWindow(void * window_c, int x_c, int y_c)
{
  SDL_Window * window_cpp = (SDL_Window *)(window_c);
  int x_cpp = (int)x_c;
  int y_cpp = (int)y_c;
  (SDL_WarpMouseInWindow(window_cpp, x_cpp, y_cpp));
};

// Toggle whether or not the cursor is shown. 
extern "C" SDL2_LIB_EXPORT void sdlms_sdl_ShowCursor(int toggle_c, int * result_c)
{
  int toggle_cpp = (int)toggle_c;
  int result_cpp;
  result_cpp = (SDL_ShowCursor(toggle_cpp));
  *result_c = (int)result_cpp;
};

