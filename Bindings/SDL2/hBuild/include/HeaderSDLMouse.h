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

// HeaderSDLMouse.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_HeaderSDLMouse
#define _DEFINED_HG3D_HeaderSDLMouse

#include "ClassPtr.h"


// Get the window which currently has mouse focus. 
void sdlms_sdl_GetMouseFocus(void * * result_c);

// Retrieve the current state of the mouse. 
void sdlms_sdl_GetMouseState(int * x_c, int * y_c, unsigned int * result_c);

// Retrieve the relative state of the mouse. 
void sdlms_sdl_GetRelativeMouseState(int * x_c, int * y_c, unsigned int * result_c);

// Moves the mouse to the given position within the window. 
void sdlms_sdl_WarpMouseInWindow(void * window_c, int x_c, int y_c);

// Toggle whether or not the cursor is shown. 
void sdlms_sdl_ShowCursor(int toggle_c, int * result_c);

#endif 
