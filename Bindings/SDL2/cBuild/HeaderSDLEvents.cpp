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

// HeaderSDLEvents.cpp

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
	#include "StructSDLEvent.h"
#include "./SDL.h"
#include "HG3DUtilities.h"




// 
extern "C" SDL2_LIB_EXPORT void sdlevts_sdl_PumpEvents()
{
  (SDL_PumpEvents());
};

// 
extern "C" SDL2_LIB_EXPORT void sdlevts_sdl_FlushEvent(unsigned int type_c)
{
  Uint32 type_cpp = (Uint32)type_c;
  (SDL_FlushEvent(type_cpp));
};

// 
extern "C" SDL2_LIB_EXPORT void sdlevts_sdl_FlushEvents(unsigned int minType_c, unsigned int maxType_c)
{
  Uint32 minType_cpp = (Uint32)minType_c;
  Uint32 maxType_cpp = (Uint32)maxType_c;
  (SDL_FlushEvents(minType_cpp, maxType_cpp));
};

// Polls for currently pending events. 
extern "C" SDL2_LIB_EXPORT void sdlevts_sdl_PollEvent(struct sdlevent_struct * event_c, int * result_c)
{
  SDL_Event event_cpp;
  int result_cpp;
  result_cpp = (SDL_PollEvent(&event_cpp));
  *event_c = *((struct sdlevent_struct*) &event_cpp);
  *result_c = (int)result_cpp;
};

// Waits indefinitely for the next available event. 
extern "C" SDL2_LIB_EXPORT void sdlevts_sdl_WaitEvent(struct sdlevent_struct * event_c, int * result_c)
{
  SDL_Event event_cpp;
  int result_cpp;
  result_cpp = (SDL_WaitEvent(&event_cpp));
  *event_c = *((struct sdlevent_struct*) &event_cpp);
  *result_c = (int)result_cpp;
};

// Waits until the specified timeout (in milliseconds) for the next available event. 
extern "C" SDL2_LIB_EXPORT void sdlevts_sdl_WaitEventTimeout(struct sdlevent_struct * event_c, int timeout_c, int * result_c)
{
  SDL_Event event_cpp;
  int timeout_cpp = (int)timeout_c;
  int result_cpp;
  result_cpp = (SDL_WaitEventTimeout(&event_cpp, timeout_cpp));
  *event_c = *((struct sdlevent_struct*) &event_cpp);
  *result_c = (int)result_cpp;
};

// Add an event to the event queue. 
extern "C" SDL2_LIB_EXPORT void sdlevts_sdl_PushEvent(struct sdlevent_struct * event_c, int * result_c)
{
  SDL_Event event_cpp;
  int result_cpp;
  result_cpp = (SDL_PushEvent(&event_cpp));
  *event_c = *((struct sdlevent_struct*) &event_cpp);
  *result_c = (int)result_cpp;
};

// 
extern "C" SDL2_LIB_EXPORT void sdlevts_sdl_RegisterEvents(int numevents_c, unsigned int * result_c)
{
  int numevents_cpp = (int)numevents_c;
  Uint32 result_cpp;
  result_cpp = (SDL_RegisterEvents(numevents_cpp));
  *result_c = (unsigned int)result_cpp;
};

