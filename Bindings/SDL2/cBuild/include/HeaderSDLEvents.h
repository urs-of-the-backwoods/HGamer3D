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

// HeaderSDLEvents.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_HeaderSDLEvents
#define _DEFINED_HG3D_HeaderSDLEvents

#include "ClassPtr.h"
#include "StructSDLEvent.h"


// 
void sdlevts_sdl_PumpEvents();

// 
void sdlevts_sdl_FlushEvent(unsigned int type_c);

// 
void sdlevts_sdl_FlushEvents(unsigned int minType_c, unsigned int maxType_c);

// Polls for currently pending events. 
void sdlevts_sdl_PollEvent(struct sdlevent_struct * event_c, int * result_c);

// Waits indefinitely for the next available event. 
void sdlevts_sdl_WaitEvent(struct sdlevent_struct * event_c, int * result_c);

// Waits until the specified timeout (in milliseconds) for the next available event. 
void sdlevts_sdl_WaitEventTimeout(struct sdlevent_struct * event_c, int timeout_c, int * result_c);

// Add an event to the event queue. 
void sdlevts_sdl_PushEvent(struct sdlevent_struct * event_c, int * result_c);

// 
void sdlevts_sdl_RegisterEvents(int numevents_c, unsigned int * result_c);

#endif 
