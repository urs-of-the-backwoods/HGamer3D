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

// ClassEventSet.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassEventSet
#define _DEFINED_HG3D_ClassEventSet

#include "ClassPtr.h"
#include "ClassEventArgs.h"


// Constructor for EventSet
void cegui_evtst_construct(struct hg3dclass_struct * result_c);

// Destructor for EventSet
void cegui_evtst_destruct(struct hg3dclass_struct * thisclass_c);

// Add a new EventEventSet
void cegui_evtst_addEvent(struct hg3dclass_struct * thisclass_c, char * name_c);

// Removes the Event
void cegui_evtst_removeEvent(struct hg3dclass_struct * thisclass_c, char * name_c);

// Remove all EventEventSet
void cegui_evtst_removeAllEvents(struct hg3dclass_struct * thisclass_c);

// Checks to see if an EventEventSet
void cegui_evtst_isEventPresent(struct hg3dclass_struct * thisclass_c, char * name_c, int * result_c);

// Fires the named event passing the given EventArgs
void cegui_evtst_fireEvent(struct hg3dclass_struct * thisclass_c, char * name_c, struct hg3dclass_struct * args_c, char * eventNamespace_c);

// Return whether the EventSet
void cegui_evtst_isMuted(struct hg3dclass_struct * thisclass_c, int * result_c);

// Set the mute state for this EventSet
void cegui_evtst_setMutedState(struct hg3dclass_struct * thisclass_c, int setting_c);

#endif 
