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

// ClassTooltip.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassTooltip
#define _DEFINED_HG3D_ClassTooltip

#include "ClassPtr.h"
#include "ClassWindow.h"


// Constructor for the Tooltip
void cegui_tltp_construct(char * type_c, char * name_c, struct hg3dclass_struct * result_c);

// Destructor for the Tooltip
void cegui_tltp_destruct(struct hg3dclass_struct * thisclass_c);

// Sets the target window for the tooltip. This used internally to manage tooltips, you should not have to call this yourself. 
void cegui_tltp_setTargetWindow(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * wnd_c);

// return the current target window for this Tooltip
void cegui_tltp_getTargetWindow(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// Resets the timer on the tooltip when in the Active / Inactive states. This is used internally to control the tooltip, it is not normally necessary to call this method yourself. 
void cegui_tltp_resetTimer(struct hg3dclass_struct * thisclass_c);

// Return the number of seconds the mouse should hover stationary over the target window before the tooltip gets activated. 
void cegui_tltp_getHoverTime(struct hg3dclass_struct * thisclass_c, float * result_c);

// Set the number of seconds the tooltip should be displayed for before it automatically de-activates itself. 0 indicates that the tooltip should never timesout and auto-deactivate. 
void cegui_tltp_setDisplayTime(struct hg3dclass_struct * thisclass_c, float seconds_c);

// Return the number of seconds that should be taken to fade the tooltip into and out of visibility. 
void cegui_tltp_getFadeTime(struct hg3dclass_struct * thisclass_c, float * result_c);

// Set the number of seconds the mouse should hover stationary over the target window before the tooltip gets activated. 
void cegui_tltp_setHoverTime(struct hg3dclass_struct * thisclass_c, float seconds_c);

// Return the number of seconds the tooltip should be displayed for before it automatically de-activates itself. 0 indicates that the tooltip never timesout and auto-deactivates. 
void cegui_tltp_getDisplayTime(struct hg3dclass_struct * thisclass_c, float * result_c);

// Set the number of seconds that should be taken to fade the tooltip into and out of visibility. 
void cegui_tltp_setFadeTime(struct hg3dclass_struct * thisclass_c, float seconds_c);

// Causes the tooltip to position itself appropriately. 
void cegui_tltp_positionSelf(struct hg3dclass_struct * thisclass_c);

// Causes the tooltip to resize itself appropriately. 
void cegui_tltp_sizeSelf(struct hg3dclass_struct * thisclass_c);

#endif 
