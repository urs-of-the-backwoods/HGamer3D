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

// ClassMenuItem.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassMenuItem
#define _DEFINED_HG3D_ClassMenuItem

#include "ClassPtr.h"
#include "ClassUVector2.h"


// return true if user is hovering over this widget (or it's pushed and user is not over it for highlight) 
void cegui_mnitm_isHovering(struct hg3dclass_struct * thisclass_c, int * result_c);

// Return true if the button widget is in the pushed state. 
void cegui_mnitm_isPushed(struct hg3dclass_struct * thisclass_c, int * result_c);

// Returns true if the popup menu attached to the menu item is open. 
void cegui_mnitm_isOpened(struct hg3dclass_struct * thisclass_c, int * result_c);

// Returns true if the menu item popup is closing or not. 
void cegui_mnitm_isPopupClosing(struct hg3dclass_struct * thisclass_c, int * result_c);

// Returns true if the menu item popup is closed or opened automatically if hovering with the mouse. 
void cegui_mnitm_hasAutoPopup(struct hg3dclass_struct * thisclass_c, int * result_c);

// Returns the time, which has to elapse before the popup window is opened/closed if the hovering state changes. 
void cegui_mnitm_getAutoPopupTimeout(struct hg3dclass_struct * thisclass_c, float * result_c);

// Sets the time, which has to elapse before the popup window is opened/closed if the hovering state changes. 
void cegui_mnitm_setAutoPopupTimeout(struct hg3dclass_struct * thisclass_c, float time_c);

// Returns the current offset for popup placement. 
void cegui_mnitm_getPopupOffset(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// sets the current offset for popup placement. 
void cegui_mnitm_setPopupOffset(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * popupOffset_c);

// Opens the PopupMenu. 
void cegui_mnitm_openPopupMenu(struct hg3dclass_struct * thisclass_c, int notify_c);

// Closes the PopupMenu. 
void cegui_mnitm_closePopupMenu(struct hg3dclass_struct * thisclass_c, int notify_c);

// Toggles the PopupMenu. 
void cegui_mnitm_togglePopupMenu(struct hg3dclass_struct * thisclass_c, int * result_c);

// starts the closing timer for the popup, which will close it if the timer is enabled. 
void cegui_mnitm_startPopupClosing(struct hg3dclass_struct * thisclass_c);

// starts the opening timer for the popup, which will open it if the timer is enabled. 
void cegui_mnitm_startPopupOpening(struct hg3dclass_struct * thisclass_c);

// Constructor for MenuItem
void cegui_mnitm_construct(char * type_c, char * name_c, struct hg3dclass_struct * result_c);

// Destructor for MenuItem
void cegui_mnitm_destruct(struct hg3dclass_struct * thisclass_c);

#endif 
