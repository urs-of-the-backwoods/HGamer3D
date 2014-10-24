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

// ClassMenuBase.h

// abstrakte Klasse!

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassMenuBase
#define _DEFINED_HG3D_ClassMenuBase

#include "ClassPtr.h"
#include "ClassMenuItem.h"


// Get the item spacing for this menu. 
void cegui_mnbs_getItemSpacing(struct hg3dclass_struct * thisclass_c, float * result_c);

// Return whether this menu allows multiple popup menus to open at the same time. 
void cegui_mnbs_isMultiplePopupsAllowed(struct hg3dclass_struct * thisclass_c, int * result_c);

// Return whether this menu should close all its open child popups, when it gets hidden. 
void cegui_mnbs_getAutoCloseNestedPopups(struct hg3dclass_struct * thisclass_c, int * result_c);

// Get currently opened MenuItem
void cegui_mnbs_getPopupMenuItem(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// Set the item spacing for this menu. 
void cegui_mnbs_setItemSpacing(struct hg3dclass_struct * thisclass_c, float spacing_c);

// Change the currently open MenuItem
void cegui_mnbs_changePopupMenuItem(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * item_c);

// Set whether this menu allows multiple popup menus to be opened simultaneously. 
void cegui_mnbs_setAllowMultiplePopups(struct hg3dclass_struct * thisclass_c, int setting_c);

// Set whether the menu should close all its open child popups, when it gets hidden. 
void cegui_mnbs_setAutoCloseNestedPopups(struct hg3dclass_struct * thisclass_c, int setting_c);

// tells the current popup that it should start its closing timer. 
void cegui_mnbs_setPopupMenuItemClosing(struct hg3dclass_struct * thisclass_c);

#endif 
