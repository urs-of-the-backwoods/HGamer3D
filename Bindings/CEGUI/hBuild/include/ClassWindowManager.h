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

// ClassWindowManager.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassWindowManager
#define _DEFINED_HG3D_ClassWindowManager

#include "ClassPtr.h"
#include "ClassWindow.h"


// Constructs a new WindowManager
void cegui_wmgr_construct(struct hg3dclass_struct * result_c);

// Destructor for WindowManager
void cegui_wmgr_destruct(struct hg3dclass_struct * thisclass_c);

// Creates a new Window
void cegui_wmgr_createWindow(struct hg3dclass_struct * thisclass_c, char * type_c, char * name_c, struct hg3dclass_struct * result_c);

// Destroy the specified Window
void cegui_wmgr_destroyWindow(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * window_c);

// Destroy the specified Window
void cegui_wmgr_destroyWindow2(struct hg3dclass_struct * thisclass_c, char * window_c);

// Return a pointer to the specified Window
void cegui_wmgr_getWindow(struct hg3dclass_struct * thisclass_c, char * name_c, struct hg3dclass_struct * result_c);

// Examines the list of Window
void cegui_wmgr_isWindowPresent(struct hg3dclass_struct * thisclass_c, char * name_c, int * result_c);

// Destroys all Window
void cegui_wmgr_destroyAllWindows(struct hg3dclass_struct * thisclass_c);

// Return whether the window dead pool is empty. 
void cegui_wmgr_isDeadPoolEmpty(struct hg3dclass_struct * thisclass_c, int * result_c);

// Permanently destroys any windows placed in the dead pool. 
void cegui_wmgr_cleanDeadPool(struct hg3dclass_struct * thisclass_c);

// Save a full XML window layout, starting at the given Window
void cegui_wmgr_saveWindowLayout(struct hg3dclass_struct * thisclass_c, char * window_c, char * filename_c, const int writeParent_c);

// Save a full XML window layout, starting at the given Window
void cegui_wmgr_saveWindowLayout2(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * window_c, char * filename_c, const int writeParent_c);

// Rename a window. 
void cegui_wmgr_renameWindow(struct hg3dclass_struct * thisclass_c, char * window_c, char * new_name_c);

// Rename a window. 
void cegui_wmgr_renameWindow2(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * window_c, char * new_name_c);

// Put WindowManager
void cegui_wmgr_lock(struct hg3dclass_struct * thisclass_c);

// Put WindowManager
void cegui_wmgr_unlock(struct hg3dclass_struct * thisclass_c);

// Returns whether WindowManager
void cegui_wmgr_isLocked(struct hg3dclass_struct * thisclass_c, int * result_c);

// Returns the default resource group currently set for layouts. 
void cegui_wmgr_getDefaultResourceGroup(char * result_c);

// Sets the default resource group to be used when loading layouts. 
void cegui_wmgr_setDefaultResourceGroup(char * resourceGroup_c);

#endif 
