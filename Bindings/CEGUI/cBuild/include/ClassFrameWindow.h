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

// ClassFrameWindow.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassFrameWindow
#define _DEFINED_HG3D_ClassFrameWindow

#include "ClassPtr.h"
#include "ClassPushButton.h"


// Initialises the Window
void cegui_frmwndw_initialiseComponents(struct hg3dclass_struct * thisclass_c);

// Return whether this window is sizable. Note that this requires that the window have an enabled frame and that sizing itself is enabled. 
void cegui_frmwndw_isSizingEnabled(struct hg3dclass_struct * thisclass_c, int * result_c);

// Return whether the frame for this window is enabled. 
void cegui_frmwndw_isFrameEnabled(struct hg3dclass_struct * thisclass_c, int * result_c);

// Return whether the title bar for this window is enabled. 
void cegui_frmwndw_isTitleBarEnabled(struct hg3dclass_struct * thisclass_c, int * result_c);

// Return whether this close button for this window is enabled. 
void cegui_frmwndw_isCloseButtonEnabled(struct hg3dclass_struct * thisclass_c, int * result_c);

// Return whether roll up (a.k.a shading) is enabled for this window. 
void cegui_frmwndw_isRollupEnabled(struct hg3dclass_struct * thisclass_c, int * result_c);

// Return whether the window is currently rolled up (a.k.a shaded). 
void cegui_frmwndw_isRolledup(struct hg3dclass_struct * thisclass_c, int * result_c);

// Return the thickness of the sizing border. 
void cegui_frmwndw_getSizingBorderThickness(struct hg3dclass_struct * thisclass_c, float * result_c);

// Enables or disables sizing for this window. 
void cegui_frmwndw_setSizingEnabled(struct hg3dclass_struct * thisclass_c, int setting_c);

// Enables or disables the frame for this window. 
void cegui_frmwndw_setFrameEnabled(struct hg3dclass_struct * thisclass_c, int setting_c);

// Enables or disables the title bar for the frame window. 
void cegui_frmwndw_setTitleBarEnabled(struct hg3dclass_struct * thisclass_c, int setting_c);

// Enables or disables the close button for the frame window. 
void cegui_frmwndw_setCloseButtonEnabled(struct hg3dclass_struct * thisclass_c, int setting_c);

// Enables or disables roll-up (shading) for this window. 
void cegui_frmwndw_setRollupEnabled(struct hg3dclass_struct * thisclass_c, int setting_c);

// Toggles the state of the window between rolled-up (shaded) and normal sizes. This requires roll-up to be enabled. 
void cegui_frmwndw_toggleRollup(struct hg3dclass_struct * thisclass_c);

// Set the size of the sizing border for this window. 
void cegui_frmwndw_setSizingBorderThickness(struct hg3dclass_struct * thisclass_c, float pixels_c);

// Return whether this FrameWindow
void cegui_frmwndw_isDragMovingEnabled(struct hg3dclass_struct * thisclass_c, int * result_c);

// Set whether this FrameWindow
void cegui_frmwndw_setDragMovingEnabled(struct hg3dclass_struct * thisclass_c, int setting_c);

// Set the image to be used for the north-south sizing mouse cursor. 
void cegui_frmwndw_setNSSizingCursorImage2(struct hg3dclass_struct * thisclass_c, char * imageset_c, char * image_c);

// Set the image to be used for the east-west sizing mouse cursor. 
void cegui_frmwndw_setEWSizingCursorImage2(struct hg3dclass_struct * thisclass_c, char * imageset_c, char * image_c);

// Set the image to be used for the northwest-southeast sizing mouse cursor. 
void cegui_frmwndw_setNWSESizingCursorImage2(struct hg3dclass_struct * thisclass_c, char * imageset_c, char * image_c);

// Set the image to be used for the northeast-southwest sizing mouse cursor. 
void cegui_frmwndw_setNESWSizingCursorImage2(struct hg3dclass_struct * thisclass_c, char * imageset_c, char * image_c);

// Return a pointer to the close button component widget for this FrameWindow
void cegui_frmwndw_getCloseButton(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// Constructor for FrameWindow
void cegui_frmwndw_construct(char * type_c, char * name_c, struct hg3dclass_struct * result_c);

// Destructor for FramwWindow objects. 
void cegui_frmwndw_destruct(struct hg3dclass_struct * thisclass_c);

#endif 
