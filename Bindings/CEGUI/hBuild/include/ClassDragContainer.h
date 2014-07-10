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

// ClassDragContainer.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassDragContainer
#define _DEFINED_HG3D_ClassDragContainer

#include "ClassPtr.h"
#include "ClassWindow.h"
#include "ClassUVector2.h"


// Constructor for DragContainer
void cegui_drgcnt_construct(char * type_c, char * name_c, struct hg3dclass_struct * result_c);

// Destructor for DragContainer
void cegui_drgcnt_destruct(struct hg3dclass_struct * thisclass_c);

// Return whether dragging is currently enabled for this DragContainer
void cegui_drgcnt_isDraggingEnabled(struct hg3dclass_struct * thisclass_c, int * result_c);

// Set whether dragging is currently enabled for this DragContainer
void cegui_drgcnt_setDraggingEnabled(struct hg3dclass_struct * thisclass_c, int setting_c);

// Return whether the DragContainer
void cegui_drgcnt_isBeingDragged(struct hg3dclass_struct * thisclass_c, int * result_c);

// Return the current drag threshold in pixels. 
void cegui_drgcnt_getPixelDragThreshold(struct hg3dclass_struct * thisclass_c, float * result_c);

// Set the current drag threshold in pixels. 
void cegui_drgcnt_setPixelDragThreshold(struct hg3dclass_struct * thisclass_c, float pixels_c);

// Return the alpha value that will be set on the DragContainer
void cegui_drgcnt_getDragAlpha(struct hg3dclass_struct * thisclass_c, float * result_c);

// Set the alpha value to be set on the DragContainer
void cegui_drgcnt_setDragAlpha(struct hg3dclass_struct * thisclass_c, float alpha_c);

// Set the Image to be used for the mouse cursor when a drag operation is in progress. 
void cegui_drgcnt_setDragCursorImage3(struct hg3dclass_struct * thisclass_c, char * imageset_c, char * image_c);

// Return the WindowDragContainer
void cegui_drgcnt_getCurrentDropTarget(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// Return whether sticky mode is enable or disabled. 
void cegui_drgcnt_isStickyModeEnabled(struct hg3dclass_struct * thisclass_c, int * result_c);

// Enable or disable sticky mode. 
void cegui_drgcnt_setStickyModeEnabled(struct hg3dclass_struct * thisclass_c, int setting_c);

// Immediately pick up the DragContainer
void cegui_drgcnt_pickUp(struct hg3dclass_struct * thisclass_c, const int force_sticky_c, int * result_c);

// Set the fixed mouse cursor dragging offset to be used for this DragContainer
void cegui_drgcnt_setFixedDragOffset(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * offset_c);

// Return the fixed mouse cursor dragging offset to be used for this DragContainer
void cegui_drgcnt_getFixedDragOffset(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// Set whether the fixed dragging offset - as set with the setFixedDragOffset - function will be used, or whether the built-in positioning will be used. 
void cegui_drgcnt_setUsingFixedDragOffset(struct hg3dclass_struct * thisclass_c, const int enable_c);

// Return whether the fixed dragging offset - as set with the setFixedDragOffset function - will be used, or whether the built-in positioning will be used. 
void cegui_drgcnt_isUsingFixedDragOffset(struct hg3dclass_struct * thisclass_c, int * result_c);

#endif 
