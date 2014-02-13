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

// ClassScrollbar.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassScrollbar
#define _DEFINED_HG3D_ClassScrollbar

#include "ClassPtr.h"
#include "ClassPushButton.h"
#include "ClassThumb.h"


// Return the size of the document or data. 
void cegui_scrlbr_getDocumentSize(struct hg3dclass_struct * thisclass_c, float * result_c);

// Return the page size for this scroll bar. 
void cegui_scrlbr_getPageSize(struct hg3dclass_struct * thisclass_c, float * result_c);

// Return the step size for this scroll bar. 
void cegui_scrlbr_getStepSize(struct hg3dclass_struct * thisclass_c, float * result_c);

// Return the overlap size for this scroll bar. 
void cegui_scrlbr_getOverlapSize(struct hg3dclass_struct * thisclass_c, float * result_c);

// Return the current position of scroll bar within the document. 
void cegui_scrlbr_getScrollPosition(struct hg3dclass_struct * thisclass_c, float * result_c);

// Return a pointer to the 'increase' PushButtoncomponent widget for this Scrollbar
void cegui_scrlbr_getIncreaseButton(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// Return a pointer to the 'decrease' PushButtonScrollbar
void cegui_scrlbr_getDecreaseButton(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// Return a pointer to the ThumbScrollbar
void cegui_scrlbr_getThumb(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// Initialises the Scrollbar
void cegui_scrlbr_initialiseComponents(struct hg3dclass_struct * thisclass_c);

// Set the size of the document or data. 
void cegui_scrlbr_setDocumentSize(struct hg3dclass_struct * thisclass_c, float document_size_c);

// Set the page size for this scroll bar. 
void cegui_scrlbr_setPageSize(struct hg3dclass_struct * thisclass_c, float page_size_c);

// Set the step size for this scroll bar. 
void cegui_scrlbr_setStepSize(struct hg3dclass_struct * thisclass_c, float step_size_c);

// Set the overlap size for this scroll bar. 
void cegui_scrlbr_setOverlapSize(struct hg3dclass_struct * thisclass_c, float overlap_size_c);

// Set the current position of scroll bar within the document. 
void cegui_scrlbr_setScrollPosition(struct hg3dclass_struct * thisclass_c, float position_c);

// Enable or disable the 'end lock' mode for the scrollbar. 
void cegui_scrlbr_setEndLockEnabled(struct hg3dclass_struct * thisclass_c, const int enabled_c);

// Returns whether the 'end lock'mode for the scrollbar is enabled. 
void cegui_scrlbr_isEndLockEnabled(struct hg3dclass_struct * thisclass_c, int * result_c);

// Constructor for Scrollbar
void cegui_scrlbr_construct(char * type_c, char * name_c, struct hg3dclass_struct * result_c);

// Destructor for Scrollbar
void cegui_scrlbr_destruct(struct hg3dclass_struct * thisclass_c);

#endif 
