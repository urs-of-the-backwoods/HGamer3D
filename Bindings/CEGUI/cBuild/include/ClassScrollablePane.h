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

// ClassScrollablePane.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassScrollablePane
#define _DEFINED_HG3D_ClassScrollablePane

#include "ClassPtr.h"
#include "ClassScrolledContainer.h"
#include "ClassScrollbar.h"


// Constructor for the ScrollablePane
void cegui_scrlpn_construct(char * type_c, char * name_c, struct hg3dclass_struct * result_c);

// Destructor for the ScrollablePane
void cegui_scrlpn_destruct(struct hg3dclass_struct * thisclass_c);

// Returns a pointer to the window holding the pane contents. 
void cegui_scrlpn_getContentPane(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// Return whether the vertical scroll bar is always shown. 
void cegui_scrlpn_isVertScrollbarAlwaysShown(struct hg3dclass_struct * thisclass_c, int * result_c);

// Set whether the vertical scroll bar should always be shown. 
void cegui_scrlpn_setShowVertScrollbar(struct hg3dclass_struct * thisclass_c, int setting_c);

// Return whether the horizontal scroll bar is always shown. 
void cegui_scrlpn_isHorzScrollbarAlwaysShown(struct hg3dclass_struct * thisclass_c, int * result_c);

// Set whether the horizontal scroll bar should always be shown. 
void cegui_scrlpn_setShowHorzScrollbar(struct hg3dclass_struct * thisclass_c, int setting_c);

// Return whether the content pane is auto sized. 
void cegui_scrlpn_isContentPaneAutoSized(struct hg3dclass_struct * thisclass_c, int * result_c);

// Set whether the content pane should be auto-sized. 
void cegui_scrlpn_setContentPaneAutoSized(struct hg3dclass_struct * thisclass_c, int setting_c);

// Returns the horizontal scrollbar step size as a fraction of one complete view page. 
void cegui_scrlpn_getHorizontalStepSize(struct hg3dclass_struct * thisclass_c, float * result_c);

// Sets the horizontal scrollbar step size as a fraction of one complete view page. 
void cegui_scrlpn_setHorizontalStepSize(struct hg3dclass_struct * thisclass_c, float step_c);

// Returns the horizontal scrollbar overlap size as a fraction of one complete view page. 
void cegui_scrlpn_getHorizontalOverlapSize(struct hg3dclass_struct * thisclass_c, float * result_c);

// Sets the horizontal scrollbar overlap size as a fraction of one complete view page. 
void cegui_scrlpn_setHorizontalOverlapSize(struct hg3dclass_struct * thisclass_c, float overlap_c);

// Returns the horizontal scroll position as a fraction of the complete scrollable width. 
void cegui_scrlpn_getHorizontalScrollPosition(struct hg3dclass_struct * thisclass_c, float * result_c);

// Sets the horizontal scroll position as a fraction of the complete scrollable width. 
void cegui_scrlpn_setHorizontalScrollPosition(struct hg3dclass_struct * thisclass_c, float position_c);

// Returns the vertical scrollbar step size as a fraction of one complete view page. 
void cegui_scrlpn_getVerticalStepSize(struct hg3dclass_struct * thisclass_c, float * result_c);

// Sets the vertical scrollbar step size as a fraction of one complete view page. 
void cegui_scrlpn_setVerticalStepSize(struct hg3dclass_struct * thisclass_c, float step_c);

// Returns the vertical scrollbar overlap size as a fraction of one complete view page. 
void cegui_scrlpn_getVerticalOverlapSize(struct hg3dclass_struct * thisclass_c, float * result_c);

// Sets the vertical scrollbar overlap size as a fraction of one complete view page. 
void cegui_scrlpn_setVerticalOverlapSize(struct hg3dclass_struct * thisclass_c, float overlap_c);

// Returns the vertical scroll position as a fraction of the complete scrollable height. 
void cegui_scrlpn_getVerticalScrollPosition(struct hg3dclass_struct * thisclass_c, float * result_c);

// Sets the vertical scroll position as a fraction of the complete scrollable height. 
void cegui_scrlpn_setVerticalScrollPosition(struct hg3dclass_struct * thisclass_c, float position_c);

// Return a pointer to the vertical scrollbar component widget for this ScrollablePane
void cegui_scrlpn_getVertScrollbar(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// Return a pointer to the horizontal scrollbar component widget for this ScrollablePane
void cegui_scrlpn_getHorzScrollbar(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// Initialises the Window
void cegui_scrlpn_initialiseComponents(struct hg3dclass_struct * thisclass_c);

// Internal destroy method which actually just adds the window and any parent destructed child windows to the dead pool. 
void cegui_scrlpn_destroy(struct hg3dclass_struct * thisclass_c);

#endif 
