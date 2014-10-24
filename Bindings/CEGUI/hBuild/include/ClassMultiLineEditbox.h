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

// ClassMultiLineEditbox.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassMultiLineEditbox
#define _DEFINED_HG3D_ClassMultiLineEditbox

#include "ClassPtr.h"
#include "ClassScrollbar.h"


// return true if the edit box has input focus. 
void cegui_mltlnedtbx_hasInputFocus(struct hg3dclass_struct * thisclass_c, int * result_c);

// return true if the edit box is read-only. 
void cegui_mltlnedtbx_isReadOnly(struct hg3dclass_struct * thisclass_c, int * result_c);

// return the current position of the carat. 
void cegui_mltlnedtbx_getCaratIndex(struct hg3dclass_struct * thisclass_c, int * result_c);

// return the current selection start point. 
void cegui_mltlnedtbx_getSelectionStartIndex(struct hg3dclass_struct * thisclass_c, int * result_c);

// return the current selection end point. 
void cegui_mltlnedtbx_getSelectionEndIndex(struct hg3dclass_struct * thisclass_c, int * result_c);

// return the length of the current selection (in code points / characters). 
void cegui_mltlnedtbx_getSelectionLength(struct hg3dclass_struct * thisclass_c, int * result_c);

// return the maximum text length set for this edit box. 
void cegui_mltlnedtbx_getMaxTextLength(struct hg3dclass_struct * thisclass_c, int * result_c);

// Return whether the text in the edit box will be word-wrapped. 
void cegui_mltlnedtbx_isWordWrapped(struct hg3dclass_struct * thisclass_c, int * result_c);

// Return a pointer to the vertical scrollbar component widget for this MultiLineEditbox
void cegui_mltlnedtbx_getVertScrollbar(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// Return whether the vertical scroll bar is always shown. 
void cegui_mltlnedtbx_isVertScrollbarAlwaysShown(struct hg3dclass_struct * thisclass_c, int * result_c);

// Return a pointer to the horizontal scrollbar component widget for this MultiLineEditbox
void cegui_mltlnedtbx_getHorzScrollbar(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// Return the line number a given index falls on with the current formatting. Will return last line if index is out of range. 
void cegui_mltlnedtbx_getLineNumberFromIndex(struct hg3dclass_struct * thisclass_c, int index_c, int * result_c);

// Initialise the Window
void cegui_mltlnedtbx_initialiseComponents(struct hg3dclass_struct * thisclass_c);

// Specify whether the edit box is read-only. 
void cegui_mltlnedtbx_setReadOnly(struct hg3dclass_struct * thisclass_c, int setting_c);

// Set the current position of the carat. 
void cegui_mltlnedtbx_setCaratIndex(struct hg3dclass_struct * thisclass_c, int carat_pos_c);

// Define the current selection for the edit box. 
void cegui_mltlnedtbx_setSelection(struct hg3dclass_struct * thisclass_c, int start_pos_c, int end_pos_c);

// set the maximum text length for this edit box. 
void cegui_mltlnedtbx_setMaxTextLength(struct hg3dclass_struct * thisclass_c, int max_len_c);

// Scroll the view so that the current carat position is visible. 
void cegui_mltlnedtbx_ensureCaratIsVisible(struct hg3dclass_struct * thisclass_c);

// Set whether the text will be word wrapped or not. 
void cegui_mltlnedtbx_setWordWrapping(struct hg3dclass_struct * thisclass_c, int setting_c);

// Set whether the vertical scroll bar should always be shown. 
void cegui_mltlnedtbx_setShowVertScrollbar(struct hg3dclass_struct * thisclass_c, int setting_c);

// Constructor for the MultiLineEditbox
void cegui_mltlnedtbx_construct(char * type_c, char * name_c, struct hg3dclass_struct * result_c);

// Destructor for the MultiLineEditbox
void cegui_mltlnedtbx_destruct(struct hg3dclass_struct * thisclass_c);

#endif 
