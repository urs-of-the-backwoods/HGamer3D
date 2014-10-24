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

// ClassEditbox.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassEditbox
#define _DEFINED_HG3D_ClassEditbox

#include "ClassPtr.h"


// return true if the Editbox
void cegui_edtbx_hasInputFocus(struct hg3dclass_struct * thisclass_c, int * result_c);

// return true if the Editbox
void cegui_edtbx_isReadOnly(struct hg3dclass_struct * thisclass_c, int * result_c);

// return true if the text for the Editbox
void cegui_edtbx_isTextMasked(struct hg3dclass_struct * thisclass_c, int * result_c);

// return true if the Editbox
void cegui_edtbx_isTextValid(struct hg3dclass_struct * thisclass_c, int * result_c);

// return the currently set validation string 
void cegui_edtbx_getValidationString(struct hg3dclass_struct * thisclass_c, char * result_c);

// return the current position of the carat. 
void cegui_edtbx_getCaratIndex(struct hg3dclass_struct * thisclass_c, int * result_c);

// return the current selection start point. 
void cegui_edtbx_getSelectionStartIndex(struct hg3dclass_struct * thisclass_c, int * result_c);

// return the current selection end point. 
void cegui_edtbx_getSelectionEndIndex(struct hg3dclass_struct * thisclass_c, int * result_c);

// return the length of the current selection (in code points / characters). 
void cegui_edtbx_getSelectionLength(struct hg3dclass_struct * thisclass_c, int * result_c);

// return the utf32 code point used when rendering masked text. 
void cegui_edtbx_getMaskCodePoint(struct hg3dclass_struct * thisclass_c, int * result_c);

// return the maximum text length set for this Editbox
void cegui_edtbx_getMaxTextLength(struct hg3dclass_struct * thisclass_c, int * result_c);

// Specify whether the Editbox
void cegui_edtbx_setReadOnly(struct hg3dclass_struct * thisclass_c, int setting_c);

// Specify whether the text for the Editbox
void cegui_edtbx_setTextMasked(struct hg3dclass_struct * thisclass_c, int setting_c);

// Set the text validation string. 
void cegui_edtbx_setValidationString(struct hg3dclass_struct * thisclass_c, char * validation_string_c);

// Set the current position of the carat. 
void cegui_edtbx_setCaratIndex(struct hg3dclass_struct * thisclass_c, int carat_pos_c);

// Define the current selection for the Editbox
void cegui_edtbx_setSelection(struct hg3dclass_struct * thisclass_c, int start_pos_c, int end_pos_c);

// set the utf32 code point used when rendering masked text. 
void cegui_edtbx_setMaskCodePoint(struct hg3dclass_struct * thisclass_c, int code_point_c);

// set the maximum text length for this Editbox
void cegui_edtbx_setMaxTextLength(struct hg3dclass_struct * thisclass_c, int max_len_c);

// Constructor for Editbox
void cegui_edtbx_construct(char * type_c, char * name_c, struct hg3dclass_struct * result_c);

// Destructor for Editbox
void cegui_edtbx_destruct(struct hg3dclass_struct * thisclass_c);

#endif 
