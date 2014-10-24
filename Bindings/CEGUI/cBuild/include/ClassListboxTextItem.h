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

// ClassListboxTextItem.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassListboxTextItem
#define _DEFINED_HG3D_ClassListboxTextItem

#include "ClassPtr.h"
#include "ClassFont.h"


// base class destructor 
void cegui_lstbxti_destruct(struct hg3dclass_struct * thisclass_c);

// Return a pointer to the font being used by this ListboxTextItem
void cegui_lstbxti_getFont(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// Set the font to be used by this ListboxTextItem
void cegui_lstbxti_setFont(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * font_c);

// Set the font to be used by this ListboxTextItem
void cegui_lstbxti_setFont2(struct hg3dclass_struct * thisclass_c, char * font_name_c);

// Set whether the the ListboxTextItem
void cegui_lstbxti_setTextParsingEnabled(struct hg3dclass_struct * thisclass_c, const int enable_c);

// return whether text parsing is enabled for this ListboxTextItem
void cegui_lstbxti_isTextParsingEnabled(struct hg3dclass_struct * thisclass_c, int * result_c);

// set the text string for this list box item. 
void cegui_lstbxti_setText(struct hg3dclass_struct * thisclass_c, char * text_c);

#endif 
