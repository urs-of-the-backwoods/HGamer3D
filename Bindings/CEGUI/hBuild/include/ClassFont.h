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

// ClassFont.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassFont
#define _DEFINED_HG3D_ClassFont

#include "ClassPtr.h"


// Destructor. 
void cegui_fnt_destruct(struct hg3dclass_struct * thisclass_c);

// Return the string holding the font name. 
void cegui_fnt_getName(struct hg3dclass_struct * thisclass_c, char * result_c);

// Return the type of the font. 
void cegui_fnt_getTypeName(struct hg3dclass_struct * thisclass_c, char * result_c);

// Return whether this Font
void cegui_fnt_isCodepointAvailable(struct hg3dclass_struct * thisclass_c, int cp_c, int * result_c);

// Enable or disable auto-scaling for this Font
void cegui_fnt_setAutoScaled(struct hg3dclass_struct * thisclass_c, const int auto_scaled_c);

// Return whether this Font
void cegui_fnt_isAutoScaled(struct hg3dclass_struct * thisclass_c, int * result_c);

// Return the pixel line spacing value for. 
void cegui_fnt_getLineSpacing(struct hg3dclass_struct * thisclass_c, float y_scale_c, float * result_c);

// return the exact pixel height of the font. 
void cegui_fnt_getFontHeight(struct hg3dclass_struct * thisclass_c, float y_scale_c, float * result_c);

// Return the number of pixels from the top of the highest glyph to the baseline. 
void cegui_fnt_getBaseline(struct hg3dclass_struct * thisclass_c, float y_scale_c, float * result_c);

// Return the pixel width of the specified text if rendered with this Font
void cegui_fnt_getTextExtent(struct hg3dclass_struct * thisclass_c, char * text_c, float x_scale_c, float * result_c);

// Return the index of the closest text character in String textpixel
void cegui_fnt_getCharAtPixel(struct hg3dclass_struct * thisclass_c, char * text_c, float pixel_c, float x_scale_c, int * result_c);

// Return the index of the closest text character in String textstart_charpixel
void cegui_fnt_getCharAtPixel2(struct hg3dclass_struct * thisclass_c, char * text_c, int start_char_c, float pixel_c, float x_scale_c, int * result_c);

// Sets the default resource group to be used when loading font data. 
void cegui_fnt_setDefaultResourceGroup(char * resourceGroup_c);

// Returns the default resource group currently set for Fonts. 
void cegui_fnt_getDefaultResourceGroup(char * result_c);

#endif 
