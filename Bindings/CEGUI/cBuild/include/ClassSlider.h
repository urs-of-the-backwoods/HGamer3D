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

// ClassSlider.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassSlider
#define _DEFINED_HG3D_ClassSlider

#include "ClassPtr.h"
#include "ClassThumb.h"


// return the current slider value. 
void cegui_sldr_getCurrentValue(struct hg3dclass_struct * thisclass_c, float * result_c);

// return the maximum value set for this widget 
void cegui_sldr_getMaxValue(struct hg3dclass_struct * thisclass_c, float * result_c);

// return the current click step setting for the slider. 
void cegui_sldr_getClickStep(struct hg3dclass_struct * thisclass_c, float * result_c);

// Return a pointer to the ThumbSlider
void cegui_sldr_getThumb(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// Initialises the Window
void cegui_sldr_initialiseComponents(struct hg3dclass_struct * thisclass_c);

// set the maximum value for the slider. Note that the minimum value is fixed at 0. 
void cegui_sldr_setMaxValue(struct hg3dclass_struct * thisclass_c, float maxVal_c);

// set the current slider value. 
void cegui_sldr_setCurrentValue(struct hg3dclass_struct * thisclass_c, float value_c);

// set the current click step setting for the slider. 
void cegui_sldr_setClickStep(struct hg3dclass_struct * thisclass_c, float step_c);

// Slider
void cegui_sldr_construct(char * type_c, char * name_c, struct hg3dclass_struct * result_c);

// Slider
void cegui_sldr_destruct(struct hg3dclass_struct * thisclass_c);

#endif 
