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

// ClassSpinner.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassSpinner
#define _DEFINED_HG3D_ClassSpinner

#include "ClassPtr.h"
#include "EnumTextInputMode.h"


// Constructor for Spinner
void cegui_spnnr_construct(char * type_c, char * name_c, struct hg3dclass_struct * result_c);

// Destructor for Spinner
void cegui_spnnr_destruct(struct hg3dclass_struct * thisclass_c);

// Initialises the Window
void cegui_spnnr_initialiseComponents(struct hg3dclass_struct * thisclass_c);

// Return the current spinner value. 
void cegui_spnnr_getCurrentValue(struct hg3dclass_struct * thisclass_c, double * result_c);

// Return the current step value. 
void cegui_spnnr_getStepSize(struct hg3dclass_struct * thisclass_c, double * result_c);

// Return the current maximum limit value for the Spinner
void cegui_spnnr_getMaximumValue(struct hg3dclass_struct * thisclass_c, double * result_c);

// Return the current minimum limit value for the Spinner
void cegui_spnnr_getMinimumValue(struct hg3dclass_struct * thisclass_c, double * result_c);

// Return the current text input / display mode setting. 
void cegui_spnnr_getTextInputMode(struct hg3dclass_struct * thisclass_c, enum EnumTextInputMode * result_c);

// Set the current spinner value. 
void cegui_spnnr_setCurrentValue(struct hg3dclass_struct * thisclass_c, double value_c);

// Set the current step value. 
void cegui_spnnr_setStepSize(struct hg3dclass_struct * thisclass_c, double step_c);

// Set the spinner maximum value. 
void cegui_spnnr_setMaximumValue(struct hg3dclass_struct * thisclass_c, double maxValue_c);

// Set the spinner minimum value. 
void cegui_spnnr_setMinimumValue(struct hg3dclass_struct * thisclass_c, double minVaue_c);

// Set the spinner input / display mode. 
void cegui_spnnr_setTextInputMode(struct hg3dclass_struct * thisclass_c, enum EnumTextInputMode mode_c);

#endif 
