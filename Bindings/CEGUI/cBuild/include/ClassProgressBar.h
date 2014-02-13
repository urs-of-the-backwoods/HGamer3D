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

// ClassProgressBar.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassProgressBar
#define _DEFINED_HG3D_ClassProgressBar

#include "ClassPtr.h"


// return the current progress value 
void cegui_prgbr_getProgress(struct hg3dclass_struct * thisclass_c, float * result_c);

// return the current step size 
void cegui_prgbr_getStep(struct hg3dclass_struct * thisclass_c, float * result_c);

// set the current progress. 
void cegui_prgbr_setProgress(struct hg3dclass_struct * thisclass_c, float progress_c);

// set the size of the 'step' in percentage points (default is 0.01f or 1%). 
void cegui_prgbr_setStepSize(struct hg3dclass_struct * thisclass_c, float step_val_c);

// cause the progress to step 
void cegui_prgbr_step(struct hg3dclass_struct * thisclass_c);

// Modify the progress level by a specified delta. 
void cegui_prgbr_adjustProgress(struct hg3dclass_struct * thisclass_c, float delta_c);

// Constructor for ProgressBar
void cegui_prgbr_construct(char * type_c, char * name_c, struct hg3dclass_struct * result_c);

// Destructor for ProgressBar
void cegui_prgbr_destruct(struct hg3dclass_struct * thisclass_c);

#endif 
