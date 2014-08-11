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

// ClassHG3DWindowStaticFunctions.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassHG3DWindowStaticFunctions
#define _DEFINED_HG3D_ClassHG3DWindowStaticFunctions

#include "ClassPtr.h"
#include "ClassPushButton.h"
#include "ClassWindow.h"
#include "ClassListbox.h"
#include "ClassCombobox.h"
#include "ClassRadioButton.h"
#include "ClassEditbox.h"
#include "ClassMultiLineEditbox.h"
#include "ClassFrameWindow.h"
#include "ClassProgressBar.h"
#include "ClassSlider.h"
#include "ClassSpinner.h"
#include "ClassMultiColumnList.h"
#include "ClassUDim.h"
#include "ClassUVector2.h"
#include "ClassSystem.h"


// 
void cegui_hg3dwsfs_castWindowToPushButton(struct hg3dclass_struct * window_c, struct hg3dclass_struct * result_c);

// 
void cegui_hg3dwsfs_castWindowToListbox(struct hg3dclass_struct * window_c, struct hg3dclass_struct * result_c);

// 
void cegui_hg3dwsfs_castWindowToCombobox(struct hg3dclass_struct * window_c, struct hg3dclass_struct * result_c);

// 
void cegui_hg3dwsfs_castWindowToRadioButton(struct hg3dclass_struct * window_c, struct hg3dclass_struct * result_c);

// 
void cegui_hg3dwsfs_castWindowToEditbox(struct hg3dclass_struct * window_c, struct hg3dclass_struct * result_c);

// 
void cegui_hg3dwsfs_castWindowToMultiLineEditbox(struct hg3dclass_struct * window_c, struct hg3dclass_struct * result_c);

// 
void cegui_hg3dwsfs_castWindowToFrameWindow(struct hg3dclass_struct * window_c, struct hg3dclass_struct * result_c);

// 
void cegui_hg3dwsfs_castWindowToProgressBar(struct hg3dclass_struct * window_c, struct hg3dclass_struct * result_c);

// 
void cegui_hg3dwsfs_castWindowToSlider(struct hg3dclass_struct * window_c, struct hg3dclass_struct * result_c);

// 
void cegui_hg3dwsfs_castWindowToSpinner(struct hg3dclass_struct * window_c, struct hg3dclass_struct * result_c);

// 
void cegui_hg3dwsfs_castWindowToMultiColumnList(struct hg3dclass_struct * window_c, struct hg3dclass_struct * result_c);

// 
void cegui_hg3dwsfs_udScale(struct hg3dclass_struct * ud_c, float * result_c);

// 
void cegui_hg3dwsfs_udOffset(struct hg3dclass_struct * ud_c, float * result_c);

// 
void cegui_hg3dwsfs_v2X(struct hg3dclass_struct * uv2_c, struct hg3dclass_struct * result_c);

// 
void cegui_hg3dwsfs_v2Y(struct hg3dclass_struct * uv2_c, struct hg3dclass_struct * result_c);

// 
void cegui_hg3dwsfs_getWindowWidth(struct hg3dclass_struct * window_c, struct hg3dclass_struct * result_c);

// 
void cegui_hg3dwsfs_getWindowHeight(struct hg3dclass_struct * window_c, struct hg3dclass_struct * result_c);

// 
void cegui_hg3dwsfs_setNewWindowSize(struct hg3dclass_struct * system_c, float width_c, float height_c);

// 
void cegui_hg3dwsfs_getWindowMargin(struct hg3dclass_struct * window_c, struct hg3dclass_struct * result_c);

// 
void cegui_hg3dwsfs_setWindowMargin(struct hg3dclass_struct * window_c, struct hg3dclass_struct * margin_c);

#endif 
