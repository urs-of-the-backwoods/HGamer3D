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

// ClassSystemHG3D.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassSystemHG3D
#define _DEFINED_HG3D_ClassSystemHG3D

#include "ClassPtr.h"
#include "ClassLogger.h"
#include "ClassDefaultResourceProvider.h"
#include "ClassSystem.h"
#include "ClassSchemeManager.h"
#include "ClassFontManager.h"


// 
void cegui_sstm_hg3d_createNoLogger(struct hg3dclass_struct * result_c);

// 
void cegui_sstm_hg3d_getDefaultResourceProvider(struct hg3dclass_struct * system_c, struct hg3dclass_struct * result_c);

// 
void cegui_sstm_hg3d_getSchemeManagerSingleton(struct hg3dclass_struct * result_c);

// 
void cegui_sstm_hg3d_schemeManagerCreate(struct hg3dclass_struct * smgr_c, char * scheme_c);

// 
void cegui_sstm_hg3d_fontManagerCreate(struct hg3dclass_struct * fmgr_c, char * font_c);

// 
void cegui_sstm_hg3d_getFontManagerSingleton(struct hg3dclass_struct * result_c);

// 
void cegui_sstm_hg3d_getLoggerSingleton(struct hg3dclass_struct * result_c);

#endif 
