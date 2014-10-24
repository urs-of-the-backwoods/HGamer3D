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

// ClassThumb.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassThumb
#define _DEFINED_HG3D_ClassThumb

#include "ClassPtr.h"


// return whether hot-tracking is enabled or not. 
void cegui_thmb_isHotTracked(struct hg3dclass_struct * thisclass_c, int * result_c);

// return whether the thumb is movable on the vertical axis. 
void cegui_thmb_isVertFree(struct hg3dclass_struct * thisclass_c, int * result_c);

// return whether the thumb is movable on the horizontal axis. 
void cegui_thmb_isHorzFree(struct hg3dclass_struct * thisclass_c, int * result_c);

// set whether the thumb uses hot-tracking. 
void cegui_thmb_setHotTracked(struct hg3dclass_struct * thisclass_c, int setting_c);

// set whether thumb is movable on the vertical axis. 
void cegui_thmb_setVertFree(struct hg3dclass_struct * thisclass_c, int setting_c);

// set whether thumb is movable on the horizontal axis. 
void cegui_thmb_setHorzFree(struct hg3dclass_struct * thisclass_c, int setting_c);

// set the movement range of the thumb for the vertical axis. 
void cegui_thmb_setVertRange(struct hg3dclass_struct * thisclass_c, float min_c, float max_c);

// set the movement range of the thumb for the horizontal axis. 
void cegui_thmb_setHorzRange(struct hg3dclass_struct * thisclass_c, float min_c, float max_c);

// Constructor for Thumb
void cegui_thmb_construct(char * type_c, char * name_c, struct hg3dclass_struct * result_c);

// Destructor for Thumb
void cegui_thmb_destruct(struct hg3dclass_struct * thisclass_c);

#endif 
