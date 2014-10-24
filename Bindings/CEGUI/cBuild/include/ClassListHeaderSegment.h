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

// ClassListHeaderSegment.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassListHeaderSegment
#define _DEFINED_HG3D_ClassListHeaderSegment

#include "ClassPtr.h"
#include "EnumSortDirection.h"


// Return whether this segment can be sized. 
void cegui_lsthdrsgm_isSizingEnabled(struct hg3dclass_struct * thisclass_c, int * result_c);

// Return the current sort direction set for this segment. 
void cegui_lsthdrsgm_getSortDirection(struct hg3dclass_struct * thisclass_c, enum EnumSortDirection * result_c);

// Return whether drag moving is enabled for this segment. 
void cegui_lsthdrsgm_isDragMovingEnabled(struct hg3dclass_struct * thisclass_c, int * result_c);

// Return whether the segment is clickable. 
void cegui_lsthdrsgm_isClickable(struct hg3dclass_struct * thisclass_c, int * result_c);

// Return whether the segment is currently in its hovering state. 
void cegui_lsthdrsgm_isSegmentHovering(struct hg3dclass_struct * thisclass_c, int * result_c);

// Return whether the segment is currently in its pushed state. 
void cegui_lsthdrsgm_isSegmentPushed(struct hg3dclass_struct * thisclass_c, int * result_c);

// Return whether the splitter is currently in its hovering state. 
void cegui_lsthdrsgm_isSplitterHovering(struct hg3dclass_struct * thisclass_c, int * result_c);

// Return whether the segment is currently being drag-moved. 
void cegui_lsthdrsgm_isBeingDragMoved(struct hg3dclass_struct * thisclass_c, int * result_c);

// Return whether the segment is currently being drag-moved. 
void cegui_lsthdrsgm_isBeingDragSized(struct hg3dclass_struct * thisclass_c, int * result_c);

// Set whether this segment can be sized. 
void cegui_lsthdrsgm_setSizingEnabled(struct hg3dclass_struct * thisclass_c, int setting_c);

// Set the current sort direction set for this segment. 
void cegui_lsthdrsgm_setSortDirection(struct hg3dclass_struct * thisclass_c, enum EnumSortDirection sort_dir_c);

// Set whether drag moving is allowed for this segment. 
void cegui_lsthdrsgm_setDragMovingEnabled(struct hg3dclass_struct * thisclass_c, int setting_c);

// Set whether the segment is clickable. 
void cegui_lsthdrsgm_setClickable(struct hg3dclass_struct * thisclass_c, int setting_c);

// 
void cegui_lsthdrsgm_setSizingCursorImage2(struct hg3dclass_struct * thisclass_c, char * imageset_c, char * image_c);

// 
void cegui_lsthdrsgm_setMovingCursorImage2(struct hg3dclass_struct * thisclass_c, char * imageset_c, char * image_c);

// Constructor for list header segment base class. 
void cegui_lsthdrsgm_construct(char * type_c, char * name_c, struct hg3dclass_struct * result_c);

// Destructor for list header segment base class. 
void cegui_lsthdrsgm_destruct(struct hg3dclass_struct * thisclass_c);

#endif 
