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

// ClassListHeader.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassListHeader
#define _DEFINED_HG3D_ClassListHeader

#include "ClassPtr.h"
#include "ClassListHeaderSegment.h"
#include "EnumSortDirection.h"


// Return the number of columns or segments attached to the header. 
void cegui_lsthdr_getColumnCount(struct hg3dclass_struct * thisclass_c, unsigned int * result_c);

// Return the ListHeaderSegment
void cegui_lsthdr_getSegmentFromColumn(struct hg3dclass_struct * thisclass_c, unsigned int column_c, struct hg3dclass_struct * result_c);

// Return the ListHeaderSegment
void cegui_lsthdr_getSegmentFromID(struct hg3dclass_struct * thisclass_c, unsigned int id_c, struct hg3dclass_struct * result_c);

// Return the ListHeaderSegment
void cegui_lsthdr_getSortSegment(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// Return the zero based column index of the specified segment. 
void cegui_lsthdr_getColumnFromSegment(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * segment_c, unsigned int * result_c);

// Return the zero based column index of the segment with the specified ID. 
void cegui_lsthdr_getColumnFromID(struct hg3dclass_struct * thisclass_c, unsigned int id_c, unsigned int * result_c);

// Return the zero based index of the current sort column. There must be at least one segment/column to successfully call this method. 
void cegui_lsthdr_getSortColumn(struct hg3dclass_struct * thisclass_c, unsigned int * result_c);

// Return the zero based column index of the segment with the specified text. 
void cegui_lsthdr_getColumnWithText(struct hg3dclass_struct * thisclass_c, char * text_c, unsigned int * result_c);

// Return the pixel offset to the given ListHeaderSegment
void cegui_lsthdr_getPixelOffsetToSegment(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * segment_c, float * result_c);

// Return the pixel offset to the ListHeaderSegment
void cegui_lsthdr_getPixelOffsetToColumn(struct hg3dclass_struct * thisclass_c, unsigned int column_c, float * result_c);

// Return the total pixel width of all attached segments. 
void cegui_lsthdr_getTotalSegmentsPixelExtent(struct hg3dclass_struct * thisclass_c, float * result_c);

// Return the currently set sort direction. 
void cegui_lsthdr_getSortDirection(struct hg3dclass_struct * thisclass_c, enum EnumSortDirection * result_c);

// Return whether user manipulation of the sort column & direction are enabled. 
void cegui_lsthdr_isSortingEnabled(struct hg3dclass_struct * thisclass_c, int * result_c);

// Return whether the user may size column segments. 
void cegui_lsthdr_isColumnSizingEnabled(struct hg3dclass_struct * thisclass_c, int * result_c);

// Return whether the user may modify the order of the segments. 
void cegui_lsthdr_isColumnDraggingEnabled(struct hg3dclass_struct * thisclass_c, int * result_c);

// Return the current segment offset value. This value is used to implement scrolling of the header segments within the ListHeader
void cegui_lsthdr_getSegmentOffset(struct hg3dclass_struct * thisclass_c, float * result_c);

// Set whether user manipulation of the sort column and direction is enabled. 
void cegui_lsthdr_setSortingEnabled(struct hg3dclass_struct * thisclass_c, int setting_c);

// Set the current sort direction. 
void cegui_lsthdr_setSortDirection(struct hg3dclass_struct * thisclass_c, enum EnumSortDirection direction_c);

// Set the column segment to be used as the sort column. 
void cegui_lsthdr_setSortSegment(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * segment_c);

// Set the column to be used as the sort column. 
void cegui_lsthdr_setSortColumn(struct hg3dclass_struct * thisclass_c, unsigned int column_c);

// Set the column to to be used for sorting via its ID code. 
void cegui_lsthdr_setSortColumnFromID(struct hg3dclass_struct * thisclass_c, unsigned int id_c);

// Set whether columns may be sized by the user. 
void cegui_lsthdr_setColumnSizingEnabled(struct hg3dclass_struct * thisclass_c, int setting_c);

// Set whether columns may be reordered by the user via drag and drop. 
void cegui_lsthdr_setColumnDraggingEnabled(struct hg3dclass_struct * thisclass_c, int setting_c);

// Removes a column segment from the ListHeader
void cegui_lsthdr_removeColumn(struct hg3dclass_struct * thisclass_c, unsigned int column_c);

// Remove the specified segment from the ListHeader
void cegui_lsthdr_removeSegment(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * segment_c);

// Moves a column segment into a new position. 
void cegui_lsthdr_moveColumn(struct hg3dclass_struct * thisclass_c, unsigned int column_c, unsigned int position_c);

// Move a column segment to a new position. 
void cegui_lsthdr_moveColumn2(struct hg3dclass_struct * thisclass_c, unsigned int column_c, struct hg3dclass_struct * position_c);

// Moves a segment into a new position. 
void cegui_lsthdr_moveSegment(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * segment_c, unsigned int position_c);

// Move a segment to a new position. 
void cegui_lsthdr_moveSegment2(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * segment_c, struct hg3dclass_struct * position_c);

// Set the current base segment offset. (This implements scrolling of the header segments within the header area). 
void cegui_lsthdr_setSegmentOffset(struct hg3dclass_struct * thisclass_c, float offset_c);

// Constructor for the list header base class. 
void cegui_lsthdr_construct(char * type_c, char * name_c, struct hg3dclass_struct * result_c);

// Destructor for the list header base class. 
void cegui_lsthdr_destruct(struct hg3dclass_struct * thisclass_c);

#endif 
