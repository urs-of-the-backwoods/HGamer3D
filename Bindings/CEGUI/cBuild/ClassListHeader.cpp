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

// ClassListHeader.cpp

// 

#include <wchar.h>
#include <string>
#include <iostream>

#include <iostream>
	#include <typeinfo>
	#include <stdio.h>
	#include <cstring>
	#include <exception>
	#include "CEGUIDllDefines.h"
	#include "ClassPtr.h"
	#include "EnumSortDirection.h"
#include "./CEGUI.h"
#include "./CEGUIString.h"
#include "RendererModules/Ogre/CEGUIOgreRenderer.h"
#include "./WindowManagerHG3D.h"
#include "./SystemHG3D.h"
#include "HG3DCommandHandler.h"
#include "HG3DEventController.h"
#include "HG3DEventModule.h"
#include "HG3DEventStaticFunctions.h"
#include "HG3DListboxStaticFunctions.h"
#include "HG3DWindowStaticFunctions.h"

using namespace CEGUI;



// Return the number of columns or segments attached to the header. 
extern "C" CEGUI_LIB_EXPORT void cegui_lsthdr_getColumnCount(struct hg3dclass_struct * thisclass_c, unsigned int * result_c)
{
  CEGUI::ListHeader * thisclass_cpp = static_cast<CEGUI::ListHeader*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ListHeader"));
  uint result_cpp;
  result_cpp = (thisclass_cpp->getColumnCount());
  *result_c = (unsigned int)result_cpp;
};

// Return the ListHeaderSegment
extern "C" CEGUI_LIB_EXPORT void cegui_lsthdr_getSegmentFromColumn(struct hg3dclass_struct * thisclass_c, unsigned int column_c, struct hg3dclass_struct * result_c)
{
  CEGUI::ListHeader * thisclass_cpp = static_cast<CEGUI::ListHeader*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ListHeader"));
  uint column_cpp = (uint)column_c;
  CEGUI::ListHeaderSegment * result_cpp;
  result_cpp = &(thisclass_cpp->getSegmentFromColumn(column_cpp));
  *result_c = getHG3DClass_ListHeaderSegment((void *) result_cpp);
;
};

// Return the ListHeaderSegment
extern "C" CEGUI_LIB_EXPORT void cegui_lsthdr_getSegmentFromID(struct hg3dclass_struct * thisclass_c, unsigned int id_c, struct hg3dclass_struct * result_c)
{
  CEGUI::ListHeader * thisclass_cpp = static_cast<CEGUI::ListHeader*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ListHeader"));
  uint id_cpp = (uint)id_c;
  CEGUI::ListHeaderSegment * result_cpp;
  result_cpp = &(thisclass_cpp->getSegmentFromID(id_cpp));
  *result_c = getHG3DClass_ListHeaderSegment((void *) result_cpp);
;
};

// Return the ListHeaderSegment
extern "C" CEGUI_LIB_EXPORT void cegui_lsthdr_getSortSegment(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  CEGUI::ListHeader * thisclass_cpp = static_cast<CEGUI::ListHeader*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ListHeader"));
  CEGUI::ListHeaderSegment * result_cpp;
  result_cpp = &(thisclass_cpp->getSortSegment());
  *result_c = getHG3DClass_ListHeaderSegment((void *) result_cpp);
;
};

// Return the zero based column index of the specified segment. 
extern "C" CEGUI_LIB_EXPORT void cegui_lsthdr_getColumnFromSegment(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * segment_c, unsigned int * result_c)
{
  CEGUI::ListHeader * thisclass_cpp = static_cast<CEGUI::ListHeader*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ListHeader"));
  const CEGUI::ListHeaderSegment * segment_cpp = static_cast<CEGUI::ListHeaderSegment*> (getHG3DClassPtr(*segment_c, "CEGUI::ListHeaderSegment"));
  uint result_cpp;
  result_cpp = (thisclass_cpp->getColumnFromSegment(*segment_cpp));
  *result_c = (unsigned int)result_cpp;
};

// Return the zero based column index of the segment with the specified ID. 
extern "C" CEGUI_LIB_EXPORT void cegui_lsthdr_getColumnFromID(struct hg3dclass_struct * thisclass_c, unsigned int id_c, unsigned int * result_c)
{
  CEGUI::ListHeader * thisclass_cpp = static_cast<CEGUI::ListHeader*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ListHeader"));
  uint id_cpp = (uint)id_c;
  uint result_cpp;
  result_cpp = (thisclass_cpp->getColumnFromID(id_cpp));
  *result_c = (unsigned int)result_cpp;
};

// Return the zero based index of the current sort column. There must be at least one segment/column to successfully call this method. 
extern "C" CEGUI_LIB_EXPORT void cegui_lsthdr_getSortColumn(struct hg3dclass_struct * thisclass_c, unsigned int * result_c)
{
  CEGUI::ListHeader * thisclass_cpp = static_cast<CEGUI::ListHeader*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ListHeader"));
  uint result_cpp;
  result_cpp = (thisclass_cpp->getSortColumn());
  *result_c = (unsigned int)result_cpp;
};

// Return the zero based column index of the segment with the specified text. 
extern "C" CEGUI_LIB_EXPORT void cegui_lsthdr_getColumnWithText(struct hg3dclass_struct * thisclass_c, char * text_c, unsigned int * result_c)
{
  CEGUI::ListHeader * thisclass_cpp = static_cast<CEGUI::ListHeader*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ListHeader"));
  CEGUI::String text_cpp = CEGUI::String((const char*) text_c);
  uint result_cpp;
  result_cpp = (thisclass_cpp->getColumnWithText(text_cpp));
  *result_c = (unsigned int)result_cpp;
};

// Return the pixel offset to the given ListHeaderSegment
extern "C" CEGUI_LIB_EXPORT void cegui_lsthdr_getPixelOffsetToSegment(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * segment_c, float * result_c)
{
  CEGUI::ListHeader * thisclass_cpp = static_cast<CEGUI::ListHeader*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ListHeader"));
  const CEGUI::ListHeaderSegment * segment_cpp = static_cast<CEGUI::ListHeaderSegment*> (getHG3DClassPtr(*segment_c, "CEGUI::ListHeaderSegment"));
  float result_cpp;
  result_cpp = (thisclass_cpp->getPixelOffsetToSegment(*segment_cpp));
  *result_c = (float)result_cpp;
};

// Return the pixel offset to the ListHeaderSegment
extern "C" CEGUI_LIB_EXPORT void cegui_lsthdr_getPixelOffsetToColumn(struct hg3dclass_struct * thisclass_c, unsigned int column_c, float * result_c)
{
  CEGUI::ListHeader * thisclass_cpp = static_cast<CEGUI::ListHeader*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ListHeader"));
  uint column_cpp = (uint)column_c;
  float result_cpp;
  result_cpp = (thisclass_cpp->getPixelOffsetToColumn(column_cpp));
  *result_c = (float)result_cpp;
};

// Return the total pixel width of all attached segments. 
extern "C" CEGUI_LIB_EXPORT void cegui_lsthdr_getTotalSegmentsPixelExtent(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  CEGUI::ListHeader * thisclass_cpp = static_cast<CEGUI::ListHeader*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ListHeader"));
  float result_cpp;
  result_cpp = (thisclass_cpp->getTotalSegmentsPixelExtent());
  *result_c = (float)result_cpp;
};

// Return the currently set sort direction. 
extern "C" CEGUI_LIB_EXPORT void cegui_lsthdr_getSortDirection(struct hg3dclass_struct * thisclass_c, enum EnumSortDirection * result_c)
{
  CEGUI::ListHeader * thisclass_cpp = static_cast<CEGUI::ListHeader*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ListHeader"));
  enum ListHeaderSegment::SortDirection result_cpp;
  result_cpp = (thisclass_cpp->getSortDirection());
  *result_c = (enum EnumSortDirection) result_cpp;
};

// Return whether user manipulation of the sort column & direction are enabled. 
extern "C" CEGUI_LIB_EXPORT void cegui_lsthdr_isSortingEnabled(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::ListHeader * thisclass_cpp = static_cast<CEGUI::ListHeader*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ListHeader"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isSortingEnabled());
  *result_c = (int)result_cpp;
};

// Return whether the user may size column segments. 
extern "C" CEGUI_LIB_EXPORT void cegui_lsthdr_isColumnSizingEnabled(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::ListHeader * thisclass_cpp = static_cast<CEGUI::ListHeader*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ListHeader"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isColumnSizingEnabled());
  *result_c = (int)result_cpp;
};

// Return whether the user may modify the order of the segments. 
extern "C" CEGUI_LIB_EXPORT void cegui_lsthdr_isColumnDraggingEnabled(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::ListHeader * thisclass_cpp = static_cast<CEGUI::ListHeader*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ListHeader"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isColumnDraggingEnabled());
  *result_c = (int)result_cpp;
};

// Return the current segment offset value. This value is used to implement scrolling of the header segments within the ListHeader
extern "C" CEGUI_LIB_EXPORT void cegui_lsthdr_getSegmentOffset(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  CEGUI::ListHeader * thisclass_cpp = static_cast<CEGUI::ListHeader*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ListHeader"));
  float result_cpp;
  result_cpp = (thisclass_cpp->getSegmentOffset());
  *result_c = (float)result_cpp;
};

// Set whether user manipulation of the sort column and direction is enabled. 
extern "C" CEGUI_LIB_EXPORT void cegui_lsthdr_setSortingEnabled(struct hg3dclass_struct * thisclass_c, int setting_c)
{
  CEGUI::ListHeader * thisclass_cpp = static_cast<CEGUI::ListHeader*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ListHeader"));
  bool setting_cpp = (bool)setting_c;
  (thisclass_cpp->setSortingEnabled(setting_cpp));
};

// Set the current sort direction. 
extern "C" CEGUI_LIB_EXPORT void cegui_lsthdr_setSortDirection(struct hg3dclass_struct * thisclass_c, enum EnumSortDirection direction_c)
{
  CEGUI::ListHeader * thisclass_cpp = static_cast<CEGUI::ListHeader*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ListHeader"));
  enum ListHeaderSegment::SortDirection direction_cpp = (enum ListHeaderSegment::SortDirection)direction_c;
  (thisclass_cpp->setSortDirection(direction_cpp));
};

// Set the column segment to be used as the sort column. 
extern "C" CEGUI_LIB_EXPORT void cegui_lsthdr_setSortSegment(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * segment_c)
{
  CEGUI::ListHeader * thisclass_cpp = static_cast<CEGUI::ListHeader*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ListHeader"));
  const CEGUI::ListHeaderSegment * segment_cpp = static_cast<CEGUI::ListHeaderSegment*> (getHG3DClassPtr(*segment_c, "CEGUI::ListHeaderSegment"));
  (thisclass_cpp->setSortSegment(*segment_cpp));
};

// Set the column to be used as the sort column. 
extern "C" CEGUI_LIB_EXPORT void cegui_lsthdr_setSortColumn(struct hg3dclass_struct * thisclass_c, unsigned int column_c)
{
  CEGUI::ListHeader * thisclass_cpp = static_cast<CEGUI::ListHeader*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ListHeader"));
  uint column_cpp = (uint)column_c;
  (thisclass_cpp->setSortColumn(column_cpp));
};

// Set the column to to be used for sorting via its ID code. 
extern "C" CEGUI_LIB_EXPORT void cegui_lsthdr_setSortColumnFromID(struct hg3dclass_struct * thisclass_c, unsigned int id_c)
{
  CEGUI::ListHeader * thisclass_cpp = static_cast<CEGUI::ListHeader*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ListHeader"));
  uint id_cpp = (uint)id_c;
  (thisclass_cpp->setSortColumnFromID(id_cpp));
};

// Set whether columns may be sized by the user. 
extern "C" CEGUI_LIB_EXPORT void cegui_lsthdr_setColumnSizingEnabled(struct hg3dclass_struct * thisclass_c, int setting_c)
{
  CEGUI::ListHeader * thisclass_cpp = static_cast<CEGUI::ListHeader*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ListHeader"));
  bool setting_cpp = (bool)setting_c;
  (thisclass_cpp->setColumnSizingEnabled(setting_cpp));
};

// Set whether columns may be reordered by the user via drag and drop. 
extern "C" CEGUI_LIB_EXPORT void cegui_lsthdr_setColumnDraggingEnabled(struct hg3dclass_struct * thisclass_c, int setting_c)
{
  CEGUI::ListHeader * thisclass_cpp = static_cast<CEGUI::ListHeader*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ListHeader"));
  bool setting_cpp = (bool)setting_c;
  (thisclass_cpp->setColumnDraggingEnabled(setting_cpp));
};

// Add a new column segment to the end of the header. 
extern "C" CEGUI_LIB_EXPORT void cegui_lsthdr_addColumn(struct hg3dclass_struct * thisclass_c, char * text_c, unsigned int id_c, struct hg3dclass_struct * width_c)
{
  CEGUI::ListHeader * thisclass_cpp = static_cast<CEGUI::ListHeader*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ListHeader"));
  CEGUI::String text_cpp = CEGUI::String((const char*) text_c);
  uint id_cpp = (uint)id_c;
  const CEGUI::UDim * width_cpp = static_cast<CEGUI::UDim*> (getHG3DClassPtr(*width_c, "CEGUI::UDim"));
  (thisclass_cpp->addColumn(text_cpp, id_cpp, *width_cpp));
};

// Insert a new column segment at the specified position. 
extern "C" CEGUI_LIB_EXPORT void cegui_lsthdr_insertColumn(struct hg3dclass_struct * thisclass_c, char * text_c, unsigned int id_c, struct hg3dclass_struct * width_c, unsigned int position_c)
{
  CEGUI::ListHeader * thisclass_cpp = static_cast<CEGUI::ListHeader*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ListHeader"));
  CEGUI::String text_cpp = CEGUI::String((const char*) text_c);
  uint id_cpp = (uint)id_c;
  const CEGUI::UDim * width_cpp = static_cast<CEGUI::UDim*> (getHG3DClassPtr(*width_c, "CEGUI::UDim"));
  uint position_cpp = (uint)position_c;
  (thisclass_cpp->insertColumn(text_cpp, id_cpp, *width_cpp, position_cpp));
};

// Insert a new column segment at the specified position. 
extern "C" CEGUI_LIB_EXPORT void cegui_lsthdr_insertColumn2(struct hg3dclass_struct * thisclass_c, char * text_c, unsigned int id_c, struct hg3dclass_struct * width_c, struct hg3dclass_struct * position_c)
{
  CEGUI::ListHeader * thisclass_cpp = static_cast<CEGUI::ListHeader*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ListHeader"));
  CEGUI::String text_cpp = CEGUI::String((const char*) text_c);
  uint id_cpp = (uint)id_c;
  const CEGUI::UDim * width_cpp = static_cast<CEGUI::UDim*> (getHG3DClassPtr(*width_c, "CEGUI::UDim"));
  const CEGUI::ListHeaderSegment * position_cpp = static_cast<CEGUI::ListHeaderSegment*> (getHG3DClassPtr(*position_c, "CEGUI::ListHeaderSegment"));
  (thisclass_cpp->insertColumn(text_cpp, id_cpp, *width_cpp, *position_cpp));
};

// Removes a column segment from the ListHeader
extern "C" CEGUI_LIB_EXPORT void cegui_lsthdr_removeColumn(struct hg3dclass_struct * thisclass_c, unsigned int column_c)
{
  CEGUI::ListHeader * thisclass_cpp = static_cast<CEGUI::ListHeader*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ListHeader"));
  uint column_cpp = (uint)column_c;
  (thisclass_cpp->removeColumn(column_cpp));
};

// Remove the specified segment from the ListHeader
extern "C" CEGUI_LIB_EXPORT void cegui_lsthdr_removeSegment(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * segment_c)
{
  CEGUI::ListHeader * thisclass_cpp = static_cast<CEGUI::ListHeader*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ListHeader"));
  const CEGUI::ListHeaderSegment * segment_cpp = static_cast<CEGUI::ListHeaderSegment*> (getHG3DClassPtr(*segment_c, "CEGUI::ListHeaderSegment"));
  (thisclass_cpp->removeSegment(*segment_cpp));
};

// Moves a column segment into a new position. 
extern "C" CEGUI_LIB_EXPORT void cegui_lsthdr_moveColumn(struct hg3dclass_struct * thisclass_c, unsigned int column_c, unsigned int position_c)
{
  CEGUI::ListHeader * thisclass_cpp = static_cast<CEGUI::ListHeader*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ListHeader"));
  uint column_cpp = (uint)column_c;
  uint position_cpp = (uint)position_c;
  (thisclass_cpp->moveColumn(column_cpp, position_cpp));
};

// Move a column segment to a new position. 
extern "C" CEGUI_LIB_EXPORT void cegui_lsthdr_moveColumn2(struct hg3dclass_struct * thisclass_c, unsigned int column_c, struct hg3dclass_struct * position_c)
{
  CEGUI::ListHeader * thisclass_cpp = static_cast<CEGUI::ListHeader*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ListHeader"));
  uint column_cpp = (uint)column_c;
  const CEGUI::ListHeaderSegment * position_cpp = static_cast<CEGUI::ListHeaderSegment*> (getHG3DClassPtr(*position_c, "CEGUI::ListHeaderSegment"));
  (thisclass_cpp->moveColumn(column_cpp, *position_cpp));
};

// Moves a segment into a new position. 
extern "C" CEGUI_LIB_EXPORT void cegui_lsthdr_moveSegment(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * segment_c, unsigned int position_c)
{
  CEGUI::ListHeader * thisclass_cpp = static_cast<CEGUI::ListHeader*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ListHeader"));
  const CEGUI::ListHeaderSegment * segment_cpp = static_cast<CEGUI::ListHeaderSegment*> (getHG3DClassPtr(*segment_c, "CEGUI::ListHeaderSegment"));
  uint position_cpp = (uint)position_c;
  (thisclass_cpp->moveSegment(*segment_cpp, position_cpp));
};

// Move a segment to a new position. 
extern "C" CEGUI_LIB_EXPORT void cegui_lsthdr_moveSegment2(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * segment_c, struct hg3dclass_struct * position_c)
{
  CEGUI::ListHeader * thisclass_cpp = static_cast<CEGUI::ListHeader*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ListHeader"));
  const CEGUI::ListHeaderSegment * segment_cpp = static_cast<CEGUI::ListHeaderSegment*> (getHG3DClassPtr(*segment_c, "CEGUI::ListHeaderSegment"));
  const CEGUI::ListHeaderSegment * position_cpp = static_cast<CEGUI::ListHeaderSegment*> (getHG3DClassPtr(*position_c, "CEGUI::ListHeaderSegment"));
  (thisclass_cpp->moveSegment(*segment_cpp, *position_cpp));
};

// Set the current base segment offset. (This implements scrolling of the header segments within the header area). 
extern "C" CEGUI_LIB_EXPORT void cegui_lsthdr_setSegmentOffset(struct hg3dclass_struct * thisclass_c, float offset_c)
{
  CEGUI::ListHeader * thisclass_cpp = static_cast<CEGUI::ListHeader*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ListHeader"));
  float offset_cpp = (float)offset_c;
  (thisclass_cpp->setSegmentOffset(offset_cpp));
};

// Set the width of the specified column. 
extern "C" CEGUI_LIB_EXPORT void cegui_lsthdr_setColumnWidth(struct hg3dclass_struct * thisclass_c, unsigned int column_c, struct hg3dclass_struct * width_c)
{
  CEGUI::ListHeader * thisclass_cpp = static_cast<CEGUI::ListHeader*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ListHeader"));
  uint column_cpp = (uint)column_c;
  const CEGUI::UDim * width_cpp = static_cast<CEGUI::UDim*> (getHG3DClassPtr(*width_c, "CEGUI::UDim"));
  (thisclass_cpp->setColumnWidth(column_cpp, *width_cpp));
};

// Constructor for the list header base class. 
extern "C" CEGUI_LIB_EXPORT void cegui_lsthdr_construct(char * type_c, char * name_c, struct hg3dclass_struct * result_c)
{
  CEGUI::String type_cpp = CEGUI::String((const char*) type_c);
  CEGUI::String name_cpp = CEGUI::String((const char*) name_c);
  CEGUI::ListHeader * result_cpp;
  result_cpp = (new CEGUI::ListHeader(type_cpp, name_cpp));
  *result_c = getHG3DClass_ListHeader((void *) result_cpp);
;
};

// Destructor for the list header base class. 
extern "C" CEGUI_LIB_EXPORT void cegui_lsthdr_destruct(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::ListHeader * thisclass_cpp = static_cast<CEGUI::ListHeader*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ListHeader"));
  (delete thisclass_cpp);
};

