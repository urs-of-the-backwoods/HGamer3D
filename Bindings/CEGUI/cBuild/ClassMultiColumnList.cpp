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

// ClassMultiColumnList.cpp

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
#include "EnumSelectionMode.h"
#include "./CEGUI.h"
#include "./CEGUIString.h"
#include "RendererModules/Ogre/CEGUIOgreRenderer.h"
#include "./WindowManagerHG3D.h"
#include "./SystemHG3D.h"
#include "HG3DCommandHandler.h"
#include "HG3DEventStaticFunctions.h"
#include "HG3DListboxStaticFunctions.h"
#include "HG3DEventController.h"
#include "HG3DEventModule.h"
#include "HG3DWindowStaticFunctions.h"

using namespace CEGUI;



// Return whether user manipulation of the sort column and direction are enabled. 
extern "C" CEGUI_LIB_EXPORT void cegui_mltclmlst_isUserSortControlEnabled(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::MultiColumnList * thisclass_cpp = static_cast<CEGUI::MultiColumnList*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiColumnList"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isUserSortControlEnabled());
  *result_c = (int)result_cpp;
};

// Return whether the user may size column segments. 
extern "C" CEGUI_LIB_EXPORT void cegui_mltclmlst_isUserColumnSizingEnabled(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::MultiColumnList * thisclass_cpp = static_cast<CEGUI::MultiColumnList*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiColumnList"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isUserColumnSizingEnabled());
  *result_c = (int)result_cpp;
};

// Return whether the user may modify the order of the columns. 
extern "C" CEGUI_LIB_EXPORT void cegui_mltclmlst_isUserColumnDraggingEnabled(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::MultiColumnList * thisclass_cpp = static_cast<CEGUI::MultiColumnList*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiColumnList"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isUserColumnDraggingEnabled());
  *result_c = (int)result_cpp;
};

// Return the number of columns in the multi-column list. 
extern "C" CEGUI_LIB_EXPORT void cegui_mltclmlst_getColumnCount(struct hg3dclass_struct * thisclass_c, unsigned int * result_c)
{
  CEGUI::MultiColumnList * thisclass_cpp = static_cast<CEGUI::MultiColumnList*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiColumnList"));
  uint result_cpp;
  result_cpp = (thisclass_cpp->getColumnCount());
  *result_c = (unsigned int)result_cpp;
};

// Return the number of rows in the multi-column list. 
extern "C" CEGUI_LIB_EXPORT void cegui_mltclmlst_getRowCount(struct hg3dclass_struct * thisclass_c, unsigned int * result_c)
{
  CEGUI::MultiColumnList * thisclass_cpp = static_cast<CEGUI::MultiColumnList*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiColumnList"));
  uint result_cpp;
  result_cpp = (thisclass_cpp->getRowCount());
  *result_c = (unsigned int)result_cpp;
};

// Return the zero based index of the current sort column. There must be at least one column to successfully call this method. 
extern "C" CEGUI_LIB_EXPORT void cegui_mltclmlst_getSortColumn(struct hg3dclass_struct * thisclass_c, unsigned int * result_c)
{
  CEGUI::MultiColumnList * thisclass_cpp = static_cast<CEGUI::MultiColumnList*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiColumnList"));
  uint result_cpp;
  result_cpp = (thisclass_cpp->getSortColumn());
  *result_c = (unsigned int)result_cpp;
};

// Return the zero based column index of the column with the specified ID. 
extern "C" CEGUI_LIB_EXPORT void cegui_mltclmlst_getColumnWithID(struct hg3dclass_struct * thisclass_c, unsigned int col_id_c, unsigned int * result_c)
{
  CEGUI::MultiColumnList * thisclass_cpp = static_cast<CEGUI::MultiColumnList*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiColumnList"));
  uint col_id_cpp = (uint)col_id_c;
  uint result_cpp;
  result_cpp = (thisclass_cpp->getColumnWithID(col_id_cpp));
  *result_c = (unsigned int)result_cpp;
};

// Return the zero based index of the column whos header text matches the specified text. 
extern "C" CEGUI_LIB_EXPORT void cegui_mltclmlst_getColumnWithHeaderText(struct hg3dclass_struct * thisclass_c, char * text_c, unsigned int * result_c)
{
  CEGUI::MultiColumnList * thisclass_cpp = static_cast<CEGUI::MultiColumnList*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiColumnList"));
  CEGUI::String text_cpp = CEGUI::String((const char*) text_c);
  uint result_cpp;
  result_cpp = (thisclass_cpp->getColumnWithHeaderText(text_cpp));
  *result_c = (unsigned int)result_cpp;
};

// Return the currently set sort direction. 
extern "C" CEGUI_LIB_EXPORT void cegui_mltclmlst_getSortDirection(struct hg3dclass_struct * thisclass_c, enum EnumSortDirection * result_c)
{
  CEGUI::MultiColumnList * thisclass_cpp = static_cast<CEGUI::MultiColumnList*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiColumnList"));
  enum ListHeaderSegment::SortDirection result_cpp;
  result_cpp = (thisclass_cpp->getSortDirection());
  *result_c = (enum EnumSortDirection) result_cpp;
};

// Return the ListHeaderSegment
extern "C" CEGUI_LIB_EXPORT void cegui_mltclmlst_getHeaderSegmentForColumn(struct hg3dclass_struct * thisclass_c, unsigned int col_idx_c, struct hg3dclass_struct * result_c)
{
  CEGUI::MultiColumnList * thisclass_cpp = static_cast<CEGUI::MultiColumnList*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiColumnList"));
  uint col_idx_cpp = (uint)col_idx_c;
  CEGUI::ListHeaderSegment * result_cpp;
  result_cpp = &(thisclass_cpp->getHeaderSegmentForColumn(col_idx_cpp));
  *result_c = getHG3DClass_ListHeaderSegment((void *) result_cpp);
;
};

// Return the zero based index of the Row that contains item
extern "C" CEGUI_LIB_EXPORT void cegui_mltclmlst_getItemRowIndex(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * item_c, unsigned int * result_c)
{
  CEGUI::MultiColumnList * thisclass_cpp = static_cast<CEGUI::MultiColumnList*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiColumnList"));
  const CEGUI::ListboxItem * item_cpp = static_cast<CEGUI::ListboxItem*> (getHG3DClassPtr(*item_c, "CEGUI::ListboxItem"));
  uint result_cpp;
  result_cpp = (thisclass_cpp->getItemRowIndex(item_cpp));
  *result_c = (unsigned int)result_cpp;
};

// Return the current zero based index of the column that contains item
extern "C" CEGUI_LIB_EXPORT void cegui_mltclmlst_getItemColumnIndex(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * item_c, unsigned int * result_c)
{
  CEGUI::MultiColumnList * thisclass_cpp = static_cast<CEGUI::MultiColumnList*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiColumnList"));
  const CEGUI::ListboxItem * item_cpp = static_cast<CEGUI::ListboxItem*> (getHG3DClassPtr(*item_c, "CEGUI::ListboxItem"));
  uint result_cpp;
  result_cpp = (thisclass_cpp->getItemColumnIndex(item_cpp));
  *result_c = (unsigned int)result_cpp;
};

// return whether ListboxItemitemcol_idx
extern "C" CEGUI_LIB_EXPORT void cegui_mltclmlst_isListboxItemInColumn(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * item_c, unsigned int col_idx_c, int * result_c)
{
  CEGUI::MultiColumnList * thisclass_cpp = static_cast<CEGUI::MultiColumnList*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiColumnList"));
  const CEGUI::ListboxItem * item_cpp = static_cast<CEGUI::ListboxItem*> (getHG3DClassPtr(*item_c, "CEGUI::ListboxItem"));
  uint col_idx_cpp = (uint)col_idx_c;
  bool result_cpp;
  result_cpp = (thisclass_cpp->isListboxItemInColumn(item_cpp, col_idx_cpp));
  *result_c = (int)result_cpp;
};

// return whether ListboxItemitemrow_idx
extern "C" CEGUI_LIB_EXPORT void cegui_mltclmlst_isListboxItemInRow(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * item_c, unsigned int row_idx_c, int * result_c)
{
  CEGUI::MultiColumnList * thisclass_cpp = static_cast<CEGUI::MultiColumnList*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiColumnList"));
  const CEGUI::ListboxItem * item_cpp = static_cast<CEGUI::ListboxItem*> (getHG3DClassPtr(*item_c, "CEGUI::ListboxItem"));
  uint row_idx_cpp = (uint)row_idx_c;
  bool result_cpp;
  result_cpp = (thisclass_cpp->isListboxItemInRow(item_cpp, row_idx_cpp));
  *result_c = (int)result_cpp;
};

// return whether ListboxItemitem
extern "C" CEGUI_LIB_EXPORT void cegui_mltclmlst_isListboxItemInList(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * item_c, int * result_c)
{
  CEGUI::MultiColumnList * thisclass_cpp = static_cast<CEGUI::MultiColumnList*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiColumnList"));
  const CEGUI::ListboxItem * item_cpp = static_cast<CEGUI::ListboxItem*> (getHG3DClassPtr(*item_c, "CEGUI::ListboxItem"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isListboxItemInList(item_cpp));
  *result_c = (int)result_cpp;
};

// Return the ListboxItemcol_idxtext
extern "C" CEGUI_LIB_EXPORT void cegui_mltclmlst_findColumnItemWithText(struct hg3dclass_struct * thisclass_c, char * text_c, unsigned int col_idx_c, struct hg3dclass_struct * start_item_c, struct hg3dclass_struct * result_c)
{
  CEGUI::MultiColumnList * thisclass_cpp = static_cast<CEGUI::MultiColumnList*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiColumnList"));
  CEGUI::String text_cpp = CEGUI::String((const char*) text_c);
  uint col_idx_cpp = (uint)col_idx_c;
  const CEGUI::ListboxItem * start_item_cpp = static_cast<CEGUI::ListboxItem*> (getHG3DClassPtr(*start_item_c, "CEGUI::ListboxItem"));
  CEGUI::ListboxItem * result_cpp;
  result_cpp = (thisclass_cpp->findColumnItemWithText(text_cpp, col_idx_cpp, start_item_cpp));
  *result_c = getHG3DClass_ListboxItem((void *) result_cpp);
;
};

// Return the ListboxItemrow_idxtext
extern "C" CEGUI_LIB_EXPORT void cegui_mltclmlst_findRowItemWithText(struct hg3dclass_struct * thisclass_c, char * text_c, unsigned int row_idx_c, struct hg3dclass_struct * start_item_c, struct hg3dclass_struct * result_c)
{
  CEGUI::MultiColumnList * thisclass_cpp = static_cast<CEGUI::MultiColumnList*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiColumnList"));
  CEGUI::String text_cpp = CEGUI::String((const char*) text_c);
  uint row_idx_cpp = (uint)row_idx_c;
  const CEGUI::ListboxItem * start_item_cpp = static_cast<CEGUI::ListboxItem*> (getHG3DClassPtr(*start_item_c, "CEGUI::ListboxItem"));
  CEGUI::ListboxItem * result_cpp;
  result_cpp = (thisclass_cpp->findRowItemWithText(text_cpp, row_idx_cpp, start_item_cpp));
  *result_c = getHG3DClass_ListboxItem((void *) result_cpp);
;
};

// Return the ListboxItemtext
extern "C" CEGUI_LIB_EXPORT void cegui_mltclmlst_findListItemWithText(struct hg3dclass_struct * thisclass_c, char * text_c, struct hg3dclass_struct * start_item_c, struct hg3dclass_struct * result_c)
{
  CEGUI::MultiColumnList * thisclass_cpp = static_cast<CEGUI::MultiColumnList*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiColumnList"));
  CEGUI::String text_cpp = CEGUI::String((const char*) text_c);
  const CEGUI::ListboxItem * start_item_cpp = static_cast<CEGUI::ListboxItem*> (getHG3DClassPtr(*start_item_c, "CEGUI::ListboxItem"));
  CEGUI::ListboxItem * result_cpp;
  result_cpp = (thisclass_cpp->findListItemWithText(text_cpp, start_item_cpp));
  *result_c = getHG3DClass_ListboxItem((void *) result_cpp);
;
};

// Return a pointer to the first selected ListboxItem
extern "C" CEGUI_LIB_EXPORT void cegui_mltclmlst_getFirstSelectedItem(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  CEGUI::MultiColumnList * thisclass_cpp = static_cast<CEGUI::MultiColumnList*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiColumnList"));
  CEGUI::ListboxItem * result_cpp;
  result_cpp = (thisclass_cpp->getFirstSelectedItem());
  *result_c = getHG3DClass_ListboxItem((void *) result_cpp);
;
};

// Return a pointer to the next selected ListboxItemstart_item
extern "C" CEGUI_LIB_EXPORT void cegui_mltclmlst_getNextSelected(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * start_item_c, struct hg3dclass_struct * result_c)
{
  CEGUI::MultiColumnList * thisclass_cpp = static_cast<CEGUI::MultiColumnList*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiColumnList"));
  const CEGUI::ListboxItem * start_item_cpp = static_cast<CEGUI::ListboxItem*> (getHG3DClassPtr(*start_item_c, "CEGUI::ListboxItem"));
  CEGUI::ListboxItem * result_cpp;
  result_cpp = (thisclass_cpp->getNextSelected(start_item_cpp));
  *result_c = getHG3DClass_ListboxItem((void *) result_cpp);
;
};

// Return the number of selected ListboxItems attached to this list box. 
extern "C" CEGUI_LIB_EXPORT void cegui_mltclmlst_getSelectedCount(struct hg3dclass_struct * thisclass_c, unsigned int * result_c)
{
  CEGUI::MultiColumnList * thisclass_cpp = static_cast<CEGUI::MultiColumnList*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiColumnList"));
  uint result_cpp;
  result_cpp = (thisclass_cpp->getSelectedCount());
  *result_c = (unsigned int)result_cpp;
};

// Return the ID of the currently set nominated selection column to be used when in one of the NominatedColumn* selection modes. There must be at least one column to successfully call this method. 
extern "C" CEGUI_LIB_EXPORT void cegui_mltclmlst_getNominatedSelectionColumnID(struct hg3dclass_struct * thisclass_c, unsigned int * result_c)
{
  CEGUI::MultiColumnList * thisclass_cpp = static_cast<CEGUI::MultiColumnList*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiColumnList"));
  uint result_cpp;
  result_cpp = (thisclass_cpp->getNominatedSelectionColumnID());
  *result_c = (unsigned int)result_cpp;
};

// Return the index of the currently set nominated selection column to be used when in one of the NominatedColumn* selection modes. 
extern "C" CEGUI_LIB_EXPORT void cegui_mltclmlst_getNominatedSelectionColumn(struct hg3dclass_struct * thisclass_c, unsigned int * result_c)
{
  CEGUI::MultiColumnList * thisclass_cpp = static_cast<CEGUI::MultiColumnList*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiColumnList"));
  uint result_cpp;
  result_cpp = (thisclass_cpp->getNominatedSelectionColumn());
  *result_c = (unsigned int)result_cpp;
};

// Return the index of the currently set nominated selection row to be used when in one of the NominatedRow* selection modes. 
extern "C" CEGUI_LIB_EXPORT void cegui_mltclmlst_getNominatedSelectionRow(struct hg3dclass_struct * thisclass_c, unsigned int * result_c)
{
  CEGUI::MultiColumnList * thisclass_cpp = static_cast<CEGUI::MultiColumnList*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiColumnList"));
  uint result_cpp;
  result_cpp = (thisclass_cpp->getNominatedSelectionRow());
  *result_c = (unsigned int)result_cpp;
};

// Return the currently set selection mode. 
extern "C" CEGUI_LIB_EXPORT void cegui_mltclmlst_getSelectionMode(struct hg3dclass_struct * thisclass_c, enum EnumSelectionMode * result_c)
{
  CEGUI::MultiColumnList * thisclass_cpp = static_cast<CEGUI::MultiColumnList*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiColumnList"));
  enum MultiColumnList::SelectionMode result_cpp;
  result_cpp = (thisclass_cpp->getSelectionMode());
  *result_c = (enum EnumSelectionMode) result_cpp;
};

// Return whether the vertical scroll bar is always shown. 
extern "C" CEGUI_LIB_EXPORT void cegui_mltclmlst_isVertScrollbarAlwaysShown(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::MultiColumnList * thisclass_cpp = static_cast<CEGUI::MultiColumnList*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiColumnList"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isVertScrollbarAlwaysShown());
  *result_c = (int)result_cpp;
};

// Return whether the horizontal scroll bar is always shown. 
extern "C" CEGUI_LIB_EXPORT void cegui_mltclmlst_isHorzScrollbarAlwaysShown(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::MultiColumnList * thisclass_cpp = static_cast<CEGUI::MultiColumnList*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiColumnList"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isHorzScrollbarAlwaysShown());
  *result_c = (int)result_cpp;
};

// Return the ID code assigned to the requested column. 
extern "C" CEGUI_LIB_EXPORT void cegui_mltclmlst_getColumnID(struct hg3dclass_struct * thisclass_c, unsigned int col_idx_c, unsigned int * result_c)
{
  CEGUI::MultiColumnList * thisclass_cpp = static_cast<CEGUI::MultiColumnList*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiColumnList"));
  uint col_idx_cpp = (uint)col_idx_c;
  uint result_cpp;
  result_cpp = (thisclass_cpp->getColumnID(col_idx_cpp));
  *result_c = (unsigned int)result_cpp;
};

// Return the ID code assigned to the requested row. 
extern "C" CEGUI_LIB_EXPORT void cegui_mltclmlst_getRowID(struct hg3dclass_struct * thisclass_c, unsigned int row_idx_c, unsigned int * result_c)
{
  CEGUI::MultiColumnList * thisclass_cpp = static_cast<CEGUI::MultiColumnList*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiColumnList"));
  uint row_idx_cpp = (uint)row_idx_c;
  uint result_cpp;
  result_cpp = (thisclass_cpp->getRowID(row_idx_cpp));
  *result_c = (unsigned int)result_cpp;
};

// Return the zero based row index of the row with the specified ID. 
extern "C" CEGUI_LIB_EXPORT void cegui_mltclmlst_getRowWithID(struct hg3dclass_struct * thisclass_c, unsigned int row_id_c, unsigned int * result_c)
{
  CEGUI::MultiColumnList * thisclass_cpp = static_cast<CEGUI::MultiColumnList*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiColumnList"));
  uint row_id_cpp = (uint)row_id_c;
  uint result_cpp;
  result_cpp = (thisclass_cpp->getRowWithID(row_id_cpp));
  *result_c = (unsigned int)result_cpp;
};

// Return a pointer to the vertical scrollbar component widget for this MultiColumnList
extern "C" CEGUI_LIB_EXPORT void cegui_mltclmlst_getVertScrollbar(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  CEGUI::MultiColumnList * thisclass_cpp = static_cast<CEGUI::MultiColumnList*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiColumnList"));
  CEGUI::Scrollbar * result_cpp;
  result_cpp = (thisclass_cpp->getVertScrollbar());
  *result_c = getHG3DClass_Scrollbar((void *) result_cpp);
;
};

// Return a pointer to the horizontal scrollbar component widget for this MultiColumnList
extern "C" CEGUI_LIB_EXPORT void cegui_mltclmlst_getHorzScrollbar(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  CEGUI::MultiColumnList * thisclass_cpp = static_cast<CEGUI::MultiColumnList*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiColumnList"));
  CEGUI::Scrollbar * result_cpp;
  result_cpp = (thisclass_cpp->getHorzScrollbar());
  *result_c = getHG3DClass_Scrollbar((void *) result_cpp);
;
};

// Return a pointer to the list header component widget for this MultiColumnList
extern "C" CEGUI_LIB_EXPORT void cegui_mltclmlst_getListHeader(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  CEGUI::MultiColumnList * thisclass_cpp = static_cast<CEGUI::MultiColumnList*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiColumnList"));
  CEGUI::ListHeader * result_cpp;
  result_cpp = (thisclass_cpp->getListHeader());
  *result_c = getHG3DClass_ListHeader((void *) result_cpp);
;
};

// Return the sum of all row heights in pixels. 
extern "C" CEGUI_LIB_EXPORT void cegui_mltclmlst_getTotalRowsHeight(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  CEGUI::MultiColumnList * thisclass_cpp = static_cast<CEGUI::MultiColumnList*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiColumnList"));
  float result_cpp;
  result_cpp = (thisclass_cpp->getTotalRowsHeight());
  *result_c = (float)result_cpp;
};

// Return the pixel width of the widest item in the given column. 
extern "C" CEGUI_LIB_EXPORT void cegui_mltclmlst_getWidestColumnItemWidth(struct hg3dclass_struct * thisclass_c, unsigned int col_idx_c, float * result_c)
{
  CEGUI::MultiColumnList * thisclass_cpp = static_cast<CEGUI::MultiColumnList*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiColumnList"));
  uint col_idx_cpp = (uint)col_idx_c;
  float result_cpp;
  result_cpp = (thisclass_cpp->getWidestColumnItemWidth(col_idx_cpp));
  *result_c = (float)result_cpp;
};

// Return, in pixels, the height of the highest item in the given row. 
extern "C" CEGUI_LIB_EXPORT void cegui_mltclmlst_getHighestRowItemHeight(struct hg3dclass_struct * thisclass_c, unsigned int row_idx_c, float * result_c)
{
  CEGUI::MultiColumnList * thisclass_cpp = static_cast<CEGUI::MultiColumnList*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiColumnList"));
  uint row_idx_cpp = (uint)row_idx_c;
  float result_cpp;
  result_cpp = (thisclass_cpp->getHighestRowItemHeight(row_idx_cpp));
  *result_c = (float)result_cpp;
};

// Initialise the Window
extern "C" CEGUI_LIB_EXPORT void cegui_mltclmlst_initialiseComponents(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::MultiColumnList * thisclass_cpp = static_cast<CEGUI::MultiColumnList*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiColumnList"));
  (thisclass_cpp->initialiseComponents());
};

// Remove all items from the list. 
extern "C" CEGUI_LIB_EXPORT void cegui_mltclmlst_resetList(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::MultiColumnList * thisclass_cpp = static_cast<CEGUI::MultiColumnList*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiColumnList"));
  (thisclass_cpp->resetList());
};

// Removes a column from the list box. This will cause any ListboxItem
extern "C" CEGUI_LIB_EXPORT void cegui_mltclmlst_removeColumn(struct hg3dclass_struct * thisclass_c, unsigned int col_idx_c)
{
  CEGUI::MultiColumnList * thisclass_cpp = static_cast<CEGUI::MultiColumnList*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiColumnList"));
  uint col_idx_cpp = (uint)col_idx_c;
  (thisclass_cpp->removeColumn(col_idx_cpp));
};

// Removes a column from the list box. This will cause any ListboxItem
extern "C" CEGUI_LIB_EXPORT void cegui_mltclmlst_removeColumnWithID(struct hg3dclass_struct * thisclass_c, unsigned int col_id_c)
{
  CEGUI::MultiColumnList * thisclass_cpp = static_cast<CEGUI::MultiColumnList*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiColumnList"));
  uint col_id_cpp = (uint)col_id_c;
  (thisclass_cpp->removeColumnWithID(col_id_cpp));
};

// Move the column at index col_idxposition
extern "C" CEGUI_LIB_EXPORT void cegui_mltclmlst_moveColumn(struct hg3dclass_struct * thisclass_c, unsigned int col_idx_c, unsigned int position_c)
{
  CEGUI::MultiColumnList * thisclass_cpp = static_cast<CEGUI::MultiColumnList*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiColumnList"));
  uint col_idx_cpp = (uint)col_idx_c;
  uint position_cpp = (uint)position_c;
  (thisclass_cpp->moveColumn(col_idx_cpp, position_cpp));
};

// Move the column with ID col_idposition
extern "C" CEGUI_LIB_EXPORT void cegui_mltclmlst_moveColumnWithID(struct hg3dclass_struct * thisclass_c, unsigned int col_id_c, unsigned int position_c)
{
  CEGUI::MultiColumnList * thisclass_cpp = static_cast<CEGUI::MultiColumnList*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiColumnList"));
  uint col_id_cpp = (uint)col_id_c;
  uint position_cpp = (uint)position_c;
  (thisclass_cpp->moveColumnWithID(col_id_cpp, position_cpp));
};

// Add an empty row to the list box. 
extern "C" CEGUI_LIB_EXPORT void cegui_mltclmlst_addRow(struct hg3dclass_struct * thisclass_c, unsigned int row_id_c, unsigned int * result_c)
{
  CEGUI::MultiColumnList * thisclass_cpp = static_cast<CEGUI::MultiColumnList*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiColumnList"));
  uint row_id_cpp = (uint)row_id_c;
  uint result_cpp;
  result_cpp = (thisclass_cpp->addRow(row_id_cpp));
  *result_c = (unsigned int)result_cpp;
};

// Add a row to the list box, and set the item in the column with ID col_iditem
extern "C" CEGUI_LIB_EXPORT void cegui_mltclmlst_addRow2(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * item_c, unsigned int col_id_c, unsigned int row_id_c, unsigned int * result_c)
{
  CEGUI::MultiColumnList * thisclass_cpp = static_cast<CEGUI::MultiColumnList*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiColumnList"));
  CEGUI::ListboxItem * item_cpp = static_cast<CEGUI::ListboxItem*> (getHG3DClassPtr(*item_c, "CEGUI::ListboxItem"));
  uint col_id_cpp = (uint)col_id_c;
  uint row_id_cpp = (uint)row_id_c;
  uint result_cpp;
  result_cpp = (thisclass_cpp->addRow(item_cpp, col_id_cpp, row_id_cpp));
  *result_c = (unsigned int)result_cpp;
};

// Insert an empty row into the list box. 
extern "C" CEGUI_LIB_EXPORT void cegui_mltclmlst_insertRow(struct hg3dclass_struct * thisclass_c, unsigned int row_idx_c, unsigned int row_id_c, unsigned int * result_c)
{
  CEGUI::MultiColumnList * thisclass_cpp = static_cast<CEGUI::MultiColumnList*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiColumnList"));
  uint row_idx_cpp = (uint)row_idx_c;
  uint row_id_cpp = (uint)row_id_c;
  uint result_cpp;
  result_cpp = (thisclass_cpp->insertRow(row_idx_cpp, row_id_cpp));
  *result_c = (unsigned int)result_cpp;
};

// Insert a row into the list box, and set the item in the column with ID col_iditem
extern "C" CEGUI_LIB_EXPORT void cegui_mltclmlst_insertRow2(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * item_c, unsigned int col_id_c, unsigned int row_idx_c, unsigned int row_id_c, unsigned int * result_c)
{
  CEGUI::MultiColumnList * thisclass_cpp = static_cast<CEGUI::MultiColumnList*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiColumnList"));
  CEGUI::ListboxItem * item_cpp = static_cast<CEGUI::ListboxItem*> (getHG3DClassPtr(*item_c, "CEGUI::ListboxItem"));
  uint col_id_cpp = (uint)col_id_c;
  uint row_idx_cpp = (uint)row_idx_c;
  uint row_id_cpp = (uint)row_id_c;
  uint result_cpp;
  result_cpp = (thisclass_cpp->insertRow(item_cpp, col_id_cpp, row_idx_cpp, row_id_cpp));
  *result_c = (unsigned int)result_cpp;
};

// Remove the list box row with index row_idxListboxItemrow_idx
extern "C" CEGUI_LIB_EXPORT void cegui_mltclmlst_removeRow(struct hg3dclass_struct * thisclass_c, unsigned int row_idx_c)
{
  CEGUI::MultiColumnList * thisclass_cpp = static_cast<CEGUI::MultiColumnList*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiColumnList"));
  uint row_idx_cpp = (uint)row_idx_c;
  (thisclass_cpp->removeRow(row_idx_cpp));
};

// Set the ListboxItemcol_idrow_idx
extern "C" CEGUI_LIB_EXPORT void cegui_mltclmlst_setItem2(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * item_c, unsigned int col_id_c, unsigned int row_idx_c)
{
  CEGUI::MultiColumnList * thisclass_cpp = static_cast<CEGUI::MultiColumnList*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiColumnList"));
  CEGUI::ListboxItem * item_cpp = static_cast<CEGUI::ListboxItem*> (getHG3DClassPtr(*item_c, "CEGUI::ListboxItem"));
  uint col_id_cpp = (uint)col_id_c;
  uint row_idx_cpp = (uint)row_idx_c;
  (thisclass_cpp->setItem(item_cpp, col_id_cpp, row_idx_cpp));
};

// Set the selection mode for the list box. 
extern "C" CEGUI_LIB_EXPORT void cegui_mltclmlst_setSelectionMode(struct hg3dclass_struct * thisclass_c, enum EnumSelectionMode sel_mode_c)
{
  CEGUI::MultiColumnList * thisclass_cpp = static_cast<CEGUI::MultiColumnList*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiColumnList"));
  enum MultiColumnList::SelectionMode sel_mode_cpp = (enum MultiColumnList::SelectionMode)sel_mode_c;
  (thisclass_cpp->setSelectionMode(sel_mode_cpp));
};

// Set the column to be used for the NominatedColumn* selection modes. 
extern "C" CEGUI_LIB_EXPORT void cegui_mltclmlst_setNominatedSelectionColumnID(struct hg3dclass_struct * thisclass_c, unsigned int col_id_c)
{
  CEGUI::MultiColumnList * thisclass_cpp = static_cast<CEGUI::MultiColumnList*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiColumnList"));
  uint col_id_cpp = (uint)col_id_c;
  (thisclass_cpp->setNominatedSelectionColumnID(col_id_cpp));
};

// Set the column to be used for the NominatedColumn* selection modes. 
extern "C" CEGUI_LIB_EXPORT void cegui_mltclmlst_setNominatedSelectionColumn(struct hg3dclass_struct * thisclass_c, unsigned int col_idx_c)
{
  CEGUI::MultiColumnList * thisclass_cpp = static_cast<CEGUI::MultiColumnList*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiColumnList"));
  uint col_idx_cpp = (uint)col_idx_c;
  (thisclass_cpp->setNominatedSelectionColumn(col_idx_cpp));
};

// Set the row to be used for the NominatedRow* selection modes. 
extern "C" CEGUI_LIB_EXPORT void cegui_mltclmlst_setNominatedSelectionRow(struct hg3dclass_struct * thisclass_c, unsigned int row_idx_c)
{
  CEGUI::MultiColumnList * thisclass_cpp = static_cast<CEGUI::MultiColumnList*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiColumnList"));
  uint row_idx_cpp = (uint)row_idx_c;
  (thisclass_cpp->setNominatedSelectionRow(row_idx_cpp));
};

// Set the sort direction to be used. 
extern "C" CEGUI_LIB_EXPORT void cegui_mltclmlst_setSortDirection(struct hg3dclass_struct * thisclass_c, enum EnumSortDirection direction_c)
{
  CEGUI::MultiColumnList * thisclass_cpp = static_cast<CEGUI::MultiColumnList*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiColumnList"));
  enum ListHeaderSegment::SortDirection direction_cpp = (enum ListHeaderSegment::SortDirection)direction_c;
  (thisclass_cpp->setSortDirection(direction_cpp));
};

// Set the column to be used as the sort key. 
extern "C" CEGUI_LIB_EXPORT void cegui_mltclmlst_setSortColumn(struct hg3dclass_struct * thisclass_c, unsigned int col_idx_c)
{
  CEGUI::MultiColumnList * thisclass_cpp = static_cast<CEGUI::MultiColumnList*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiColumnList"));
  uint col_idx_cpp = (uint)col_idx_c;
  (thisclass_cpp->setSortColumn(col_idx_cpp));
};

// Set the column to be used as the sort key. 
extern "C" CEGUI_LIB_EXPORT void cegui_mltclmlst_setSortColumnByID(struct hg3dclass_struct * thisclass_c, unsigned int col_id_c)
{
  CEGUI::MultiColumnList * thisclass_cpp = static_cast<CEGUI::MultiColumnList*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiColumnList"));
  uint col_id_cpp = (uint)col_id_c;
  (thisclass_cpp->setSortColumnByID(col_id_cpp));
};

// Set whether the vertical scroll bar should always be shown, or just when needed. 
extern "C" CEGUI_LIB_EXPORT void cegui_mltclmlst_setShowVertScrollbar(struct hg3dclass_struct * thisclass_c, int setting_c)
{
  CEGUI::MultiColumnList * thisclass_cpp = static_cast<CEGUI::MultiColumnList*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiColumnList"));
  bool setting_cpp = (bool)setting_c;
  (thisclass_cpp->setShowVertScrollbar(setting_cpp));
};

// Set whether the horizontal scroll bar should always be shown, or just when needed. 
extern "C" CEGUI_LIB_EXPORT void cegui_mltclmlst_setShowHorzScrollbar(struct hg3dclass_struct * thisclass_c, int setting_c)
{
  CEGUI::MultiColumnList * thisclass_cpp = static_cast<CEGUI::MultiColumnList*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiColumnList"));
  bool setting_cpp = (bool)setting_c;
  (thisclass_cpp->setShowHorzScrollbar(setting_cpp));
};

// Removed the selected state from any currently selected ListboxItem
extern "C" CEGUI_LIB_EXPORT void cegui_mltclmlst_clearAllSelections(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::MultiColumnList * thisclass_cpp = static_cast<CEGUI::MultiColumnList*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiColumnList"));
  (thisclass_cpp->clearAllSelections());
};

// Sets or clears the selected state of the given ListboxItem
extern "C" CEGUI_LIB_EXPORT void cegui_mltclmlst_setItemSelectState(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * item_c, int state_c)
{
  CEGUI::MultiColumnList * thisclass_cpp = static_cast<CEGUI::MultiColumnList*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiColumnList"));
  CEGUI::ListboxItem * item_cpp = static_cast<CEGUI::ListboxItem*> (getHG3DClassPtr(*item_c, "CEGUI::ListboxItem"));
  bool state_cpp = (bool)state_c;
  (thisclass_cpp->setItemSelectState(item_cpp, state_cpp));
};

// Inform the list box that one or more attached ListboxItems have been externally modified, and the list should re-sync its internal state and refresh the display as needed. 
extern "C" CEGUI_LIB_EXPORT void cegui_mltclmlst_handleUpdatedItemData(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::MultiColumnList * thisclass_cpp = static_cast<CEGUI::MultiColumnList*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiColumnList"));
  (thisclass_cpp->handleUpdatedItemData());
};

// Set whether user manipulation of the sort column and direction are enabled. 
extern "C" CEGUI_LIB_EXPORT void cegui_mltclmlst_setUserSortControlEnabled(struct hg3dclass_struct * thisclass_c, int setting_c)
{
  CEGUI::MultiColumnList * thisclass_cpp = static_cast<CEGUI::MultiColumnList*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiColumnList"));
  bool setting_cpp = (bool)setting_c;
  (thisclass_cpp->setUserSortControlEnabled(setting_cpp));
};

// Set whether the user may size column segments. 
extern "C" CEGUI_LIB_EXPORT void cegui_mltclmlst_setUserColumnSizingEnabled(struct hg3dclass_struct * thisclass_c, int setting_c)
{
  CEGUI::MultiColumnList * thisclass_cpp = static_cast<CEGUI::MultiColumnList*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiColumnList"));
  bool setting_cpp = (bool)setting_c;
  (thisclass_cpp->setUserColumnSizingEnabled(setting_cpp));
};

// Set whether the user may modify the order of the columns. 
extern "C" CEGUI_LIB_EXPORT void cegui_mltclmlst_setUserColumnDraggingEnabled(struct hg3dclass_struct * thisclass_c, int setting_c)
{
  CEGUI::MultiColumnList * thisclass_cpp = static_cast<CEGUI::MultiColumnList*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiColumnList"));
  bool setting_cpp = (bool)setting_c;
  (thisclass_cpp->setUserColumnDraggingEnabled(setting_cpp));
};

// Automatically determines the "best fit" size for the specified column and sets the column width to the same. 
extern "C" CEGUI_LIB_EXPORT void cegui_mltclmlst_autoSizeColumnHeader(struct hg3dclass_struct * thisclass_c, unsigned int col_idx_c)
{
  CEGUI::MultiColumnList * thisclass_cpp = static_cast<CEGUI::MultiColumnList*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiColumnList"));
  uint col_idx_cpp = (uint)col_idx_c;
  (thisclass_cpp->autoSizeColumnHeader(col_idx_cpp));
};

// Set the ID code assigned to a given row. 
extern "C" CEGUI_LIB_EXPORT void cegui_mltclmlst_setRowID(struct hg3dclass_struct * thisclass_c, unsigned int row_idx_c, unsigned int row_id_c)
{
  CEGUI::MultiColumnList * thisclass_cpp = static_cast<CEGUI::MultiColumnList*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiColumnList"));
  uint row_idx_cpp = (uint)row_idx_c;
  uint row_id_cpp = (uint)row_id_c;
  (thisclass_cpp->setRowID(row_idx_cpp, row_id_cpp));
};

// Constructor for the Multi-column list base class. 
extern "C" CEGUI_LIB_EXPORT void cegui_mltclmlst_construct(char * type_c, char * name_c, struct hg3dclass_struct * result_c)
{
  CEGUI::String type_cpp = CEGUI::String((const char*) type_c);
  CEGUI::String name_cpp = CEGUI::String((const char*) name_c);
  CEGUI::MultiColumnList * result_cpp;
  result_cpp = (new CEGUI::MultiColumnList(type_cpp, name_cpp));
  *result_c = getHG3DClass_MultiColumnList((void *) result_cpp);
;
};

// Destructor for the multi-column list base class. 
extern "C" CEGUI_LIB_EXPORT void cegui_mltclmlst_destruct(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::MultiColumnList * thisclass_cpp = static_cast<CEGUI::MultiColumnList*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiColumnList"));
  (delete thisclass_cpp);
};

