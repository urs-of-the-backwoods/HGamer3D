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

// ClassMultiColumnList.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassMultiColumnList
#define _DEFINED_HG3D_ClassMultiColumnList

#include "ClassPtr.h"
#include "EnumSortDirection.h"
#include "ClassListHeaderSegment.h"
#include "ClassListboxItem.h"
#include "EnumSelectionMode.h"
#include "ClassScrollbar.h"
#include "ClassListHeader.h"


// Return whether user manipulation of the sort column and direction are enabled. 
void cegui_mltclmlst_isUserSortControlEnabled(struct hg3dclass_struct * thisclass_c, int * result_c);

// Return whether the user may size column segments. 
void cegui_mltclmlst_isUserColumnSizingEnabled(struct hg3dclass_struct * thisclass_c, int * result_c);

// Return whether the user may modify the order of the columns. 
void cegui_mltclmlst_isUserColumnDraggingEnabled(struct hg3dclass_struct * thisclass_c, int * result_c);

// Return the number of columns in the multi-column list. 
void cegui_mltclmlst_getColumnCount(struct hg3dclass_struct * thisclass_c, unsigned int * result_c);

// Return the number of rows in the multi-column list. 
void cegui_mltclmlst_getRowCount(struct hg3dclass_struct * thisclass_c, unsigned int * result_c);

// Return the zero based index of the current sort column. There must be at least one column to successfully call this method. 
void cegui_mltclmlst_getSortColumn(struct hg3dclass_struct * thisclass_c, unsigned int * result_c);

// Return the zero based column index of the column with the specified ID. 
void cegui_mltclmlst_getColumnWithID(struct hg3dclass_struct * thisclass_c, unsigned int col_id_c, unsigned int * result_c);

// Return the zero based index of the column whos header text matches the specified text. 
void cegui_mltclmlst_getColumnWithHeaderText(struct hg3dclass_struct * thisclass_c, char * text_c, unsigned int * result_c);

// Return the currently set sort direction. 
void cegui_mltclmlst_getSortDirection(struct hg3dclass_struct * thisclass_c, enum EnumSortDirection * result_c);

// Return the ListHeaderSegment
void cegui_mltclmlst_getHeaderSegmentForColumn(struct hg3dclass_struct * thisclass_c, unsigned int col_idx_c, struct hg3dclass_struct * result_c);

// Return the zero based index of the Row that contains item
void cegui_mltclmlst_getItemRowIndex(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * item_c, unsigned int * result_c);

// Return the current zero based index of the column that contains item
void cegui_mltclmlst_getItemColumnIndex(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * item_c, unsigned int * result_c);

// return whether ListboxItemitemcol_idx
void cegui_mltclmlst_isListboxItemInColumn(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * item_c, unsigned int col_idx_c, int * result_c);

// return whether ListboxItemitemrow_idx
void cegui_mltclmlst_isListboxItemInRow(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * item_c, unsigned int row_idx_c, int * result_c);

// return whether ListboxItemitem
void cegui_mltclmlst_isListboxItemInList(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * item_c, int * result_c);

// Return the ListboxItemcol_idxtext
void cegui_mltclmlst_findColumnItemWithText(struct hg3dclass_struct * thisclass_c, char * text_c, unsigned int col_idx_c, struct hg3dclass_struct * start_item_c, struct hg3dclass_struct * result_c);

// Return the ListboxItemrow_idxtext
void cegui_mltclmlst_findRowItemWithText(struct hg3dclass_struct * thisclass_c, char * text_c, unsigned int row_idx_c, struct hg3dclass_struct * start_item_c, struct hg3dclass_struct * result_c);

// Return the ListboxItemtext
void cegui_mltclmlst_findListItemWithText(struct hg3dclass_struct * thisclass_c, char * text_c, struct hg3dclass_struct * start_item_c, struct hg3dclass_struct * result_c);

// Return a pointer to the first selected ListboxItem
void cegui_mltclmlst_getFirstSelectedItem(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// Return a pointer to the next selected ListboxItemstart_item
void cegui_mltclmlst_getNextSelected(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * start_item_c, struct hg3dclass_struct * result_c);

// Return the number of selected ListboxItems attached to this list box. 
void cegui_mltclmlst_getSelectedCount(struct hg3dclass_struct * thisclass_c, unsigned int * result_c);

// Return the ID of the currently set nominated selection column to be used when in one of the NominatedColumn* selection modes. There must be at least one column to successfully call this method. 
void cegui_mltclmlst_getNominatedSelectionColumnID(struct hg3dclass_struct * thisclass_c, unsigned int * result_c);

// Return the index of the currently set nominated selection column to be used when in one of the NominatedColumn* selection modes. 
void cegui_mltclmlst_getNominatedSelectionColumn(struct hg3dclass_struct * thisclass_c, unsigned int * result_c);

// Return the index of the currently set nominated selection row to be used when in one of the NominatedRow* selection modes. 
void cegui_mltclmlst_getNominatedSelectionRow(struct hg3dclass_struct * thisclass_c, unsigned int * result_c);

// Return the currently set selection mode. 
void cegui_mltclmlst_getSelectionMode(struct hg3dclass_struct * thisclass_c, enum EnumSelectionMode * result_c);

// Return whether the vertical scroll bar is always shown. 
void cegui_mltclmlst_isVertScrollbarAlwaysShown(struct hg3dclass_struct * thisclass_c, int * result_c);

// Return whether the horizontal scroll bar is always shown. 
void cegui_mltclmlst_isHorzScrollbarAlwaysShown(struct hg3dclass_struct * thisclass_c, int * result_c);

// Return the ID code assigned to the requested column. 
void cegui_mltclmlst_getColumnID(struct hg3dclass_struct * thisclass_c, unsigned int col_idx_c, unsigned int * result_c);

// Return the ID code assigned to the requested row. 
void cegui_mltclmlst_getRowID(struct hg3dclass_struct * thisclass_c, unsigned int row_idx_c, unsigned int * result_c);

// Return the zero based row index of the row with the specified ID. 
void cegui_mltclmlst_getRowWithID(struct hg3dclass_struct * thisclass_c, unsigned int row_id_c, unsigned int * result_c);

// Return a pointer to the vertical scrollbar component widget for this MultiColumnList
void cegui_mltclmlst_getVertScrollbar(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// Return a pointer to the horizontal scrollbar component widget for this MultiColumnList
void cegui_mltclmlst_getHorzScrollbar(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// Return a pointer to the list header component widget for this MultiColumnList
void cegui_mltclmlst_getListHeader(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// Return the sum of all row heights in pixels. 
void cegui_mltclmlst_getTotalRowsHeight(struct hg3dclass_struct * thisclass_c, float * result_c);

// Return the pixel width of the widest item in the given column. 
void cegui_mltclmlst_getWidestColumnItemWidth(struct hg3dclass_struct * thisclass_c, unsigned int col_idx_c, float * result_c);

// Return, in pixels, the height of the highest item in the given row. 
void cegui_mltclmlst_getHighestRowItemHeight(struct hg3dclass_struct * thisclass_c, unsigned int row_idx_c, float * result_c);

// Initialise the Window
void cegui_mltclmlst_initialiseComponents(struct hg3dclass_struct * thisclass_c);

// Remove all items from the list. 
void cegui_mltclmlst_resetList(struct hg3dclass_struct * thisclass_c);

// Removes a column from the list box. This will cause any ListboxItem
void cegui_mltclmlst_removeColumn(struct hg3dclass_struct * thisclass_c, unsigned int col_idx_c);

// Removes a column from the list box. This will cause any ListboxItem
void cegui_mltclmlst_removeColumnWithID(struct hg3dclass_struct * thisclass_c, unsigned int col_id_c);

// Move the column at index col_idxposition
void cegui_mltclmlst_moveColumn(struct hg3dclass_struct * thisclass_c, unsigned int col_idx_c, unsigned int position_c);

// Move the column with ID col_idposition
void cegui_mltclmlst_moveColumnWithID(struct hg3dclass_struct * thisclass_c, unsigned int col_id_c, unsigned int position_c);

// Add an empty row to the list box. 
void cegui_mltclmlst_addRow(struct hg3dclass_struct * thisclass_c, unsigned int row_id_c, unsigned int * result_c);

// Add a row to the list box, and set the item in the column with ID col_iditem
void cegui_mltclmlst_addRow2(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * item_c, unsigned int col_id_c, unsigned int row_id_c, unsigned int * result_c);

// Insert an empty row into the list box. 
void cegui_mltclmlst_insertRow(struct hg3dclass_struct * thisclass_c, unsigned int row_idx_c, unsigned int row_id_c, unsigned int * result_c);

// Insert a row into the list box, and set the item in the column with ID col_iditem
void cegui_mltclmlst_insertRow2(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * item_c, unsigned int col_id_c, unsigned int row_idx_c, unsigned int row_id_c, unsigned int * result_c);

// Remove the list box row with index row_idxListboxItemrow_idx
void cegui_mltclmlst_removeRow(struct hg3dclass_struct * thisclass_c, unsigned int row_idx_c);

// Set the ListboxItemcol_idrow_idx
void cegui_mltclmlst_setItem2(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * item_c, unsigned int col_id_c, unsigned int row_idx_c);

// Set the selection mode for the list box. 
void cegui_mltclmlst_setSelectionMode(struct hg3dclass_struct * thisclass_c, enum EnumSelectionMode sel_mode_c);

// Set the column to be used for the NominatedColumn* selection modes. 
void cegui_mltclmlst_setNominatedSelectionColumnID(struct hg3dclass_struct * thisclass_c, unsigned int col_id_c);

// Set the column to be used for the NominatedColumn* selection modes. 
void cegui_mltclmlst_setNominatedSelectionColumn(struct hg3dclass_struct * thisclass_c, unsigned int col_idx_c);

// Set the row to be used for the NominatedRow* selection modes. 
void cegui_mltclmlst_setNominatedSelectionRow(struct hg3dclass_struct * thisclass_c, unsigned int row_idx_c);

// Set the sort direction to be used. 
void cegui_mltclmlst_setSortDirection(struct hg3dclass_struct * thisclass_c, enum EnumSortDirection direction_c);

// Set the column to be used as the sort key. 
void cegui_mltclmlst_setSortColumn(struct hg3dclass_struct * thisclass_c, unsigned int col_idx_c);

// Set the column to be used as the sort key. 
void cegui_mltclmlst_setSortColumnByID(struct hg3dclass_struct * thisclass_c, unsigned int col_id_c);

// Set whether the vertical scroll bar should always be shown, or just when needed. 
void cegui_mltclmlst_setShowVertScrollbar(struct hg3dclass_struct * thisclass_c, int setting_c);

// Set whether the horizontal scroll bar should always be shown, or just when needed. 
void cegui_mltclmlst_setShowHorzScrollbar(struct hg3dclass_struct * thisclass_c, int setting_c);

// Removed the selected state from any currently selected ListboxItem
void cegui_mltclmlst_clearAllSelections(struct hg3dclass_struct * thisclass_c);

// Sets or clears the selected state of the given ListboxItem
void cegui_mltclmlst_setItemSelectState(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * item_c, int state_c);

// Inform the list box that one or more attached ListboxItems have been externally modified, and the list should re-sync its internal state and refresh the display as needed. 
void cegui_mltclmlst_handleUpdatedItemData(struct hg3dclass_struct * thisclass_c);

// Set whether user manipulation of the sort column and direction are enabled. 
void cegui_mltclmlst_setUserSortControlEnabled(struct hg3dclass_struct * thisclass_c, int setting_c);

// Set whether the user may size column segments. 
void cegui_mltclmlst_setUserColumnSizingEnabled(struct hg3dclass_struct * thisclass_c, int setting_c);

// Set whether the user may modify the order of the columns. 
void cegui_mltclmlst_setUserColumnDraggingEnabled(struct hg3dclass_struct * thisclass_c, int setting_c);

// Automatically determines the "best fit" size for the specified column and sets the column width to the same. 
void cegui_mltclmlst_autoSizeColumnHeader(struct hg3dclass_struct * thisclass_c, unsigned int col_idx_c);

// Set the ID code assigned to a given row. 
void cegui_mltclmlst_setRowID(struct hg3dclass_struct * thisclass_c, unsigned int row_idx_c, unsigned int row_id_c);

// Constructor for the Multi-column list base class. 
void cegui_mltclmlst_construct(char * type_c, char * name_c, struct hg3dclass_struct * result_c);

// Destructor for the multi-column list base class. 
void cegui_mltclmlst_destruct(struct hg3dclass_struct * thisclass_c);

#endif 
