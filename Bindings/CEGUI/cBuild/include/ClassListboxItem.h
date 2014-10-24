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

// ClassListboxItem.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassListboxItem
#define _DEFINED_HG3D_ClassListboxItem

#include "ClassPtr.h"
#include "ClassWindow.h"


// base class destructor 
void cegui_lstbxitm_destruct(struct hg3dclass_struct * thisclass_c);

// return the text string set for this list box item. 
void cegui_lstbxitm_getTooltipText(struct hg3dclass_struct * thisclass_c, char * result_c);

// 
void cegui_lstbxitm_getText(struct hg3dclass_struct * thisclass_c, char * result_c);

// return text string with visual
void cegui_lstbxitm_getTextVisual(struct hg3dclass_struct * thisclass_c, char * result_c);

// Return the current ID assigned to this list box item. 
void cegui_lstbxitm_getID(struct hg3dclass_struct * thisclass_c, unsigned int * result_c);

// return whether this item is selected. 
void cegui_lstbxitm_isSelected(struct hg3dclass_struct * thisclass_c, int * result_c);

// return whether this item is disabled. 
void cegui_lstbxitm_isDisabled(struct hg3dclass_struct * thisclass_c, int * result_c);

// return whether this item will be automatically deleted when the list box it is attached to is destroyed, or when the item is removed from the list box. 
void cegui_lstbxitm_isAutoDeleted(struct hg3dclass_struct * thisclass_c, int * result_c);

// Get the owner window for this ListboxItem
void cegui_lstbxitm_getOwnerWindow(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// set the text string for this list box item. 
void cegui_lstbxitm_setText(struct hg3dclass_struct * thisclass_c, char * text_c);

// 
void cegui_lstbxitm_setTooltipText(struct hg3dclass_struct * thisclass_c, char * text_c);

// Set the ID assigned to this list box item. 
void cegui_lstbxitm_setID(struct hg3dclass_struct * thisclass_c, unsigned int item_id_c);

// set whether this item is selected. 
void cegui_lstbxitm_setSelected(struct hg3dclass_struct * thisclass_c, int setting_c);

// set whether this item is disabled. 
void cegui_lstbxitm_setDisabled(struct hg3dclass_struct * thisclass_c, int setting_c);

// Set whether this item will be automatically deleted when the list box it is attached to is destroyed, or when the item is removed from the list box. 
void cegui_lstbxitm_setAutoDeleted(struct hg3dclass_struct * thisclass_c, int setting_c);

// Set the owner window for this ListboxItem
void cegui_lstbxitm_setOwnerWindow(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * owner_c);

// Set the selection highlighting brush image. 
void cegui_lstbxitm_setSelectionBrushImage2(struct hg3dclass_struct * thisclass_c, char * imageset_c, char * image_c);

#endif 
