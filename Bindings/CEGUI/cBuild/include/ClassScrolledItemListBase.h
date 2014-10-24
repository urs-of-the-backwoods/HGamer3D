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

// ClassScrolledItemListBase.h

// abstrakte Klasse!

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassScrolledItemListBase
#define _DEFINED_HG3D_ClassScrolledItemListBase

#include "ClassPtr.h"
#include "ClassScrollbar.h"
#include "ClassItemEntry.h"


// Returns whether the vertical scrollbar is being forced visible. Despite content size. 
void cegui_scrlitmlstbs_isVertScrollbarAlwaysShown(struct hg3dclass_struct * thisclass_c, int * result_c);

// Returns whether the horizontal scrollbar is being forced visible. Despite content size. 
void cegui_scrlitmlstbs_isHorzScrollbarAlwaysShown(struct hg3dclass_struct * thisclass_c, int * result_c);

// Get the vertical scrollbar component attached to this window. 
void cegui_scrlitmlstbs_getVertScrollbar(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// Get the horizontal scrollbar component attached to this window. 
void cegui_scrlitmlstbs_getHorzScrollbar(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// Sets whether the vertical scrollbar should be forced visible. Despite content size. 
void cegui_scrlitmlstbs_setShowVertScrollbar(struct hg3dclass_struct * thisclass_c, int mode_c);

// Sets whether the horizontal scrollbar should be forced visible. Despite content size. 
void cegui_scrlitmlstbs_setShowHorzScrollbar(struct hg3dclass_struct * thisclass_c, int mode_c);

// Scroll the vertical list position if needed to ensure that the ItemEntryitemScrolledItemListBase
void cegui_scrlitmlstbs_ensureItemIsVisibleVert(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * item_c);

// Scroll the horizontal list position if needed to ensure that the ItemEntryitemScrolledItemListBase
void cegui_scrlitmlstbs_ensureItemIsVisibleHorz(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * item_c);

// 
void cegui_scrlitmlstbs_initialiseComponents(struct hg3dclass_struct * thisclass_c);

#endif 
