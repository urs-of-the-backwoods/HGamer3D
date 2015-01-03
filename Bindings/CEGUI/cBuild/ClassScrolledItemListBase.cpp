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

// ClassScrolledItemListBase.cpp

// abstrakte Klasse!

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



// Returns whether the vertical scrollbar is being forced visible. Despite content size. 
extern "C" CEGUI_LIB_EXPORT void cegui_scrlitmlstbs_isVertScrollbarAlwaysShown(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::ScrolledItemListBase * thisclass_cpp = static_cast<CEGUI::ScrolledItemListBase*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ScrolledItemListBase"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isVertScrollbarAlwaysShown());
  *result_c = (int)result_cpp;
};

// Returns whether the horizontal scrollbar is being forced visible. Despite content size. 
extern "C" CEGUI_LIB_EXPORT void cegui_scrlitmlstbs_isHorzScrollbarAlwaysShown(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::ScrolledItemListBase * thisclass_cpp = static_cast<CEGUI::ScrolledItemListBase*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ScrolledItemListBase"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isHorzScrollbarAlwaysShown());
  *result_c = (int)result_cpp;
};

// Get the vertical scrollbar component attached to this window. 
extern "C" CEGUI_LIB_EXPORT void cegui_scrlitmlstbs_getVertScrollbar(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  CEGUI::ScrolledItemListBase * thisclass_cpp = static_cast<CEGUI::ScrolledItemListBase*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ScrolledItemListBase"));
  CEGUI::Scrollbar * result_cpp;
  result_cpp = (thisclass_cpp->getVertScrollbar());
  *result_c = getHG3DClass_Scrollbar((void *) result_cpp);
;
};

// Get the horizontal scrollbar component attached to this window. 
extern "C" CEGUI_LIB_EXPORT void cegui_scrlitmlstbs_getHorzScrollbar(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  CEGUI::ScrolledItemListBase * thisclass_cpp = static_cast<CEGUI::ScrolledItemListBase*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ScrolledItemListBase"));
  CEGUI::Scrollbar * result_cpp;
  result_cpp = (thisclass_cpp->getHorzScrollbar());
  *result_c = getHG3DClass_Scrollbar((void *) result_cpp);
;
};

// Sets whether the vertical scrollbar should be forced visible. Despite content size. 
extern "C" CEGUI_LIB_EXPORT void cegui_scrlitmlstbs_setShowVertScrollbar(struct hg3dclass_struct * thisclass_c, int mode_c)
{
  CEGUI::ScrolledItemListBase * thisclass_cpp = static_cast<CEGUI::ScrolledItemListBase*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ScrolledItemListBase"));
  bool mode_cpp = (bool)mode_c;
  (thisclass_cpp->setShowVertScrollbar(mode_cpp));
};

// Sets whether the horizontal scrollbar should be forced visible. Despite content size. 
extern "C" CEGUI_LIB_EXPORT void cegui_scrlitmlstbs_setShowHorzScrollbar(struct hg3dclass_struct * thisclass_c, int mode_c)
{
  CEGUI::ScrolledItemListBase * thisclass_cpp = static_cast<CEGUI::ScrolledItemListBase*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ScrolledItemListBase"));
  bool mode_cpp = (bool)mode_c;
  (thisclass_cpp->setShowHorzScrollbar(mode_cpp));
};

// Scroll the vertical list position if needed to ensure that the ItemEntryitemScrolledItemListBase
extern "C" CEGUI_LIB_EXPORT void cegui_scrlitmlstbs_ensureItemIsVisibleVert(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * item_c)
{
  CEGUI::ScrolledItemListBase * thisclass_cpp = static_cast<CEGUI::ScrolledItemListBase*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ScrolledItemListBase"));
  const CEGUI::ItemEntry * item_cpp = static_cast<CEGUI::ItemEntry*> (getHG3DClassPtr(*item_c, "CEGUI::ItemEntry"));
  (thisclass_cpp->ensureItemIsVisibleVert(*item_cpp));
};

// Scroll the horizontal list position if needed to ensure that the ItemEntryitemScrolledItemListBase
extern "C" CEGUI_LIB_EXPORT void cegui_scrlitmlstbs_ensureItemIsVisibleHorz(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * item_c)
{
  CEGUI::ScrolledItemListBase * thisclass_cpp = static_cast<CEGUI::ScrolledItemListBase*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ScrolledItemListBase"));
  const CEGUI::ItemEntry * item_cpp = static_cast<CEGUI::ItemEntry*> (getHG3DClassPtr(*item_c, "CEGUI::ItemEntry"));
  (thisclass_cpp->ensureItemIsVisibleHorz(*item_cpp));
};

// 
extern "C" CEGUI_LIB_EXPORT void cegui_scrlitmlstbs_initialiseComponents(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::ScrolledItemListBase * thisclass_cpp = static_cast<CEGUI::ScrolledItemListBase*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ScrolledItemListBase"));
  (thisclass_cpp->initialiseComponents());
};

