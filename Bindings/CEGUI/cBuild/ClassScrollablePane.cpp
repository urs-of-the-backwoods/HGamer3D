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

// ClassScrollablePane.cpp

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



// Constructor for the ScrollablePane
extern "C" CEGUI_LIB_EXPORT void cegui_scrlpn_construct(char * type_c, char * name_c, struct hg3dclass_struct * result_c)
{
  CEGUI::String type_cpp = CEGUI::String((const char*) type_c);
  CEGUI::String name_cpp = CEGUI::String((const char*) name_c);
  CEGUI::ScrollablePane * result_cpp;
  result_cpp = (new CEGUI::ScrollablePane(type_cpp, name_cpp));
  *result_c = getHG3DClass_ScrollablePane((void *) result_cpp);
;
};

// Destructor for the ScrollablePane
extern "C" CEGUI_LIB_EXPORT void cegui_scrlpn_destruct(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::ScrollablePane * thisclass_cpp = static_cast<CEGUI::ScrollablePane*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ScrollablePane"));
  (delete thisclass_cpp);
};

// Returns a pointer to the window holding the pane contents. 
extern "C" CEGUI_LIB_EXPORT void cegui_scrlpn_getContentPane(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  CEGUI::ScrollablePane * thisclass_cpp = static_cast<CEGUI::ScrollablePane*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ScrollablePane"));
  const CEGUI::ScrolledContainer * result_cpp;
  result_cpp = (thisclass_cpp->getContentPane());
  *result_c = getHG3DClass_ScrolledContainer((void *) result_cpp);
;
};

// Return whether the vertical scroll bar is always shown. 
extern "C" CEGUI_LIB_EXPORT void cegui_scrlpn_isVertScrollbarAlwaysShown(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::ScrollablePane * thisclass_cpp = static_cast<CEGUI::ScrollablePane*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ScrollablePane"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isVertScrollbarAlwaysShown());
  *result_c = (int)result_cpp;
};

// Set whether the vertical scroll bar should always be shown. 
extern "C" CEGUI_LIB_EXPORT void cegui_scrlpn_setShowVertScrollbar(struct hg3dclass_struct * thisclass_c, int setting_c)
{
  CEGUI::ScrollablePane * thisclass_cpp = static_cast<CEGUI::ScrollablePane*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ScrollablePane"));
  bool setting_cpp = (bool)setting_c;
  (thisclass_cpp->setShowVertScrollbar(setting_cpp));
};

// Return whether the horizontal scroll bar is always shown. 
extern "C" CEGUI_LIB_EXPORT void cegui_scrlpn_isHorzScrollbarAlwaysShown(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::ScrollablePane * thisclass_cpp = static_cast<CEGUI::ScrollablePane*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ScrollablePane"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isHorzScrollbarAlwaysShown());
  *result_c = (int)result_cpp;
};

// Set whether the horizontal scroll bar should always be shown. 
extern "C" CEGUI_LIB_EXPORT void cegui_scrlpn_setShowHorzScrollbar(struct hg3dclass_struct * thisclass_c, int setting_c)
{
  CEGUI::ScrollablePane * thisclass_cpp = static_cast<CEGUI::ScrollablePane*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ScrollablePane"));
  bool setting_cpp = (bool)setting_c;
  (thisclass_cpp->setShowHorzScrollbar(setting_cpp));
};

// Return whether the content pane is auto sized. 
extern "C" CEGUI_LIB_EXPORT void cegui_scrlpn_isContentPaneAutoSized(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::ScrollablePane * thisclass_cpp = static_cast<CEGUI::ScrollablePane*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ScrollablePane"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isContentPaneAutoSized());
  *result_c = (int)result_cpp;
};

// Set whether the content pane should be auto-sized. 
extern "C" CEGUI_LIB_EXPORT void cegui_scrlpn_setContentPaneAutoSized(struct hg3dclass_struct * thisclass_c, int setting_c)
{
  CEGUI::ScrollablePane * thisclass_cpp = static_cast<CEGUI::ScrollablePane*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ScrollablePane"));
  bool setting_cpp = (bool)setting_c;
  (thisclass_cpp->setContentPaneAutoSized(setting_cpp));
};

// Returns the horizontal scrollbar step size as a fraction of one complete view page. 
extern "C" CEGUI_LIB_EXPORT void cegui_scrlpn_getHorizontalStepSize(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  CEGUI::ScrollablePane * thisclass_cpp = static_cast<CEGUI::ScrollablePane*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ScrollablePane"));
  float result_cpp;
  result_cpp = (thisclass_cpp->getHorizontalStepSize());
  *result_c = (float)result_cpp;
};

// Sets the horizontal scrollbar step size as a fraction of one complete view page. 
extern "C" CEGUI_LIB_EXPORT void cegui_scrlpn_setHorizontalStepSize(struct hg3dclass_struct * thisclass_c, float step_c)
{
  CEGUI::ScrollablePane * thisclass_cpp = static_cast<CEGUI::ScrollablePane*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ScrollablePane"));
  float step_cpp = (float)step_c;
  (thisclass_cpp->setHorizontalStepSize(step_cpp));
};

// Returns the horizontal scrollbar overlap size as a fraction of one complete view page. 
extern "C" CEGUI_LIB_EXPORT void cegui_scrlpn_getHorizontalOverlapSize(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  CEGUI::ScrollablePane * thisclass_cpp = static_cast<CEGUI::ScrollablePane*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ScrollablePane"));
  float result_cpp;
  result_cpp = (thisclass_cpp->getHorizontalOverlapSize());
  *result_c = (float)result_cpp;
};

// Sets the horizontal scrollbar overlap size as a fraction of one complete view page. 
extern "C" CEGUI_LIB_EXPORT void cegui_scrlpn_setHorizontalOverlapSize(struct hg3dclass_struct * thisclass_c, float overlap_c)
{
  CEGUI::ScrollablePane * thisclass_cpp = static_cast<CEGUI::ScrollablePane*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ScrollablePane"));
  float overlap_cpp = (float)overlap_c;
  (thisclass_cpp->setHorizontalOverlapSize(overlap_cpp));
};

// Returns the horizontal scroll position as a fraction of the complete scrollable width. 
extern "C" CEGUI_LIB_EXPORT void cegui_scrlpn_getHorizontalScrollPosition(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  CEGUI::ScrollablePane * thisclass_cpp = static_cast<CEGUI::ScrollablePane*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ScrollablePane"));
  float result_cpp;
  result_cpp = (thisclass_cpp->getHorizontalScrollPosition());
  *result_c = (float)result_cpp;
};

// Sets the horizontal scroll position as a fraction of the complete scrollable width. 
extern "C" CEGUI_LIB_EXPORT void cegui_scrlpn_setHorizontalScrollPosition(struct hg3dclass_struct * thisclass_c, float position_c)
{
  CEGUI::ScrollablePane * thisclass_cpp = static_cast<CEGUI::ScrollablePane*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ScrollablePane"));
  float position_cpp = (float)position_c;
  (thisclass_cpp->setHorizontalScrollPosition(position_cpp));
};

// Returns the vertical scrollbar step size as a fraction of one complete view page. 
extern "C" CEGUI_LIB_EXPORT void cegui_scrlpn_getVerticalStepSize(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  CEGUI::ScrollablePane * thisclass_cpp = static_cast<CEGUI::ScrollablePane*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ScrollablePane"));
  float result_cpp;
  result_cpp = (thisclass_cpp->getVerticalStepSize());
  *result_c = (float)result_cpp;
};

// Sets the vertical scrollbar step size as a fraction of one complete view page. 
extern "C" CEGUI_LIB_EXPORT void cegui_scrlpn_setVerticalStepSize(struct hg3dclass_struct * thisclass_c, float step_c)
{
  CEGUI::ScrollablePane * thisclass_cpp = static_cast<CEGUI::ScrollablePane*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ScrollablePane"));
  float step_cpp = (float)step_c;
  (thisclass_cpp->setVerticalStepSize(step_cpp));
};

// Returns the vertical scrollbar overlap size as a fraction of one complete view page. 
extern "C" CEGUI_LIB_EXPORT void cegui_scrlpn_getVerticalOverlapSize(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  CEGUI::ScrollablePane * thisclass_cpp = static_cast<CEGUI::ScrollablePane*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ScrollablePane"));
  float result_cpp;
  result_cpp = (thisclass_cpp->getVerticalOverlapSize());
  *result_c = (float)result_cpp;
};

// Sets the vertical scrollbar overlap size as a fraction of one complete view page. 
extern "C" CEGUI_LIB_EXPORT void cegui_scrlpn_setVerticalOverlapSize(struct hg3dclass_struct * thisclass_c, float overlap_c)
{
  CEGUI::ScrollablePane * thisclass_cpp = static_cast<CEGUI::ScrollablePane*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ScrollablePane"));
  float overlap_cpp = (float)overlap_c;
  (thisclass_cpp->setVerticalOverlapSize(overlap_cpp));
};

// Returns the vertical scroll position as a fraction of the complete scrollable height. 
extern "C" CEGUI_LIB_EXPORT void cegui_scrlpn_getVerticalScrollPosition(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  CEGUI::ScrollablePane * thisclass_cpp = static_cast<CEGUI::ScrollablePane*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ScrollablePane"));
  float result_cpp;
  result_cpp = (thisclass_cpp->getVerticalScrollPosition());
  *result_c = (float)result_cpp;
};

// Sets the vertical scroll position as a fraction of the complete scrollable height. 
extern "C" CEGUI_LIB_EXPORT void cegui_scrlpn_setVerticalScrollPosition(struct hg3dclass_struct * thisclass_c, float position_c)
{
  CEGUI::ScrollablePane * thisclass_cpp = static_cast<CEGUI::ScrollablePane*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ScrollablePane"));
  float position_cpp = (float)position_c;
  (thisclass_cpp->setVerticalScrollPosition(position_cpp));
};

// Return a pointer to the vertical scrollbar component widget for this ScrollablePane
extern "C" CEGUI_LIB_EXPORT void cegui_scrlpn_getVertScrollbar(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  CEGUI::ScrollablePane * thisclass_cpp = static_cast<CEGUI::ScrollablePane*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ScrollablePane"));
  CEGUI::Scrollbar * result_cpp;
  result_cpp = (thisclass_cpp->getVertScrollbar());
  *result_c = getHG3DClass_Scrollbar((void *) result_cpp);
;
};

// Return a pointer to the horizontal scrollbar component widget for this ScrollablePane
extern "C" CEGUI_LIB_EXPORT void cegui_scrlpn_getHorzScrollbar(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  CEGUI::ScrollablePane * thisclass_cpp = static_cast<CEGUI::ScrollablePane*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ScrollablePane"));
  CEGUI::Scrollbar * result_cpp;
  result_cpp = (thisclass_cpp->getHorzScrollbar());
  *result_c = getHG3DClass_Scrollbar((void *) result_cpp);
;
};

// Initialises the Window
extern "C" CEGUI_LIB_EXPORT void cegui_scrlpn_initialiseComponents(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::ScrollablePane * thisclass_cpp = static_cast<CEGUI::ScrollablePane*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ScrollablePane"));
  (thisclass_cpp->initialiseComponents());
};

// Internal destroy method which actually just adds the window and any parent destructed child windows to the dead pool. 
extern "C" CEGUI_LIB_EXPORT void cegui_scrlpn_destroy(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::ScrollablePane * thisclass_cpp = static_cast<CEGUI::ScrollablePane*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ScrollablePane"));
  (thisclass_cpp->destroy());
};

