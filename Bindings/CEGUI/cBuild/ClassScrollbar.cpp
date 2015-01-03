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

// ClassScrollbar.cpp

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
#include "HG3DEventController.h"
#include "HG3DEventModule.h"
#include "HG3DEventStaticFunctions.h"
#include "HG3DListboxStaticFunctions.h"
#include "HG3DWindowStaticFunctions.h"

using namespace CEGUI;



// Return the size of the document or data. 
extern "C" CEGUI_LIB_EXPORT void cegui_scrlbr_getDocumentSize(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  CEGUI::Scrollbar * thisclass_cpp = static_cast<CEGUI::Scrollbar*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Scrollbar"));
  float result_cpp;
  result_cpp = (thisclass_cpp->getDocumentSize());
  *result_c = (float)result_cpp;
};

// Return the page size for this scroll bar. 
extern "C" CEGUI_LIB_EXPORT void cegui_scrlbr_getPageSize(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  CEGUI::Scrollbar * thisclass_cpp = static_cast<CEGUI::Scrollbar*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Scrollbar"));
  float result_cpp;
  result_cpp = (thisclass_cpp->getPageSize());
  *result_c = (float)result_cpp;
};

// Return the step size for this scroll bar. 
extern "C" CEGUI_LIB_EXPORT void cegui_scrlbr_getStepSize(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  CEGUI::Scrollbar * thisclass_cpp = static_cast<CEGUI::Scrollbar*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Scrollbar"));
  float result_cpp;
  result_cpp = (thisclass_cpp->getStepSize());
  *result_c = (float)result_cpp;
};

// Return the overlap size for this scroll bar. 
extern "C" CEGUI_LIB_EXPORT void cegui_scrlbr_getOverlapSize(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  CEGUI::Scrollbar * thisclass_cpp = static_cast<CEGUI::Scrollbar*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Scrollbar"));
  float result_cpp;
  result_cpp = (thisclass_cpp->getOverlapSize());
  *result_c = (float)result_cpp;
};

// Return the current position of scroll bar within the document. 
extern "C" CEGUI_LIB_EXPORT void cegui_scrlbr_getScrollPosition(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  CEGUI::Scrollbar * thisclass_cpp = static_cast<CEGUI::Scrollbar*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Scrollbar"));
  float result_cpp;
  result_cpp = (thisclass_cpp->getScrollPosition());
  *result_c = (float)result_cpp;
};

// Return a pointer to the 'increase' PushButtoncomponent widget for this Scrollbar
extern "C" CEGUI_LIB_EXPORT void cegui_scrlbr_getIncreaseButton(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  CEGUI::Scrollbar * thisclass_cpp = static_cast<CEGUI::Scrollbar*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Scrollbar"));
  CEGUI::PushButton * result_cpp;
  result_cpp = (thisclass_cpp->getIncreaseButton());
  *result_c = getHG3DClass_PushButton((void *) result_cpp);
;
};

// Return a pointer to the 'decrease' PushButtonScrollbar
extern "C" CEGUI_LIB_EXPORT void cegui_scrlbr_getDecreaseButton(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  CEGUI::Scrollbar * thisclass_cpp = static_cast<CEGUI::Scrollbar*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Scrollbar"));
  CEGUI::PushButton * result_cpp;
  result_cpp = (thisclass_cpp->getDecreaseButton());
  *result_c = getHG3DClass_PushButton((void *) result_cpp);
;
};

// Return a pointer to the ThumbScrollbar
extern "C" CEGUI_LIB_EXPORT void cegui_scrlbr_getThumb(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  CEGUI::Scrollbar * thisclass_cpp = static_cast<CEGUI::Scrollbar*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Scrollbar"));
  CEGUI::Thumb * result_cpp;
  result_cpp = (thisclass_cpp->getThumb());
  *result_c = getHG3DClass_Thumb((void *) result_cpp);
;
};

// Initialises the Scrollbar
extern "C" CEGUI_LIB_EXPORT void cegui_scrlbr_initialiseComponents(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::Scrollbar * thisclass_cpp = static_cast<CEGUI::Scrollbar*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Scrollbar"));
  (thisclass_cpp->initialiseComponents());
};

// Set the size of the document or data. 
extern "C" CEGUI_LIB_EXPORT void cegui_scrlbr_setDocumentSize(struct hg3dclass_struct * thisclass_c, float document_size_c)
{
  CEGUI::Scrollbar * thisclass_cpp = static_cast<CEGUI::Scrollbar*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Scrollbar"));
  float document_size_cpp = (float)document_size_c;
  (thisclass_cpp->setDocumentSize(document_size_cpp));
};

// Set the page size for this scroll bar. 
extern "C" CEGUI_LIB_EXPORT void cegui_scrlbr_setPageSize(struct hg3dclass_struct * thisclass_c, float page_size_c)
{
  CEGUI::Scrollbar * thisclass_cpp = static_cast<CEGUI::Scrollbar*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Scrollbar"));
  float page_size_cpp = (float)page_size_c;
  (thisclass_cpp->setPageSize(page_size_cpp));
};

// Set the step size for this scroll bar. 
extern "C" CEGUI_LIB_EXPORT void cegui_scrlbr_setStepSize(struct hg3dclass_struct * thisclass_c, float step_size_c)
{
  CEGUI::Scrollbar * thisclass_cpp = static_cast<CEGUI::Scrollbar*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Scrollbar"));
  float step_size_cpp = (float)step_size_c;
  (thisclass_cpp->setStepSize(step_size_cpp));
};

// Set the overlap size for this scroll bar. 
extern "C" CEGUI_LIB_EXPORT void cegui_scrlbr_setOverlapSize(struct hg3dclass_struct * thisclass_c, float overlap_size_c)
{
  CEGUI::Scrollbar * thisclass_cpp = static_cast<CEGUI::Scrollbar*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Scrollbar"));
  float overlap_size_cpp = (float)overlap_size_c;
  (thisclass_cpp->setOverlapSize(overlap_size_cpp));
};

// Set the current position of scroll bar within the document. 
extern "C" CEGUI_LIB_EXPORT void cegui_scrlbr_setScrollPosition(struct hg3dclass_struct * thisclass_c, float position_c)
{
  CEGUI::Scrollbar * thisclass_cpp = static_cast<CEGUI::Scrollbar*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Scrollbar"));
  float position_cpp = (float)position_c;
  (thisclass_cpp->setScrollPosition(position_cpp));
};

// Enable or disable the 'end lock' mode for the scrollbar. 
extern "C" CEGUI_LIB_EXPORT void cegui_scrlbr_setEndLockEnabled(struct hg3dclass_struct * thisclass_c, const int enabled_c)
{
  CEGUI::Scrollbar * thisclass_cpp = static_cast<CEGUI::Scrollbar*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Scrollbar"));
  const bool enabled_cpp = (bool)enabled_c;
  (thisclass_cpp->setEndLockEnabled(enabled_cpp));
};

// Returns whether the 'end lock'mode for the scrollbar is enabled. 
extern "C" CEGUI_LIB_EXPORT void cegui_scrlbr_isEndLockEnabled(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::Scrollbar * thisclass_cpp = static_cast<CEGUI::Scrollbar*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Scrollbar"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isEndLockEnabled());
  *result_c = (int)result_cpp;
};

// Constructor for Scrollbar
extern "C" CEGUI_LIB_EXPORT void cegui_scrlbr_construct(char * type_c, char * name_c, struct hg3dclass_struct * result_c)
{
  CEGUI::String type_cpp = CEGUI::String((const char*) type_c);
  CEGUI::String name_cpp = CEGUI::String((const char*) name_c);
  CEGUI::Scrollbar * result_cpp;
  result_cpp = (new CEGUI::Scrollbar(type_cpp, name_cpp));
  *result_c = getHG3DClass_Scrollbar((void *) result_cpp);
;
};

// Destructor for Scrollbar
extern "C" CEGUI_LIB_EXPORT void cegui_scrlbr_destruct(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::Scrollbar * thisclass_cpp = static_cast<CEGUI::Scrollbar*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Scrollbar"));
  (delete thisclass_cpp);
};

