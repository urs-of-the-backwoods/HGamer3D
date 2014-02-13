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

// ClassMultiLineEditbox.cpp

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
#include "HG3DEventStaticFunctions.h"
#include "HG3DListboxStaticFunctions.h"
#include "HG3DEventController.h"
#include "HG3DCommandHandler.h"
#include "HG3DEventModule.h"
#include "HG3DWindowStaticFunctions.h"

using namespace CEGUI;



// return true if the edit box has input focus. 
extern "C" CEGUI_LIB_EXPORT void cegui_mltlnedtbx_hasInputFocus(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::MultiLineEditbox * thisclass_cpp = static_cast<CEGUI::MultiLineEditbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiLineEditbox"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->hasInputFocus());
  *result_c = (int)result_cpp;
};

// return true if the edit box is read-only. 
extern "C" CEGUI_LIB_EXPORT void cegui_mltlnedtbx_isReadOnly(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::MultiLineEditbox * thisclass_cpp = static_cast<CEGUI::MultiLineEditbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiLineEditbox"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isReadOnly());
  *result_c = (int)result_cpp;
};

// return the current position of the carat. 
extern "C" CEGUI_LIB_EXPORT void cegui_mltlnedtbx_getCaratIndex(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::MultiLineEditbox * thisclass_cpp = static_cast<CEGUI::MultiLineEditbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiLineEditbox"));
  size_t result_cpp;
  result_cpp = (thisclass_cpp->getCaratIndex());
  *result_c = (int)result_cpp;
};

// return the current selection start point. 
extern "C" CEGUI_LIB_EXPORT void cegui_mltlnedtbx_getSelectionStartIndex(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::MultiLineEditbox * thisclass_cpp = static_cast<CEGUI::MultiLineEditbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiLineEditbox"));
  size_t result_cpp;
  result_cpp = (thisclass_cpp->getSelectionStartIndex());
  *result_c = (int)result_cpp;
};

// return the current selection end point. 
extern "C" CEGUI_LIB_EXPORT void cegui_mltlnedtbx_getSelectionEndIndex(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::MultiLineEditbox * thisclass_cpp = static_cast<CEGUI::MultiLineEditbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiLineEditbox"));
  size_t result_cpp;
  result_cpp = (thisclass_cpp->getSelectionEndIndex());
  *result_c = (int)result_cpp;
};

// return the length of the current selection (in code points / characters). 
extern "C" CEGUI_LIB_EXPORT void cegui_mltlnedtbx_getSelectionLength(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::MultiLineEditbox * thisclass_cpp = static_cast<CEGUI::MultiLineEditbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiLineEditbox"));
  size_t result_cpp;
  result_cpp = (thisclass_cpp->getSelectionLength());
  *result_c = (int)result_cpp;
};

// return the maximum text length set for this edit box. 
extern "C" CEGUI_LIB_EXPORT void cegui_mltlnedtbx_getMaxTextLength(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::MultiLineEditbox * thisclass_cpp = static_cast<CEGUI::MultiLineEditbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiLineEditbox"));
  size_t result_cpp;
  result_cpp = (thisclass_cpp->getMaxTextLength());
  *result_c = (int)result_cpp;
};

// Return whether the text in the edit box will be word-wrapped. 
extern "C" CEGUI_LIB_EXPORT void cegui_mltlnedtbx_isWordWrapped(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::MultiLineEditbox * thisclass_cpp = static_cast<CEGUI::MultiLineEditbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiLineEditbox"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isWordWrapped());
  *result_c = (int)result_cpp;
};

// Return a pointer to the vertical scrollbar component widget for this MultiLineEditbox
extern "C" CEGUI_LIB_EXPORT void cegui_mltlnedtbx_getVertScrollbar(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  CEGUI::MultiLineEditbox * thisclass_cpp = static_cast<CEGUI::MultiLineEditbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiLineEditbox"));
  CEGUI::Scrollbar * result_cpp;
  result_cpp = (thisclass_cpp->getVertScrollbar());
  *result_c = getHG3DClass_Scrollbar((void *) result_cpp);
;
};

// Return whether the vertical scroll bar is always shown. 
extern "C" CEGUI_LIB_EXPORT void cegui_mltlnedtbx_isVertScrollbarAlwaysShown(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::MultiLineEditbox * thisclass_cpp = static_cast<CEGUI::MultiLineEditbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiLineEditbox"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isVertScrollbarAlwaysShown());
  *result_c = (int)result_cpp;
};

// Return a pointer to the horizontal scrollbar component widget for this MultiLineEditbox
extern "C" CEGUI_LIB_EXPORT void cegui_mltlnedtbx_getHorzScrollbar(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  CEGUI::MultiLineEditbox * thisclass_cpp = static_cast<CEGUI::MultiLineEditbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiLineEditbox"));
  CEGUI::Scrollbar * result_cpp;
  result_cpp = (thisclass_cpp->getHorzScrollbar());
  *result_c = getHG3DClass_Scrollbar((void *) result_cpp);
;
};

// Return the line number a given index falls on with the current formatting. Will return last line if index is out of range. 
extern "C" CEGUI_LIB_EXPORT void cegui_mltlnedtbx_getLineNumberFromIndex(struct hg3dclass_struct * thisclass_c, int index_c, int * result_c)
{
  CEGUI::MultiLineEditbox * thisclass_cpp = static_cast<CEGUI::MultiLineEditbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiLineEditbox"));
  size_t index_cpp = (size_t)index_c;
  size_t result_cpp;
  result_cpp = (thisclass_cpp->getLineNumberFromIndex(index_cpp));
  *result_c = (int)result_cpp;
};

// Initialise the Window
extern "C" CEGUI_LIB_EXPORT void cegui_mltlnedtbx_initialiseComponents(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::MultiLineEditbox * thisclass_cpp = static_cast<CEGUI::MultiLineEditbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiLineEditbox"));
  (thisclass_cpp->initialiseComponents());
};

// Specify whether the edit box is read-only. 
extern "C" CEGUI_LIB_EXPORT void cegui_mltlnedtbx_setReadOnly(struct hg3dclass_struct * thisclass_c, int setting_c)
{
  CEGUI::MultiLineEditbox * thisclass_cpp = static_cast<CEGUI::MultiLineEditbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiLineEditbox"));
  bool setting_cpp = (bool)setting_c;
  (thisclass_cpp->setReadOnly(setting_cpp));
};

// Set the current position of the carat. 
extern "C" CEGUI_LIB_EXPORT void cegui_mltlnedtbx_setCaratIndex(struct hg3dclass_struct * thisclass_c, int carat_pos_c)
{
  CEGUI::MultiLineEditbox * thisclass_cpp = static_cast<CEGUI::MultiLineEditbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiLineEditbox"));
  size_t carat_pos_cpp = (size_t)carat_pos_c;
  (thisclass_cpp->setCaratIndex(carat_pos_cpp));
};

// Define the current selection for the edit box. 
extern "C" CEGUI_LIB_EXPORT void cegui_mltlnedtbx_setSelection(struct hg3dclass_struct * thisclass_c, int start_pos_c, int end_pos_c)
{
  CEGUI::MultiLineEditbox * thisclass_cpp = static_cast<CEGUI::MultiLineEditbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiLineEditbox"));
  size_t start_pos_cpp = (size_t)start_pos_c;
  size_t end_pos_cpp = (size_t)end_pos_c;
  (thisclass_cpp->setSelection(start_pos_cpp, end_pos_cpp));
};

// set the maximum text length for this edit box. 
extern "C" CEGUI_LIB_EXPORT void cegui_mltlnedtbx_setMaxTextLength(struct hg3dclass_struct * thisclass_c, int max_len_c)
{
  CEGUI::MultiLineEditbox * thisclass_cpp = static_cast<CEGUI::MultiLineEditbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiLineEditbox"));
  size_t max_len_cpp = (size_t)max_len_c;
  (thisclass_cpp->setMaxTextLength(max_len_cpp));
};

// Scroll the view so that the current carat position is visible. 
extern "C" CEGUI_LIB_EXPORT void cegui_mltlnedtbx_ensureCaratIsVisible(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::MultiLineEditbox * thisclass_cpp = static_cast<CEGUI::MultiLineEditbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiLineEditbox"));
  (thisclass_cpp->ensureCaratIsVisible());
};

// Set whether the text will be word wrapped or not. 
extern "C" CEGUI_LIB_EXPORT void cegui_mltlnedtbx_setWordWrapping(struct hg3dclass_struct * thisclass_c, int setting_c)
{
  CEGUI::MultiLineEditbox * thisclass_cpp = static_cast<CEGUI::MultiLineEditbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiLineEditbox"));
  bool setting_cpp = (bool)setting_c;
  (thisclass_cpp->setWordWrapping(setting_cpp));
};

// Set whether the vertical scroll bar should always be shown. 
extern "C" CEGUI_LIB_EXPORT void cegui_mltlnedtbx_setShowVertScrollbar(struct hg3dclass_struct * thisclass_c, int setting_c)
{
  CEGUI::MultiLineEditbox * thisclass_cpp = static_cast<CEGUI::MultiLineEditbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiLineEditbox"));
  bool setting_cpp = (bool)setting_c;
  (thisclass_cpp->setShowVertScrollbar(setting_cpp));
};

// Constructor for the MultiLineEditbox
extern "C" CEGUI_LIB_EXPORT void cegui_mltlnedtbx_construct(char * type_c, char * name_c, struct hg3dclass_struct * result_c)
{
  CEGUI::String type_cpp = CEGUI::String((const char*) type_c);
  CEGUI::String name_cpp = CEGUI::String((const char*) name_c);
  CEGUI::MultiLineEditbox * result_cpp;
  result_cpp = (new CEGUI::MultiLineEditbox(type_cpp, name_cpp));
  *result_c = getHG3DClass_MultiLineEditbox((void *) result_cpp);
;
};

// Destructor for the MultiLineEditbox
extern "C" CEGUI_LIB_EXPORT void cegui_mltlnedtbx_destruct(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::MultiLineEditbox * thisclass_cpp = static_cast<CEGUI::MultiLineEditbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::MultiLineEditbox"));
  (delete thisclass_cpp);
};

