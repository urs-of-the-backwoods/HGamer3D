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

// ClassEditbox.cpp

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



// return true if the Editbox
extern "C" CEGUI_LIB_EXPORT void cegui_edtbx_hasInputFocus(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::Editbox * thisclass_cpp = static_cast<CEGUI::Editbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Editbox"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->hasInputFocus());
  *result_c = (int)result_cpp;
};

// return true if the Editbox
extern "C" CEGUI_LIB_EXPORT void cegui_edtbx_isReadOnly(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::Editbox * thisclass_cpp = static_cast<CEGUI::Editbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Editbox"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isReadOnly());
  *result_c = (int)result_cpp;
};

// return true if the text for the Editbox
extern "C" CEGUI_LIB_EXPORT void cegui_edtbx_isTextMasked(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::Editbox * thisclass_cpp = static_cast<CEGUI::Editbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Editbox"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isTextMasked());
  *result_c = (int)result_cpp;
};

// return true if the Editbox
extern "C" CEGUI_LIB_EXPORT void cegui_edtbx_isTextValid(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::Editbox * thisclass_cpp = static_cast<CEGUI::Editbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Editbox"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isTextValid());
  *result_c = (int)result_cpp;
};

// return the currently set validation string 
extern "C" CEGUI_LIB_EXPORT void cegui_edtbx_getValidationString(struct hg3dclass_struct * thisclass_c, char * result_c)
{
  CEGUI::Editbox * thisclass_cpp = static_cast<CEGUI::Editbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Editbox"));
  CEGUI::String result_cpp;
  result_cpp = (thisclass_cpp->getValidationString());
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

// return the current position of the carat. 
extern "C" CEGUI_LIB_EXPORT void cegui_edtbx_getCaratIndex(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::Editbox * thisclass_cpp = static_cast<CEGUI::Editbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Editbox"));
  size_t result_cpp;
  result_cpp = (thisclass_cpp->getCaratIndex());
  *result_c = (int)result_cpp;
};

// return the current selection start point. 
extern "C" CEGUI_LIB_EXPORT void cegui_edtbx_getSelectionStartIndex(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::Editbox * thisclass_cpp = static_cast<CEGUI::Editbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Editbox"));
  size_t result_cpp;
  result_cpp = (thisclass_cpp->getSelectionStartIndex());
  *result_c = (int)result_cpp;
};

// return the current selection end point. 
extern "C" CEGUI_LIB_EXPORT void cegui_edtbx_getSelectionEndIndex(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::Editbox * thisclass_cpp = static_cast<CEGUI::Editbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Editbox"));
  size_t result_cpp;
  result_cpp = (thisclass_cpp->getSelectionEndIndex());
  *result_c = (int)result_cpp;
};

// return the length of the current selection (in code points / characters). 
extern "C" CEGUI_LIB_EXPORT void cegui_edtbx_getSelectionLength(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::Editbox * thisclass_cpp = static_cast<CEGUI::Editbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Editbox"));
  size_t result_cpp;
  result_cpp = (thisclass_cpp->getSelectionLength());
  *result_c = (int)result_cpp;
};

// return the utf32 code point used when rendering masked text. 
extern "C" CEGUI_LIB_EXPORT void cegui_edtbx_getMaskCodePoint(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::Editbox * thisclass_cpp = static_cast<CEGUI::Editbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Editbox"));
  utf32 result_cpp;
  result_cpp = (thisclass_cpp->getMaskCodePoint());
  *result_c = (int)result_cpp;
};

// return the maximum text length set for this Editbox
extern "C" CEGUI_LIB_EXPORT void cegui_edtbx_getMaxTextLength(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::Editbox * thisclass_cpp = static_cast<CEGUI::Editbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Editbox"));
  size_t result_cpp;
  result_cpp = (thisclass_cpp->getMaxTextLength());
  *result_c = (int)result_cpp;
};

// Specify whether the Editbox
extern "C" CEGUI_LIB_EXPORT void cegui_edtbx_setReadOnly(struct hg3dclass_struct * thisclass_c, int setting_c)
{
  CEGUI::Editbox * thisclass_cpp = static_cast<CEGUI::Editbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Editbox"));
  bool setting_cpp = (bool)setting_c;
  (thisclass_cpp->setReadOnly(setting_cpp));
};

// Specify whether the text for the Editbox
extern "C" CEGUI_LIB_EXPORT void cegui_edtbx_setTextMasked(struct hg3dclass_struct * thisclass_c, int setting_c)
{
  CEGUI::Editbox * thisclass_cpp = static_cast<CEGUI::Editbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Editbox"));
  bool setting_cpp = (bool)setting_c;
  (thisclass_cpp->setTextMasked(setting_cpp));
};

// Set the text validation string. 
extern "C" CEGUI_LIB_EXPORT void cegui_edtbx_setValidationString(struct hg3dclass_struct * thisclass_c, char * validation_string_c)
{
  CEGUI::Editbox * thisclass_cpp = static_cast<CEGUI::Editbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Editbox"));
  CEGUI::String validation_string_cpp = CEGUI::String((const char*) validation_string_c);
  (thisclass_cpp->setValidationString(validation_string_cpp));
};

// Set the current position of the carat. 
extern "C" CEGUI_LIB_EXPORT void cegui_edtbx_setCaratIndex(struct hg3dclass_struct * thisclass_c, int carat_pos_c)
{
  CEGUI::Editbox * thisclass_cpp = static_cast<CEGUI::Editbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Editbox"));
  size_t carat_pos_cpp = (size_t)carat_pos_c;
  (thisclass_cpp->setCaratIndex(carat_pos_cpp));
};

// Define the current selection for the Editbox
extern "C" CEGUI_LIB_EXPORT void cegui_edtbx_setSelection(struct hg3dclass_struct * thisclass_c, int start_pos_c, int end_pos_c)
{
  CEGUI::Editbox * thisclass_cpp = static_cast<CEGUI::Editbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Editbox"));
  size_t start_pos_cpp = (size_t)start_pos_c;
  size_t end_pos_cpp = (size_t)end_pos_c;
  (thisclass_cpp->setSelection(start_pos_cpp, end_pos_cpp));
};

// set the utf32 code point used when rendering masked text. 
extern "C" CEGUI_LIB_EXPORT void cegui_edtbx_setMaskCodePoint(struct hg3dclass_struct * thisclass_c, int code_point_c)
{
  CEGUI::Editbox * thisclass_cpp = static_cast<CEGUI::Editbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Editbox"));
  utf32 code_point_cpp = (utf32)code_point_c;
  (thisclass_cpp->setMaskCodePoint(code_point_cpp));
};

// set the maximum text length for this Editbox
extern "C" CEGUI_LIB_EXPORT void cegui_edtbx_setMaxTextLength(struct hg3dclass_struct * thisclass_c, int max_len_c)
{
  CEGUI::Editbox * thisclass_cpp = static_cast<CEGUI::Editbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Editbox"));
  size_t max_len_cpp = (size_t)max_len_c;
  (thisclass_cpp->setMaxTextLength(max_len_cpp));
};

// Constructor for Editbox
extern "C" CEGUI_LIB_EXPORT void cegui_edtbx_construct(char * type_c, char * name_c, struct hg3dclass_struct * result_c)
{
  CEGUI::String type_cpp = CEGUI::String((const char*) type_c);
  CEGUI::String name_cpp = CEGUI::String((const char*) name_c);
  CEGUI::Editbox * result_cpp;
  result_cpp = (new CEGUI::Editbox(type_cpp, name_cpp));
  *result_c = getHG3DClass_Editbox((void *) result_cpp);
;
};

// Destructor for Editbox
extern "C" CEGUI_LIB_EXPORT void cegui_edtbx_destruct(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::Editbox * thisclass_cpp = static_cast<CEGUI::Editbox*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Editbox"));
  (delete thisclass_cpp);
};

