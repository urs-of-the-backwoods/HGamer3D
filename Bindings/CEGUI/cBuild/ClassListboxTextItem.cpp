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

// ClassListboxTextItem.cpp

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



// base class destructor 
extern "C" CEGUI_LIB_EXPORT void cegui_lstbxti_destruct(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::ListboxTextItem * thisclass_cpp = static_cast<CEGUI::ListboxTextItem*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ListboxTextItem"));
  (delete thisclass_cpp);
};

// Return a pointer to the font being used by this ListboxTextItem
extern "C" CEGUI_LIB_EXPORT void cegui_lstbxti_getFont(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  CEGUI::ListboxTextItem * thisclass_cpp = static_cast<CEGUI::ListboxTextItem*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ListboxTextItem"));
  CEGUI::Font * result_cpp;
  result_cpp = (thisclass_cpp->getFont());
  *result_c = getHG3DClass_Font((void *) result_cpp);
;
};

// Set the font to be used by this ListboxTextItem
extern "C" CEGUI_LIB_EXPORT void cegui_lstbxti_setFont(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * font_c)
{
  CEGUI::ListboxTextItem * thisclass_cpp = static_cast<CEGUI::ListboxTextItem*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ListboxTextItem"));
  CEGUI::Font * font_cpp = static_cast<CEGUI::Font*> (getHG3DClassPtr(*font_c, "CEGUI::Font"));
  (thisclass_cpp->setFont(font_cpp));
};

// Set the font to be used by this ListboxTextItem
extern "C" CEGUI_LIB_EXPORT void cegui_lstbxti_setFont2(struct hg3dclass_struct * thisclass_c, char * font_name_c)
{
  CEGUI::ListboxTextItem * thisclass_cpp = static_cast<CEGUI::ListboxTextItem*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ListboxTextItem"));
  CEGUI::String font_name_cpp = CEGUI::String((const char*) font_name_c);
  (thisclass_cpp->setFont(font_name_cpp));
};

// Set whether the the ListboxTextItem
extern "C" CEGUI_LIB_EXPORT void cegui_lstbxti_setTextParsingEnabled(struct hg3dclass_struct * thisclass_c, const int enable_c)
{
  CEGUI::ListboxTextItem * thisclass_cpp = static_cast<CEGUI::ListboxTextItem*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ListboxTextItem"));
  const bool enable_cpp = (bool)enable_c;
  (thisclass_cpp->setTextParsingEnabled(enable_cpp));
};

// return whether text parsing is enabled for this ListboxTextItem
extern "C" CEGUI_LIB_EXPORT void cegui_lstbxti_isTextParsingEnabled(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::ListboxTextItem * thisclass_cpp = static_cast<CEGUI::ListboxTextItem*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ListboxTextItem"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isTextParsingEnabled());
  *result_c = (int)result_cpp;
};

// set the text string for this list box item. 
extern "C" CEGUI_LIB_EXPORT void cegui_lstbxti_setText(struct hg3dclass_struct * thisclass_c, char * text_c)
{
  CEGUI::ListboxTextItem * thisclass_cpp = static_cast<CEGUI::ListboxTextItem*> (getHG3DClassPtr(*thisclass_c, "CEGUI::ListboxTextItem"));
  CEGUI::String text_cpp = CEGUI::String((const char*) text_c);
  (thisclass_cpp->setText(text_cpp));
};

