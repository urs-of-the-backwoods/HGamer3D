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

// ClassFont.cpp

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



// Destructor. 
extern "C" CEGUI_LIB_EXPORT void cegui_fnt_destruct(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::Font * thisclass_cpp = static_cast<CEGUI::Font*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Font"));
  (delete thisclass_cpp);
};

// Return the string holding the font name. 
extern "C" CEGUI_LIB_EXPORT void cegui_fnt_getName(struct hg3dclass_struct * thisclass_c, char * result_c)
{
  CEGUI::Font * thisclass_cpp = static_cast<CEGUI::Font*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Font"));
  CEGUI::String result_cpp;
  result_cpp = (thisclass_cpp->getName());
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

// Return the type of the font. 
extern "C" CEGUI_LIB_EXPORT void cegui_fnt_getTypeName(struct hg3dclass_struct * thisclass_c, char * result_c)
{
  CEGUI::Font * thisclass_cpp = static_cast<CEGUI::Font*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Font"));
  CEGUI::String result_cpp;
  result_cpp = (thisclass_cpp->getTypeName());
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

// Return whether this Font
extern "C" CEGUI_LIB_EXPORT void cegui_fnt_isCodepointAvailable(struct hg3dclass_struct * thisclass_c, int cp_c, int * result_c)
{
  CEGUI::Font * thisclass_cpp = static_cast<CEGUI::Font*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Font"));
  utf32 cp_cpp = (utf32)cp_c;
  bool result_cpp;
  result_cpp = (thisclass_cpp->isCodepointAvailable(cp_cpp));
  *result_c = (int)result_cpp;
};

// Enable or disable auto-scaling for this Font
extern "C" CEGUI_LIB_EXPORT void cegui_fnt_setAutoScaled(struct hg3dclass_struct * thisclass_c, const int auto_scaled_c)
{
  CEGUI::Font * thisclass_cpp = static_cast<CEGUI::Font*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Font"));
  const bool auto_scaled_cpp = (bool)auto_scaled_c;
  (thisclass_cpp->setAutoScaled(auto_scaled_cpp));
};

// Return whether this Font
extern "C" CEGUI_LIB_EXPORT void cegui_fnt_isAutoScaled(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::Font * thisclass_cpp = static_cast<CEGUI::Font*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Font"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isAutoScaled());
  *result_c = (int)result_cpp;
};

// Return the pixel line spacing value for. 
extern "C" CEGUI_LIB_EXPORT void cegui_fnt_getLineSpacing(struct hg3dclass_struct * thisclass_c, float y_scale_c, float * result_c)
{
  CEGUI::Font * thisclass_cpp = static_cast<CEGUI::Font*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Font"));
  float y_scale_cpp = (float)y_scale_c;
  float result_cpp;
  result_cpp = (thisclass_cpp->getLineSpacing(y_scale_cpp));
  *result_c = (float)result_cpp;
};

// return the exact pixel height of the font. 
extern "C" CEGUI_LIB_EXPORT void cegui_fnt_getFontHeight(struct hg3dclass_struct * thisclass_c, float y_scale_c, float * result_c)
{
  CEGUI::Font * thisclass_cpp = static_cast<CEGUI::Font*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Font"));
  float y_scale_cpp = (float)y_scale_c;
  float result_cpp;
  result_cpp = (thisclass_cpp->getFontHeight(y_scale_cpp));
  *result_c = (float)result_cpp;
};

// Return the number of pixels from the top of the highest glyph to the baseline. 
extern "C" CEGUI_LIB_EXPORT void cegui_fnt_getBaseline(struct hg3dclass_struct * thisclass_c, float y_scale_c, float * result_c)
{
  CEGUI::Font * thisclass_cpp = static_cast<CEGUI::Font*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Font"));
  float y_scale_cpp = (float)y_scale_c;
  float result_cpp;
  result_cpp = (thisclass_cpp->getBaseline(y_scale_cpp));
  *result_c = (float)result_cpp;
};

// Return the pixel width of the specified text if rendered with this Font
extern "C" CEGUI_LIB_EXPORT void cegui_fnt_getTextExtent(struct hg3dclass_struct * thisclass_c, char * text_c, float x_scale_c, float * result_c)
{
  CEGUI::Font * thisclass_cpp = static_cast<CEGUI::Font*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Font"));
  CEGUI::String text_cpp = CEGUI::String((const char*) text_c);
  float x_scale_cpp = (float)x_scale_c;
  float result_cpp;
  result_cpp = (thisclass_cpp->getTextExtent(text_cpp, x_scale_cpp));
  *result_c = (float)result_cpp;
};

// Return the index of the closest text character in String textpixel
extern "C" CEGUI_LIB_EXPORT void cegui_fnt_getCharAtPixel(struct hg3dclass_struct * thisclass_c, char * text_c, float pixel_c, float x_scale_c, int * result_c)
{
  CEGUI::Font * thisclass_cpp = static_cast<CEGUI::Font*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Font"));
  CEGUI::String text_cpp = CEGUI::String((const char*) text_c);
  float pixel_cpp = (float)pixel_c;
  float x_scale_cpp = (float)x_scale_c;
  size_t result_cpp;
  result_cpp = (thisclass_cpp->getCharAtPixel(text_cpp, pixel_cpp, x_scale_cpp));
  *result_c = (int)result_cpp;
};

// Return the index of the closest text character in String textstart_charpixel
extern "C" CEGUI_LIB_EXPORT void cegui_fnt_getCharAtPixel2(struct hg3dclass_struct * thisclass_c, char * text_c, int start_char_c, float pixel_c, float x_scale_c, int * result_c)
{
  CEGUI::Font * thisclass_cpp = static_cast<CEGUI::Font*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Font"));
  CEGUI::String text_cpp = CEGUI::String((const char*) text_c);
  size_t start_char_cpp = (size_t)start_char_c;
  float pixel_cpp = (float)pixel_c;
  float x_scale_cpp = (float)x_scale_c;
  size_t result_cpp;
  result_cpp = (thisclass_cpp->getCharAtPixel(text_cpp, start_char_cpp, pixel_cpp, x_scale_cpp));
  *result_c = (int)result_cpp;
};

// Sets the default resource group to be used when loading font data. 
extern "C" CEGUI_LIB_EXPORT void cegui_fnt_setDefaultResourceGroup(char * resourceGroup_c)
{
  CEGUI::String resourceGroup_cpp = CEGUI::String((const char*) resourceGroup_c);
  (CEGUI::Font::setDefaultResourceGroup(resourceGroup_cpp));
};

// Returns the default resource group currently set for Fonts. 
extern "C" CEGUI_LIB_EXPORT void cegui_fnt_getDefaultResourceGroup(char * result_c)
{
  CEGUI::String result_cpp;
  result_cpp = (CEGUI::Font::getDefaultResourceGroup());
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

