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

// ClassImageset.cpp

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



// Construct a new Imageset
extern "C" CEGUI_LIB_EXPORT void cegui_imgst_construct(char * name_c, char * filename_c, char * resourceGroup_c, struct hg3dclass_struct * result_c)
{
  CEGUI::String name_cpp = CEGUI::String((const char*) name_c);
  CEGUI::String filename_cpp = CEGUI::String((const char*) filename_c);
  CEGUI::String resourceGroup_cpp = CEGUI::String((const char*) resourceGroup_c);
  CEGUI::Imageset * result_cpp;
  result_cpp = (new CEGUI::Imageset(name_cpp, filename_cpp, resourceGroup_cpp));
  *result_c = getHG3DClass_Imageset((void *) result_cpp);
;
};

// Destroys Imageset
extern "C" CEGUI_LIB_EXPORT void cegui_imgst_destruct(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::Imageset * thisclass_cpp = static_cast<CEGUI::Imageset*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Imageset"));
  (delete thisclass_cpp);
};

// return String object holding the name of the Imageset
extern "C" CEGUI_LIB_EXPORT void cegui_imgst_getName(struct hg3dclass_struct * thisclass_c, char * result_c)
{
  CEGUI::Imageset * thisclass_cpp = static_cast<CEGUI::Imageset*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Imageset"));
  CEGUI::String result_cpp;
  result_cpp = (thisclass_cpp->getName());
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

// return number of images defined for this Imageset
extern "C" CEGUI_LIB_EXPORT void cegui_imgst_getImageCount(struct hg3dclass_struct * thisclass_c, unsigned int * result_c)
{
  CEGUI::Imageset * thisclass_cpp = static_cast<CEGUI::Imageset*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Imageset"));
  uint result_cpp;
  result_cpp = (thisclass_cpp->getImageCount());
  *result_c = (unsigned int)result_cpp;
};

// return true if an Image with the specified name exists. 
extern "C" CEGUI_LIB_EXPORT void cegui_imgst_isImageDefined(struct hg3dclass_struct * thisclass_c, char * name_c, int * result_c)
{
  CEGUI::Imageset * thisclass_cpp = static_cast<CEGUI::Imageset*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Imageset"));
  CEGUI::String name_cpp = CEGUI::String((const char*) name_c);
  bool result_cpp;
  result_cpp = (thisclass_cpp->isImageDefined(name_cpp));
  *result_c = (int)result_cpp;
};

// remove the definition for the Image with the specified name. If no such Image exists, nothing happens. 
extern "C" CEGUI_LIB_EXPORT void cegui_imgst_undefineImage(struct hg3dclass_struct * thisclass_c, char * name_c)
{
  CEGUI::Imageset * thisclass_cpp = static_cast<CEGUI::Imageset*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Imageset"));
  CEGUI::String name_cpp = CEGUI::String((const char*) name_c);
  (thisclass_cpp->undefineImage(name_cpp));
};

// Removes the definitions for all Image objects currently defined in the Imageset
extern "C" CEGUI_LIB_EXPORT void cegui_imgst_undefineAllImages(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::Imageset * thisclass_cpp = static_cast<CEGUI::Imageset*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Imageset"));
  (thisclass_cpp->undefineAllImages());
};

// return the width of the named image. 
extern "C" CEGUI_LIB_EXPORT void cegui_imgst_getImageWidth(struct hg3dclass_struct * thisclass_c, char * name_c, float * result_c)
{
  CEGUI::Imageset * thisclass_cpp = static_cast<CEGUI::Imageset*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Imageset"));
  CEGUI::String name_cpp = CEGUI::String((const char*) name_c);
  float result_cpp;
  result_cpp = (thisclass_cpp->getImageWidth(name_cpp));
  *result_c = (float)result_cpp;
};

// return the height of the named image. 
extern "C" CEGUI_LIB_EXPORT void cegui_imgst_getImageHeight(struct hg3dclass_struct * thisclass_c, char * name_c, float * result_c)
{
  CEGUI::Imageset * thisclass_cpp = static_cast<CEGUI::Imageset*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Imageset"));
  CEGUI::String name_cpp = CEGUI::String((const char*) name_c);
  float result_cpp;
  result_cpp = (thisclass_cpp->getImageHeight(name_cpp));
  *result_c = (float)result_cpp;
};

// return the x rendering offset for the named image. 
extern "C" CEGUI_LIB_EXPORT void cegui_imgst_getImageOffsetX(struct hg3dclass_struct * thisclass_c, char * name_c, float * result_c)
{
  CEGUI::Imageset * thisclass_cpp = static_cast<CEGUI::Imageset*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Imageset"));
  CEGUI::String name_cpp = CEGUI::String((const char*) name_c);
  float result_cpp;
  result_cpp = (thisclass_cpp->getImageOffsetX(name_cpp));
  *result_c = (float)result_cpp;
};

// return the y rendering offset for the named image. 
extern "C" CEGUI_LIB_EXPORT void cegui_imgst_getImageOffsetY(struct hg3dclass_struct * thisclass_c, char * name_c, float * result_c)
{
  CEGUI::Imageset * thisclass_cpp = static_cast<CEGUI::Imageset*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Imageset"));
  CEGUI::String name_cpp = CEGUI::String((const char*) name_c);
  float result_cpp;
  result_cpp = (thisclass_cpp->getImageOffsetY(name_cpp));
  *result_c = (float)result_cpp;
};

// Return whether this Imageset
extern "C" CEGUI_LIB_EXPORT void cegui_imgst_isAutoScaled(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::Imageset * thisclass_cpp = static_cast<CEGUI::Imageset*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Imageset"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isAutoScaled());
  *result_c = (int)result_cpp;
};

// Enable or disable auto-scaling for this Imageset
extern "C" CEGUI_LIB_EXPORT void cegui_imgst_setAutoScalingEnabled(struct hg3dclass_struct * thisclass_c, int setting_c)
{
  CEGUI::Imageset * thisclass_cpp = static_cast<CEGUI::Imageset*> (getHG3DClassPtr(*thisclass_c, "CEGUI::Imageset"));
  bool setting_cpp = (bool)setting_c;
  (thisclass_cpp->setAutoScalingEnabled(setting_cpp));
};

// Sets the default resource group to be used when loading imageset data. 
extern "C" CEGUI_LIB_EXPORT void cegui_imgst_setDefaultResourceGroup(char * resourceGroup_c)
{
  CEGUI::String resourceGroup_cpp = CEGUI::String((const char*) resourceGroup_c);
  (CEGUI::Imageset::setDefaultResourceGroup(resourceGroup_cpp));
};

// Returns the default resource group currently set for Imagesets. 
extern "C" CEGUI_LIB_EXPORT void cegui_imgst_getDefaultResourceGroup(char * result_c)
{
  CEGUI::String result_cpp;
  result_cpp = (CEGUI::Imageset::getDefaultResourceGroup());
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

