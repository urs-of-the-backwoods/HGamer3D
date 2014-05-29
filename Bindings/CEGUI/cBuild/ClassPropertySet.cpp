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

// ClassPropertySet.cpp

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



// Constructs a new PropertySet
extern "C" CEGUI_LIB_EXPORT void cegui_prpst_construct(struct hg3dclass_struct * result_c)
{
  CEGUI::PropertySet * result_cpp;
  result_cpp = (new CEGUI::PropertySet());
  *result_c = getHG3DClass_PropertySet((void *) result_cpp);
;
};

// Destructor for PropertySet
extern "C" CEGUI_LIB_EXPORT void cegui_prpst_destruct(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::PropertySet * thisclass_cpp = static_cast<CEGUI::PropertySet*> (getHG3DClassPtr(*thisclass_c, "CEGUI::PropertySet"));
  (delete thisclass_cpp);
};

// Removes a Property from the PropertySet
extern "C" CEGUI_LIB_EXPORT void cegui_prpst_removeProperty(struct hg3dclass_struct * thisclass_c, char * name_c)
{
  CEGUI::PropertySet * thisclass_cpp = static_cast<CEGUI::PropertySet*> (getHG3DClassPtr(*thisclass_c, "CEGUI::PropertySet"));
  CEGUI::String name_cpp = CEGUI::String((const char*) name_c);
  (thisclass_cpp->removeProperty(name_cpp));
};

// Removes all Property objects from the PropertySet
extern "C" CEGUI_LIB_EXPORT void cegui_prpst_clearProperties(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::PropertySet * thisclass_cpp = static_cast<CEGUI::PropertySet*> (getHG3DClassPtr(*thisclass_c, "CEGUI::PropertySet"));
  (thisclass_cpp->clearProperties());
};

// Checks to see if a Property with the given name is in the PropertySet
extern "C" CEGUI_LIB_EXPORT void cegui_prpst_isPropertyPresent(struct hg3dclass_struct * thisclass_c, char * name_c, int * result_c)
{
  CEGUI::PropertySet * thisclass_cpp = static_cast<CEGUI::PropertySet*> (getHG3DClassPtr(*thisclass_c, "CEGUI::PropertySet"));
  CEGUI::String name_cpp = CEGUI::String((const char*) name_c);
  bool result_cpp;
  result_cpp = (thisclass_cpp->isPropertyPresent(name_cpp));
  *result_c = (int)result_cpp;
};

// Return the help text for the specified Property. 
extern "C" CEGUI_LIB_EXPORT void cegui_prpst_getPropertyHelp(struct hg3dclass_struct * thisclass_c, char * name_c, char * result_c)
{
  CEGUI::PropertySet * thisclass_cpp = static_cast<CEGUI::PropertySet*> (getHG3DClassPtr(*thisclass_c, "CEGUI::PropertySet"));
  CEGUI::String name_cpp = CEGUI::String((const char*) name_c);
  CEGUI::String result_cpp;
  result_cpp = (thisclass_cpp->getPropertyHelp(name_cpp));
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

// Gets the current value of the specified Property. 
extern "C" CEGUI_LIB_EXPORT void cegui_prpst_getProperty(struct hg3dclass_struct * thisclass_c, char * name_c, char * result_c)
{
  CEGUI::PropertySet * thisclass_cpp = static_cast<CEGUI::PropertySet*> (getHG3DClassPtr(*thisclass_c, "CEGUI::PropertySet"));
  CEGUI::String name_cpp = CEGUI::String((const char*) name_c);
  CEGUI::String result_cpp;
  result_cpp = (thisclass_cpp->getProperty(name_cpp));
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

// Sets the current value of a Property. 
extern "C" CEGUI_LIB_EXPORT void cegui_prpst_setProperty(struct hg3dclass_struct * thisclass_c, char * name_c, char * value_c)
{
  CEGUI::PropertySet * thisclass_cpp = static_cast<CEGUI::PropertySet*> (getHG3DClassPtr(*thisclass_c, "CEGUI::PropertySet"));
  CEGUI::String name_cpp = CEGUI::String((const char*) name_c);
  CEGUI::String value_cpp = CEGUI::String((const char*) value_c);
  (thisclass_cpp->setProperty(name_cpp, value_cpp));
};

// Returns whether a Property is at it's default value. 
extern "C" CEGUI_LIB_EXPORT void cegui_prpst_isPropertyDefault(struct hg3dclass_struct * thisclass_c, char * name_c, int * result_c)
{
  CEGUI::PropertySet * thisclass_cpp = static_cast<CEGUI::PropertySet*> (getHG3DClassPtr(*thisclass_c, "CEGUI::PropertySet"));
  CEGUI::String name_cpp = CEGUI::String((const char*) name_c);
  bool result_cpp;
  result_cpp = (thisclass_cpp->isPropertyDefault(name_cpp));
  *result_c = (int)result_cpp;
};

// Returns the default value of a Property as a String. 
extern "C" CEGUI_LIB_EXPORT void cegui_prpst_getPropertyDefault(struct hg3dclass_struct * thisclass_c, char * name_c, char * result_c)
{
  CEGUI::PropertySet * thisclass_cpp = static_cast<CEGUI::PropertySet*> (getHG3DClassPtr(*thisclass_c, "CEGUI::PropertySet"));
  CEGUI::String name_cpp = CEGUI::String((const char*) name_c);
  CEGUI::String result_cpp;
  result_cpp = (thisclass_cpp->getPropertyDefault(name_cpp));
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

