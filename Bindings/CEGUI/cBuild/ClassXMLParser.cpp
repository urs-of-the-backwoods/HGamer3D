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

// ClassXMLParser.cpp

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



// XMLParser
extern "C" CEGUI_LIB_EXPORT void cegui_xmlprs_destruct(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::XMLParser * thisclass_cpp = static_cast<CEGUI::XMLParser*> (getHG3DClassPtr(*thisclass_c, "CEGUI::XMLParser"));
  (delete thisclass_cpp);
};

// Initialises the XMLParser
extern "C" CEGUI_LIB_EXPORT void cegui_xmlprs_initialise(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::XMLParser * thisclass_cpp = static_cast<CEGUI::XMLParser*> (getHG3DClassPtr(*thisclass_c, "CEGUI::XMLParser"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->initialise());
  *result_c = (int)result_cpp;
};

// Cleans up the XMLParser
extern "C" CEGUI_LIB_EXPORT void cegui_xmlprs_cleanup(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::XMLParser * thisclass_cpp = static_cast<CEGUI::XMLParser*> (getHG3DClassPtr(*thisclass_c, "CEGUI::XMLParser"));
  (thisclass_cpp->cleanup());
};

// Return identification string for the XML parser module. If the internal id string has not been set by the XML parser module creator, a generic string of "Unknown XML parser" will be returned. 
extern "C" CEGUI_LIB_EXPORT void cegui_xmlprs_getIdentifierString(struct hg3dclass_struct * thisclass_c, char * result_c)
{
  CEGUI::XMLParser * thisclass_cpp = static_cast<CEGUI::XMLParser*> (getHG3DClassPtr(*thisclass_c, "CEGUI::XMLParser"));
  CEGUI::String result_cpp;
  result_cpp = (thisclass_cpp->getIdentifierString());
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

