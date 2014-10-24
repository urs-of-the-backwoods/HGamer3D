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

// ClassConfigFile.cpp

// 

#include <wchar.h>
#include <string>
#include <iostream>

#include <iostream>
	#include <typeinfo>
	#include <stdio.h>
	#include <cstring>
	#include <exception>
	#include "OgreDllDefines.h"
	#include "ClassPtr.h"
	#include "./Ogre.h"
#include "./OgreString.h"
#include "./WindowUtilsHG3D.h"
#include "HG3DUtilities.h"

using namespace Ogre;
using Ogre::uint;



// 
extern "C" Ogre_LIB_EXPORT void ogre_cf_construct(struct hg3dclass_struct * result_c)
{
  Ogre::ConfigFile * result_cpp;
  result_cpp = (new Ogre::ConfigFile());
  *result_c = getHG3DClass_ConfigFile((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_cf_destruct(struct hg3dclass_struct * thisclass_c)
{
  Ogre::ConfigFile * thisclass_cpp = static_cast<Ogre::ConfigFile*> (getHG3DClassPtr(*thisclass_c, "Ogre::ConfigFile"));
  (delete thisclass_cpp);
};

// load from a filename (not using resource group locations) 
extern "C" Ogre_LIB_EXPORT void ogre_cf_load(struct hg3dclass_struct * thisclass_c, char * filename_c, char * separators_c, long trimWhitespace_c)
{
  Ogre::ConfigFile * thisclass_cpp = static_cast<Ogre::ConfigFile*> (getHG3DClassPtr(*thisclass_c, "Ogre::ConfigFile"));
  Ogre::String filename_cpp = Ogre::String((const char*) filename_c);
  Ogre::String separators_cpp = Ogre::String((const char*) separators_c);
  bool trimWhitespace_cpp = (bool)trimWhitespace_c;
  (thisclass_cpp->load(filename_cpp, separators_cpp, trimWhitespace_cpp));
};

// load from a filename (using resource group locations) 
extern "C" Ogre_LIB_EXPORT void ogre_cf_load2(struct hg3dclass_struct * thisclass_c, char * filename_c, char * resourceGroup_c, char * separators_c, long trimWhitespace_c)
{
  Ogre::ConfigFile * thisclass_cpp = static_cast<Ogre::ConfigFile*> (getHG3DClassPtr(*thisclass_c, "Ogre::ConfigFile"));
  Ogre::String filename_cpp = Ogre::String((const char*) filename_c);
  Ogre::String resourceGroup_cpp = Ogre::String((const char*) resourceGroup_c);
  Ogre::String separators_cpp = Ogre::String((const char*) separators_c);
  bool trimWhitespace_cpp = (bool)trimWhitespace_c;
  (thisclass_cpp->load(filename_cpp, resourceGroup_cpp, separators_cpp, trimWhitespace_cpp));
};

// load from a filename (not using resource group locations) 
extern "C" Ogre_LIB_EXPORT void ogre_cf_loadDirect(struct hg3dclass_struct * thisclass_c, char * filename_c, char * separators_c, long trimWhitespace_c)
{
  Ogre::ConfigFile * thisclass_cpp = static_cast<Ogre::ConfigFile*> (getHG3DClassPtr(*thisclass_c, "Ogre::ConfigFile"));
  Ogre::String filename_cpp = Ogre::String((const char*) filename_c);
  Ogre::String separators_cpp = Ogre::String((const char*) separators_c);
  bool trimWhitespace_cpp = (bool)trimWhitespace_c;
  (thisclass_cpp->loadDirect(filename_cpp, separators_cpp, trimWhitespace_cpp));
};

// load from a filename (using resource group locations) 
extern "C" Ogre_LIB_EXPORT void ogre_cf_loadFromResourceSystem(struct hg3dclass_struct * thisclass_c, char * filename_c, char * resourceGroup_c, char * separators_c, long trimWhitespace_c)
{
  Ogre::ConfigFile * thisclass_cpp = static_cast<Ogre::ConfigFile*> (getHG3DClassPtr(*thisclass_c, "Ogre::ConfigFile"));
  Ogre::String filename_cpp = Ogre::String((const char*) filename_c);
  Ogre::String resourceGroup_cpp = Ogre::String((const char*) resourceGroup_c);
  Ogre::String separators_cpp = Ogre::String((const char*) separators_c);
  bool trimWhitespace_cpp = (bool)trimWhitespace_c;
  (thisclass_cpp->loadFromResourceSystem(filename_cpp, resourceGroup_cpp, separators_cpp, trimWhitespace_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_cf_getSetting(struct hg3dclass_struct * thisclass_c, char * key_c, char * section_c, char * defaultValue_c, char * result_c)
{
  Ogre::ConfigFile * thisclass_cpp = static_cast<Ogre::ConfigFile*> (getHG3DClassPtr(*thisclass_c, "Ogre::ConfigFile"));
  Ogre::String key_cpp = Ogre::String((const char*) key_c);
  Ogre::String section_cpp = Ogre::String((const char*) section_c);
  Ogre::String defaultValue_cpp = Ogre::String((const char*) defaultValue_c);
  Ogre::String result_cpp;
  result_cpp = (thisclass_cpp->getSetting(key_cpp, section_cpp, defaultValue_cpp));
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_cf_clear(struct hg3dclass_struct * thisclass_c)
{
  Ogre::ConfigFile * thisclass_cpp = static_cast<Ogre::ConfigFile*> (getHG3DClassPtr(*thisclass_c, "Ogre::ConfigFile"));
  (thisclass_cpp->clear());
};

