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

// ClassException.cpp

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



// 
extern "C" Ogre_LIB_EXPORT void ogre_exc_construct(int number_c, char * description_c, char * source_c, struct hg3dclass_struct * result_c)
{
  int number_cpp = (int)number_c;
  Ogre::String description_cpp = Ogre::String((const char*) description_c);
  Ogre::String source_cpp = Ogre::String((const char*) source_c);
  Ogre::Exception * result_cpp;
  result_cpp = (new Ogre::Exception(number_cpp, description_cpp, source_cpp));
  *result_c = getHG3DClass_Exception((void *) result_cpp);
;
};

// Needed for compatibility with std::exception. 
extern "C" Ogre_LIB_EXPORT void ogre_exc_destruct(struct hg3dclass_struct * thisclass_c)
{
  Ogre::Exception * thisclass_cpp = static_cast<Ogre::Exception*> (getHG3DClassPtr(*thisclass_c, "Ogre::Exception"));
  (delete thisclass_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_exc_getFullDescription(struct hg3dclass_struct * thisclass_c, char * result_c)
{
  Ogre::Exception * thisclass_cpp = static_cast<Ogre::Exception*> (getHG3DClassPtr(*thisclass_c, "Ogre::Exception"));
  Ogre::String result_cpp;
  result_cpp = (thisclass_cpp->getFullDescription());
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_exc_getNumber(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::Exception * thisclass_cpp = static_cast<Ogre::Exception*> (getHG3DClassPtr(*thisclass_c, "Ogre::Exception"));
  int result_cpp;
  result_cpp = (thisclass_cpp->getNumber());
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_exc_getSource(struct hg3dclass_struct * thisclass_c, char * result_c)
{
  Ogre::Exception * thisclass_cpp = static_cast<Ogre::Exception*> (getHG3DClassPtr(*thisclass_c, "Ogre::Exception"));
  Ogre::String result_cpp;
  result_cpp = (thisclass_cpp->getSource());
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_exc_getFile(struct hg3dclass_struct * thisclass_c, char * result_c)
{
  Ogre::Exception * thisclass_cpp = static_cast<Ogre::Exception*> (getHG3DClassPtr(*thisclass_c, "Ogre::Exception"));
  Ogre::String result_cpp;
  result_cpp = (thisclass_cpp->getFile());
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_exc_getLine(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::Exception * thisclass_cpp = static_cast<Ogre::Exception*> (getHG3DClassPtr(*thisclass_c, "Ogre::Exception"));
  long result_cpp;
  result_cpp = (thisclass_cpp->getLine());
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_exc_getDescription(struct hg3dclass_struct * thisclass_c, char * result_c)
{
  Ogre::Exception * thisclass_cpp = static_cast<Ogre::Exception*> (getHG3DClassPtr(*thisclass_c, "Ogre::Exception"));
  Ogre::String result_cpp;
  result_cpp = (thisclass_cpp->getDescription());
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

// Override std::exception::what. 
extern "C" Ogre_LIB_EXPORT void ogre_exc_what(struct hg3dclass_struct * thisclass_c, char * result_c)
{
  Ogre::Exception * thisclass_cpp = static_cast<Ogre::Exception*> (getHG3DClassPtr(*thisclass_c, "Ogre::Exception"));
  const char * result_cpp;
  result_cpp = (thisclass_cpp->what());
if (strlen( (char*) result_cpp) < (1024 * 64 - 1))  { 
strcpy(result_c, (char*) result_cpp); } else {
strcpy(result_c, "error: outstring larger then 64k");};
};

