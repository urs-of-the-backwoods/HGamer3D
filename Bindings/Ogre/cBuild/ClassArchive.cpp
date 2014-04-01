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

// ClassArchive.cpp

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
extern "C" Ogre_LIB_EXPORT void ogre_arch_destruct(struct hg3dclass_struct * thisclass_c)
{
  Ogre::Archive * thisclass_cpp = static_cast<Ogre::Archive*> (getHG3DClassPtr(*thisclass_c, "Ogre::Archive"));
  (delete thisclass_cpp);
};

// Get the name of this archive. 
extern "C" Ogre_LIB_EXPORT void ogre_arch_getName(struct hg3dclass_struct * thisclass_c, char * result_c)
{
  Ogre::Archive * thisclass_cpp = static_cast<Ogre::Archive*> (getHG3DClassPtr(*thisclass_c, "Ogre::Archive"));
  Ogre::String result_cpp;
  result_cpp = (thisclass_cpp->getName());
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

// Returns whether this archive is case sensitive in the way it matches files. 
extern "C" Ogre_LIB_EXPORT void ogre_arch_isCaseSensitive(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::Archive * thisclass_cpp = static_cast<Ogre::Archive*> (getHG3DClassPtr(*thisclass_c, "Ogre::Archive"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isCaseSensitive());
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_arch_load(struct hg3dclass_struct * thisclass_c)
{
  Ogre::Archive * thisclass_cpp = static_cast<Ogre::Archive*> (getHG3DClassPtr(*thisclass_c, "Ogre::Archive"));
  (thisclass_cpp->load());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_arch_unload(struct hg3dclass_struct * thisclass_c)
{
  Ogre::Archive * thisclass_cpp = static_cast<Ogre::Archive*> (getHG3DClassPtr(*thisclass_c, "Ogre::Archive"));
  (thisclass_cpp->unload());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_arch_isReadOnly(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::Archive * thisclass_cpp = static_cast<Ogre::Archive*> (getHG3DClassPtr(*thisclass_c, "Ogre::Archive"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isReadOnly());
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_arch_remove(struct hg3dclass_struct * thisclass_c, char * filename_c)
{
  Ogre::Archive * thisclass_cpp = static_cast<Ogre::Archive*> (getHG3DClassPtr(*thisclass_c, "Ogre::Archive"));
  Ogre::String filename_cpp = Ogre::String((const char*) filename_c);
  (thisclass_cpp->remove(filename_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_arch_exists(struct hg3dclass_struct * thisclass_c, char * filename_c, int * result_c)
{
  Ogre::Archive * thisclass_cpp = static_cast<Ogre::Archive*> (getHG3DClassPtr(*thisclass_c, "Ogre::Archive"));
  Ogre::String filename_cpp = Ogre::String((const char*) filename_c);
  bool result_cpp;
  result_cpp = (thisclass_cpp->exists(filename_cpp));
  *result_c = (int)result_cpp;
};

// Return the type code of this Archive
extern "C" Ogre_LIB_EXPORT void ogre_arch_getType(struct hg3dclass_struct * thisclass_c, char * result_c)
{
  Ogre::Archive * thisclass_cpp = static_cast<Ogre::Archive*> (getHG3DClassPtr(*thisclass_c, "Ogre::Archive"));
  Ogre::String result_cpp;
  result_cpp = (thisclass_cpp->getType());
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

