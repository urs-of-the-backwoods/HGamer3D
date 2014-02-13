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

// ClassArchiveManager.cpp

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
extern "C" Ogre_LIB_EXPORT void ogre_archm_destruct(struct hg3dclass_struct * thisclass_c)
{
  Ogre::ArchiveManager * thisclass_cpp = static_cast<Ogre::ArchiveManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::ArchiveManager"));
  (delete thisclass_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_archm_load(struct hg3dclass_struct * thisclass_c, char * filename_c, char * archiveType_c, struct hg3dclass_struct * result_c)
{
  Ogre::ArchiveManager * thisclass_cpp = static_cast<Ogre::ArchiveManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::ArchiveManager"));
  Ogre::String filename_cpp = Ogre::String((const char*) filename_c);
  Ogre::String archiveType_cpp = Ogre::String((const char*) archiveType_c);
  Ogre::Archive * result_cpp;
  result_cpp = (thisclass_cpp->load(filename_cpp, archiveType_cpp));
  *result_c = getHG3DClass_Archive((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_archm_unload(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * arch_c)
{
  Ogre::ArchiveManager * thisclass_cpp = static_cast<Ogre::ArchiveManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::ArchiveManager"));
  Ogre::Archive * arch_cpp = static_cast<Ogre::Archive*> (getHG3DClassPtr(*arch_c, "Ogre::Archive"));
  (thisclass_cpp->unload(arch_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_archm_unload2(struct hg3dclass_struct * thisclass_c, char * filename_c)
{
  Ogre::ArchiveManager * thisclass_cpp = static_cast<Ogre::ArchiveManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::ArchiveManager"));
  Ogre::String filename_cpp = Ogre::String((const char*) filename_c);
  (thisclass_cpp->unload(filename_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_archm_getSingleton(struct hg3dclass_struct * result_c)
{
  Ogre::ArchiveManager * result_cpp;
  result_cpp = &(Ogre::ArchiveManager::getSingleton());
  *result_c = getHG3DClass_ArchiveManager((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_archm_getSingletonPtr(struct hg3dclass_struct * result_c)
{
  Ogre::ArchiveManager * result_cpp;
  result_cpp = (Ogre::ArchiveManager::getSingletonPtr());
  *result_c = getHG3DClass_ArchiveManager((void *) result_cpp);
;
};

