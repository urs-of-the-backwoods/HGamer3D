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

// ClassMaterialManager.cpp

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
	#include "StructSharedPtr.h"
#include "./Ogre.h"
#include "./OgreString.h"
#include "./WindowUtilsHG3D.h"
#include "HG3DUtilities.h"

using namespace Ogre;



// 
extern "C" Ogre_LIB_EXPORT void ogre_mtrlmgr_construct(struct hg3dclass_struct * result_c)
{
  Ogre::MaterialManager * result_cpp;
  result_cpp = (new Ogre::MaterialManager());
  *result_c = getHG3DClass_MaterialManager((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mtrlmgr_destruct(struct hg3dclass_struct * thisclass_c)
{
  Ogre::MaterialManager * thisclass_cpp = static_cast<Ogre::MaterialManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::MaterialManager"));
  (delete thisclass_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mtrlmgr_initialise(struct hg3dclass_struct * thisclass_c)
{
  Ogre::MaterialManager * thisclass_cpp = static_cast<Ogre::MaterialManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::MaterialManager"));
  (thisclass_cpp->initialise());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mtrlmgr_setDefaultAnisotropy(struct hg3dclass_struct * thisclass_c, unsigned int maxAniso_c)
{
  Ogre::MaterialManager * thisclass_cpp = static_cast<Ogre::MaterialManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::MaterialManager"));
  unsigned int maxAniso_cpp = (unsigned int)maxAniso_c;
  (thisclass_cpp->setDefaultAnisotropy(maxAniso_cpp));
};

// Get the default maxAnisotropy. 
extern "C" Ogre_LIB_EXPORT void ogre_mtrlmgr_getDefaultAnisotropy(struct hg3dclass_struct * thisclass_c, unsigned int * result_c)
{
  Ogre::MaterialManager * thisclass_cpp = static_cast<Ogre::MaterialManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::MaterialManager"));
  unsigned int result_cpp;
  result_cpp = (thisclass_cpp->getDefaultAnisotropy());
  *result_c = (unsigned int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mtrlmgr_getDefaultSettings(struct hg3dclass_struct * thisclass_c, struct sharedptr_struct * result_c)
{
  Ogre::MaterialManager * thisclass_cpp = static_cast<Ogre::MaterialManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::MaterialManager"));
  MaterialPtr result_cpp;
  result_cpp = (thisclass_cpp->getDefaultSettings());
  *result_c = *((struct sharedptr_struct*) &result_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mtrlmgr_getActiveScheme(struct hg3dclass_struct * thisclass_c, char * result_c)
{
  Ogre::MaterialManager * thisclass_cpp = static_cast<Ogre::MaterialManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::MaterialManager"));
  Ogre::String result_cpp;
  result_cpp = (thisclass_cpp->getActiveScheme());
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mtrlmgr_setActiveScheme(struct hg3dclass_struct * thisclass_c, char * schemeName_c)
{
  Ogre::MaterialManager * thisclass_cpp = static_cast<Ogre::MaterialManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::MaterialManager"));
  Ogre::String schemeName_cpp = Ogre::String((const char*) schemeName_c);
  (thisclass_cpp->setActiveScheme(schemeName_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mtrlmgr_getSingleton(struct hg3dclass_struct * result_c)
{
  Ogre::MaterialManager * result_cpp;
  result_cpp = &(Ogre::MaterialManager::getSingleton());
  *result_c = getHG3DClass_MaterialManager((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mtrlmgr_getSingletonPtr(struct hg3dclass_struct * result_c)
{
  Ogre::MaterialManager * result_cpp;
  result_cpp = (Ogre::MaterialManager::getSingletonPtr());
  *result_c = getHG3DClass_MaterialManager((void *) result_cpp);
;
};

