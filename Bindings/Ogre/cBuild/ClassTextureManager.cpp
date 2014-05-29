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

// ClassTextureManager.cpp

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
extern "C" Ogre_LIB_EXPORT void ogre_txmgr_destruct(struct hg3dclass_struct * thisclass_c)
{
  Ogre::TextureManager * thisclass_cpp = static_cast<Ogre::TextureManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::TextureManager"));
  (delete thisclass_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_txmgr_setDefaultNumMipmaps(struct hg3dclass_struct * thisclass_c, long num_c)
{
  Ogre::TextureManager * thisclass_cpp = static_cast<Ogre::TextureManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::TextureManager"));
  size_t num_cpp = (size_t)num_c;
  (thisclass_cpp->setDefaultNumMipmaps(num_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_txmgr_getDefaultNumMipmaps(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::TextureManager * thisclass_cpp = static_cast<Ogre::TextureManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::TextureManager"));
  size_t result_cpp;
  result_cpp = (thisclass_cpp->getDefaultNumMipmaps());
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_txmgr_getSingleton(struct hg3dclass_struct * result_c)
{
  Ogre::TextureManager * result_cpp;
  result_cpp = &(Ogre::TextureManager::getSingleton());
  *result_c = getHG3DClass_TextureManager((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_txmgr_getSingletonPtr(struct hg3dclass_struct * result_c)
{
  Ogre::TextureManager * result_cpp;
  result_cpp = (Ogre::TextureManager::getSingletonPtr());
  *result_c = getHG3DClass_TextureManager((void *) result_cpp);
;
};

