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

// ClassSkeletonManager.cpp

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



// Constructor. 
extern "C" Ogre_LIB_EXPORT void ogre_sklmgr_construct(struct hg3dclass_struct * result_c)
{
  Ogre::SkeletonManager * result_cpp;
  result_cpp = (new Ogre::SkeletonManager());
  *result_c = getHG3DClass_SkeletonManager((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_sklmgr_destruct(struct hg3dclass_struct * thisclass_c)
{
  Ogre::SkeletonManager * thisclass_cpp = static_cast<Ogre::SkeletonManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::SkeletonManager"));
  (delete thisclass_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_sklmgr_getSingleton(struct hg3dclass_struct * result_c)
{
  Ogre::SkeletonManager * result_cpp;
  result_cpp = &(Ogre::SkeletonManager::getSingleton());
  *result_c = getHG3DClass_SkeletonManager((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_sklmgr_getSingletonPtr(struct hg3dclass_struct * result_c)
{
  Ogre::SkeletonManager * result_cpp;
  result_cpp = (Ogre::SkeletonManager::getSingletonPtr());
  *result_c = getHG3DClass_SkeletonManager((void *) result_cpp);
;
};

