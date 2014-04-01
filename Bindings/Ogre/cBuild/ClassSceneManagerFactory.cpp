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

// ClassSceneManagerFactory.cpp

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
extern "C" Ogre_LIB_EXPORT void ogre_smf_destruct(struct hg3dclass_struct * thisclass_c)
{
  Ogre::SceneManagerFactory * thisclass_cpp = static_cast<Ogre::SceneManagerFactory*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManagerFactory"));
  (delete thisclass_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_smf_createInstance(struct hg3dclass_struct * thisclass_c, char * instanceName_c, struct hg3dclass_struct * result_c)
{
  Ogre::SceneManagerFactory * thisclass_cpp = static_cast<Ogre::SceneManagerFactory*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManagerFactory"));
  Ogre::String instanceName_cpp = Ogre::String((const char*) instanceName_c);
  Ogre::SceneManager * result_cpp;
  result_cpp = (thisclass_cpp->createInstance(instanceName_cpp));
  *result_c = getHG3DClass_SceneManager((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_smf_destroyInstance(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * instance_c)
{
  Ogre::SceneManagerFactory * thisclass_cpp = static_cast<Ogre::SceneManagerFactory*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneManagerFactory"));
  Ogre::SceneManager * instance_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*instance_c, "Ogre::SceneManager"));
  (thisclass_cpp->destroyInstance(instance_cpp));
};

