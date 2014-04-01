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

// ClassEntityFactory.cpp

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
extern "C" Ogre_LIB_EXPORT void ogre_entf_construct(struct hg3dclass_struct * result_c)
{
  Ogre::EntityFactory * result_cpp;
  result_cpp = (new Ogre::EntityFactory());
  *result_c = getHG3DClass_EntityFactory((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_entf_destruct(struct hg3dclass_struct * thisclass_c)
{
  Ogre::EntityFactory * thisclass_cpp = static_cast<Ogre::EntityFactory*> (getHG3DClassPtr(*thisclass_c, "Ogre::EntityFactory"));
  (delete thisclass_cpp);
};

// Get the type of the object to be created. 
extern "C" Ogre_LIB_EXPORT void ogre_entf_getType(struct hg3dclass_struct * thisclass_c, char * result_c)
{
  Ogre::EntityFactory * thisclass_cpp = static_cast<Ogre::EntityFactory*> (getHG3DClassPtr(*thisclass_c, "Ogre::EntityFactory"));
  Ogre::String result_cpp;
  result_cpp = (thisclass_cpp->getType());
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_entf_destroyInstance(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * obj_c)
{
  Ogre::EntityFactory * thisclass_cpp = static_cast<Ogre::EntityFactory*> (getHG3DClassPtr(*thisclass_c, "Ogre::EntityFactory"));
  Ogre::MovableObject * obj_cpp = static_cast<Ogre::MovableObject*> (getHG3DClassPtr(*obj_c, "Ogre::MovableObject"));
  (thisclass_cpp->destroyInstance(obj_cpp));
};

