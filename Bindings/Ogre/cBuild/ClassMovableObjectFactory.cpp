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

// ClassMovableObjectFactory.cpp

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
extern "C" Ogre_LIB_EXPORT void ogre_mvof_destruct(struct hg3dclass_struct * thisclass_c)
{
  Ogre::MovableObjectFactory * thisclass_cpp = static_cast<Ogre::MovableObjectFactory*> (getHG3DClassPtr(*thisclass_c, "Ogre::MovableObjectFactory"));
  (delete thisclass_cpp);
};

// Get the type of the object to be created. 
extern "C" Ogre_LIB_EXPORT void ogre_mvof_getType(struct hg3dclass_struct * thisclass_c, char * result_c)
{
  Ogre::MovableObjectFactory * thisclass_cpp = static_cast<Ogre::MovableObjectFactory*> (getHG3DClassPtr(*thisclass_c, "Ogre::MovableObjectFactory"));
  Ogre::String result_cpp;
  result_cpp = (thisclass_cpp->getType());
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mvof_destroyInstance(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * obj_c)
{
  Ogre::MovableObjectFactory * thisclass_cpp = static_cast<Ogre::MovableObjectFactory*> (getHG3DClassPtr(*thisclass_c, "Ogre::MovableObjectFactory"));
  Ogre::MovableObject * obj_cpp = static_cast<Ogre::MovableObject*> (getHG3DClassPtr(*obj_c, "Ogre::MovableObject"));
  (thisclass_cpp->destroyInstance(obj_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mvof_requestTypeFlags(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::MovableObjectFactory * thisclass_cpp = static_cast<Ogre::MovableObjectFactory*> (getHG3DClassPtr(*thisclass_c, "Ogre::MovableObjectFactory"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->requestTypeFlags());
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mvof_getTypeFlags(struct hg3dclass_struct * thisclass_c, unsigned int * result_c)
{
  Ogre::MovableObjectFactory * thisclass_cpp = static_cast<Ogre::MovableObjectFactory*> (getHG3DClassPtr(*thisclass_c, "Ogre::MovableObjectFactory"));
  uint32 result_cpp;
  result_cpp = (thisclass_cpp->getTypeFlags());
  *result_c = (unsigned int)result_cpp;
};

