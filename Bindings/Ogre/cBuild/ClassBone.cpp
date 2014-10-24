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

// ClassBone.cpp

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
	#include "StructVec3.h"
#include "StructQuaternion.h"
#include "./Ogre.h"
#include "./OgreString.h"
#include "./WindowUtilsHG3D.h"
#include "HG3DUtilities.h"

using namespace Ogre;
using Ogre::uint;



// 
extern "C" Ogre_LIB_EXPORT void ogre_bn_destruct(struct hg3dclass_struct * thisclass_c)
{
  Ogre::Bone * thisclass_cpp = static_cast<Ogre::Bone*> (getHG3DClassPtr(*thisclass_c, "Ogre::Bone"));
  (delete thisclass_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bn_createChild(struct hg3dclass_struct * thisclass_c, unsigned short handle_c, struct vector3_struct * translate_c, struct quaternion_struct * rotate_c, struct hg3dclass_struct * result_c)
{
  Ogre::Bone * thisclass_cpp = static_cast<Ogre::Bone*> (getHG3DClassPtr(*thisclass_c, "Ogre::Bone"));
  unsigned short handle_cpp = (unsigned short)handle_c;
  Vector3 translate_cpp = *((Vector3*) translate_c);
  Quaternion rotate_cpp = *((Quaternion*) rotate_c);
  Ogre::Bone * result_cpp;
  result_cpp = (thisclass_cpp->createChild(handle_cpp, translate_cpp, rotate_cpp));
  *result_c = getHG3DClass_Bone((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bn_getHandle(struct hg3dclass_struct * thisclass_c, unsigned short * result_c)
{
  Ogre::Bone * thisclass_cpp = static_cast<Ogre::Bone*> (getHG3DClassPtr(*thisclass_c, "Ogre::Bone"));
  unsigned short result_cpp;
  result_cpp = (thisclass_cpp->getHandle());
  *result_c = (unsigned short)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bn_setBindingPose(struct hg3dclass_struct * thisclass_c)
{
  Ogre::Bone * thisclass_cpp = static_cast<Ogre::Bone*> (getHG3DClassPtr(*thisclass_c, "Ogre::Bone"));
  (thisclass_cpp->setBindingPose());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bn_reset(struct hg3dclass_struct * thisclass_c)
{
  Ogre::Bone * thisclass_cpp = static_cast<Ogre::Bone*> (getHG3DClassPtr(*thisclass_c, "Ogre::Bone"));
  (thisclass_cpp->reset());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bn_setManuallyControlled(struct hg3dclass_struct * thisclass_c, long manuallyControlled_c)
{
  Ogre::Bone * thisclass_cpp = static_cast<Ogre::Bone*> (getHG3DClassPtr(*thisclass_c, "Ogre::Bone"));
  bool manuallyControlled_cpp = (bool)manuallyControlled_c;
  (thisclass_cpp->setManuallyControlled(manuallyControlled_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bn_isManuallyControlled(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::Bone * thisclass_cpp = static_cast<Ogre::Bone*> (getHG3DClassPtr(*thisclass_c, "Ogre::Bone"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isManuallyControlled());
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bn_needUpdate(struct hg3dclass_struct * thisclass_c, long forceParentUpdate_c)
{
  Ogre::Bone * thisclass_cpp = static_cast<Ogre::Bone*> (getHG3DClassPtr(*thisclass_c, "Ogre::Bone"));
  bool forceParentUpdate_cpp = (bool)forceParentUpdate_c;
  (thisclass_cpp->needUpdate(forceParentUpdate_cpp));
};

