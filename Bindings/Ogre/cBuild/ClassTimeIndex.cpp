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

// ClassTimeIndex.cpp

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
extern "C" Ogre_LIB_EXPORT void ogre_tnx_construct(float timePos_c, struct hg3dclass_struct * result_c)
{
  Real timePos_cpp = (Real)timePos_c;
  Ogre::TimeIndex * result_cpp;
  result_cpp = (new Ogre::TimeIndex(timePos_cpp));
  *result_c = getHG3DClass_TimeIndex((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_tnx_hasKeyIndex(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::TimeIndex * thisclass_cpp = static_cast<Ogre::TimeIndex*> (getHG3DClassPtr(*thisclass_c, "Ogre::TimeIndex"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->hasKeyIndex());
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_tnx_getTimePos(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  Ogre::TimeIndex * thisclass_cpp = static_cast<Ogre::TimeIndex*> (getHG3DClassPtr(*thisclass_c, "Ogre::TimeIndex"));
  Real result_cpp;
  result_cpp = (thisclass_cpp->getTimePos());
  *result_c = (float)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_tnx_getKeyIndex(struct hg3dclass_struct * thisclass_c, unsigned long * result_c)
{
  Ogre::TimeIndex * thisclass_cpp = static_cast<Ogre::TimeIndex*> (getHG3DClassPtr(*thisclass_c, "Ogre::TimeIndex"));
  uint result_cpp;
  result_cpp = (thisclass_cpp->getKeyIndex());
  *result_c = (unsigned long)result_cpp;
};

