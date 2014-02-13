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

// ClassAnimationTrack.cpp

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
extern "C" Ogre_LIB_EXPORT void ogre_at_destruct(struct hg3dclass_struct * thisclass_c)
{
  Ogre::AnimationTrack * thisclass_cpp = static_cast<Ogre::AnimationTrack*> (getHG3DClassPtr(*thisclass_c, "Ogre::AnimationTrack"));
  (delete thisclass_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_at_getHandle(struct hg3dclass_struct * thisclass_c, unsigned short * result_c)
{
  Ogre::AnimationTrack * thisclass_cpp = static_cast<Ogre::AnimationTrack*> (getHG3DClassPtr(*thisclass_c, "Ogre::AnimationTrack"));
  unsigned short result_cpp;
  result_cpp = (thisclass_cpp->getHandle());
  *result_c = (unsigned short)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_at_getNumKeyFrames(struct hg3dclass_struct * thisclass_c, unsigned short * result_c)
{
  Ogre::AnimationTrack * thisclass_cpp = static_cast<Ogre::AnimationTrack*> (getHG3DClassPtr(*thisclass_c, "Ogre::AnimationTrack"));
  unsigned short result_cpp;
  result_cpp = (thisclass_cpp->getNumKeyFrames());
  *result_c = (unsigned short)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_at_removeKeyFrame(struct hg3dclass_struct * thisclass_c, unsigned short index_c)
{
  Ogre::AnimationTrack * thisclass_cpp = static_cast<Ogre::AnimationTrack*> (getHG3DClassPtr(*thisclass_c, "Ogre::AnimationTrack"));
  unsigned short index_cpp = (unsigned short)index_c;
  (thisclass_cpp->removeKeyFrame(index_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_at_removeAllKeyFrames(struct hg3dclass_struct * thisclass_c)
{
  Ogre::AnimationTrack * thisclass_cpp = static_cast<Ogre::AnimationTrack*> (getHG3DClassPtr(*thisclass_c, "Ogre::AnimationTrack"));
  (thisclass_cpp->removeAllKeyFrames());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_at_apply(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * timeIndex_c, float weight_c, float scale_c)
{
  Ogre::AnimationTrack * thisclass_cpp = static_cast<Ogre::AnimationTrack*> (getHG3DClassPtr(*thisclass_c, "Ogre::AnimationTrack"));
  const Ogre::TimeIndex * timeIndex_cpp = static_cast<Ogre::TimeIndex*> (getHG3DClassPtr(*timeIndex_c, "Ogre::TimeIndex"));
  Real weight_cpp = (Real)weight_c;
  Real scale_cpp = (Real)scale_c;
  (thisclass_cpp->apply(*timeIndex_cpp, weight_cpp, scale_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_at_hasNonZeroKeyFrames(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::AnimationTrack * thisclass_cpp = static_cast<Ogre::AnimationTrack*> (getHG3DClassPtr(*thisclass_c, "Ogre::AnimationTrack"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->hasNonZeroKeyFrames());
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_at_optimise(struct hg3dclass_struct * thisclass_c)
{
  Ogre::AnimationTrack * thisclass_cpp = static_cast<Ogre::AnimationTrack*> (getHG3DClassPtr(*thisclass_c, "Ogre::AnimationTrack"));
  (thisclass_cpp->optimise());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_at_getParent(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  Ogre::AnimationTrack * thisclass_cpp = static_cast<Ogre::AnimationTrack*> (getHG3DClassPtr(*thisclass_c, "Ogre::AnimationTrack"));
  Ogre::Animation * result_cpp;
  result_cpp = (thisclass_cpp->getParent());
  *result_c = getHG3DClass_Animation((void *) result_cpp);
;
};

