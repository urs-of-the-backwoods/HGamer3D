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

// ClassAnimationStateSet.cpp

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
extern "C" Ogre_LIB_EXPORT void ogre_ass_destruct(struct hg3dclass_struct * thisclass_c)
{
  Ogre::AnimationStateSet * thisclass_cpp = static_cast<Ogre::AnimationStateSet*> (getHG3DClassPtr(*thisclass_c, "Ogre::AnimationStateSet"));
  (delete thisclass_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_ass_createAnimationState(struct hg3dclass_struct * thisclass_c, char * animName_c, float timePos_c, float length_c, float weight_c, long enabled_c, struct hg3dclass_struct * result_c)
{
  Ogre::AnimationStateSet * thisclass_cpp = static_cast<Ogre::AnimationStateSet*> (getHG3DClassPtr(*thisclass_c, "Ogre::AnimationStateSet"));
  Ogre::String animName_cpp = Ogre::String((const char*) animName_c);
  Real timePos_cpp = (Real)timePos_c;
  Real length_cpp = (Real)length_c;
  Real weight_cpp = (Real)weight_c;
  bool enabled_cpp = (bool)enabled_c;
  Ogre::AnimationState * result_cpp;
  result_cpp = (thisclass_cpp->createAnimationState(animName_cpp, timePos_cpp, length_cpp, weight_cpp, enabled_cpp));
  *result_c = getHG3DClass_AnimationState((void *) result_cpp);
;
};

// Get an animation state by the name of the animation. 
extern "C" Ogre_LIB_EXPORT void ogre_ass_getAnimationState(struct hg3dclass_struct * thisclass_c, char * name_c, struct hg3dclass_struct * result_c)
{
  Ogre::AnimationStateSet * thisclass_cpp = static_cast<Ogre::AnimationStateSet*> (getHG3DClassPtr(*thisclass_c, "Ogre::AnimationStateSet"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  Ogre::AnimationState * result_cpp;
  result_cpp = (thisclass_cpp->getAnimationState(name_cpp));
  *result_c = getHG3DClass_AnimationState((void *) result_cpp);
;
};

// Tests if state for the named animation is present. 
extern "C" Ogre_LIB_EXPORT void ogre_ass_hasAnimationState(struct hg3dclass_struct * thisclass_c, char * name_c, long * result_c)
{
  Ogre::AnimationStateSet * thisclass_cpp = static_cast<Ogre::AnimationStateSet*> (getHG3DClassPtr(*thisclass_c, "Ogre::AnimationStateSet"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  bool result_cpp;
  result_cpp = (thisclass_cpp->hasAnimationState(name_cpp));
  *result_c = (long)result_cpp;
};

// Remove animation state with the given name. 
extern "C" Ogre_LIB_EXPORT void ogre_ass_removeAnimationState(struct hg3dclass_struct * thisclass_c, char * name_c)
{
  Ogre::AnimationStateSet * thisclass_cpp = static_cast<Ogre::AnimationStateSet*> (getHG3DClassPtr(*thisclass_c, "Ogre::AnimationStateSet"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  (thisclass_cpp->removeAnimationState(name_cpp));
};

// Remove all animation states. 
extern "C" Ogre_LIB_EXPORT void ogre_ass_removeAllAnimationStates(struct hg3dclass_struct * thisclass_c)
{
  Ogre::AnimationStateSet * thisclass_cpp = static_cast<Ogre::AnimationStateSet*> (getHG3DClassPtr(*thisclass_c, "Ogre::AnimationStateSet"));
  (thisclass_cpp->removeAllAnimationStates());
};

// Copy the state of any matching animation states from this to another. 
extern "C" Ogre_LIB_EXPORT void ogre_ass_copyMatchingState(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * target_c)
{
  Ogre::AnimationStateSet * thisclass_cpp = static_cast<Ogre::AnimationStateSet*> (getHG3DClassPtr(*thisclass_c, "Ogre::AnimationStateSet"));
  Ogre::AnimationStateSet * target_cpp = static_cast<Ogre::AnimationStateSet*> (getHG3DClassPtr(*target_c, "Ogre::AnimationStateSet"));
  (thisclass_cpp->copyMatchingState(target_cpp));
};

// Get the latest animation state been altered frame number. 
extern "C" Ogre_LIB_EXPORT void ogre_ass_getDirtyFrameNumber(struct hg3dclass_struct * thisclass_c, unsigned long * result_c)
{
  Ogre::AnimationStateSet * thisclass_cpp = static_cast<Ogre::AnimationStateSet*> (getHG3DClassPtr(*thisclass_c, "Ogre::AnimationStateSet"));
  unsigned long result_cpp;
  result_cpp = (thisclass_cpp->getDirtyFrameNumber());
  *result_c = (unsigned long)result_cpp;
};

// Tests if exists enabled animation state in this set. 
extern "C" Ogre_LIB_EXPORT void ogre_ass_hasEnabledAnimationState(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::AnimationStateSet * thisclass_cpp = static_cast<Ogre::AnimationStateSet*> (getHG3DClassPtr(*thisclass_c, "Ogre::AnimationStateSet"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->hasEnabledAnimationState());
  *result_c = (long)result_cpp;
};

