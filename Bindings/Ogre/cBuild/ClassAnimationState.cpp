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

// ClassAnimationState.cpp

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
extern "C" Ogre_LIB_EXPORT void ogre_anms_construct(char * animName_c, struct hg3dclass_struct * parent_c, float timePos_c, float length_c, float weight_c, long enabled_c, struct hg3dclass_struct * result_c)
{
  Ogre::String animName_cpp = Ogre::String((const char*) animName_c);
  Ogre::AnimationStateSet * parent_cpp = static_cast<Ogre::AnimationStateSet*> (getHG3DClassPtr(*parent_c, "Ogre::AnimationStateSet"));
  Real timePos_cpp = (Real)timePos_c;
  Real length_cpp = (Real)length_c;
  Real weight_cpp = (Real)weight_c;
  bool enabled_cpp = (bool)enabled_c;
  Ogre::AnimationState * result_cpp;
  result_cpp = (new Ogre::AnimationState(animName_cpp, parent_cpp, timePos_cpp, length_cpp, weight_cpp, enabled_cpp));
  *result_c = getHG3DClass_AnimationState((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_anms_destruct(struct hg3dclass_struct * thisclass_c)
{
  Ogre::AnimationState * thisclass_cpp = static_cast<Ogre::AnimationState*> (getHG3DClassPtr(*thisclass_c, "Ogre::AnimationState"));
  (delete thisclass_cpp);
};

// Gets the name of the animation to which this state applies. 
extern "C" Ogre_LIB_EXPORT void ogre_anms_getAnimationName(struct hg3dclass_struct * thisclass_c, char * result_c)
{
  Ogre::AnimationState * thisclass_cpp = static_cast<Ogre::AnimationState*> (getHG3DClassPtr(*thisclass_c, "Ogre::AnimationState"));
  Ogre::String result_cpp;
  result_cpp = (thisclass_cpp->getAnimationName());
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

// Gets the time position for this animation. 
extern "C" Ogre_LIB_EXPORT void ogre_anms_getTimePosition(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  Ogre::AnimationState * thisclass_cpp = static_cast<Ogre::AnimationState*> (getHG3DClassPtr(*thisclass_c, "Ogre::AnimationState"));
  Real result_cpp;
  result_cpp = (thisclass_cpp->getTimePosition());
  *result_c = (float)result_cpp;
};

// Sets the time position for this animation. 
extern "C" Ogre_LIB_EXPORT void ogre_anms_setTimePosition(struct hg3dclass_struct * thisclass_c, float timePos_c)
{
  Ogre::AnimationState * thisclass_cpp = static_cast<Ogre::AnimationState*> (getHG3DClassPtr(*thisclass_c, "Ogre::AnimationState"));
  Real timePos_cpp = (Real)timePos_c;
  (thisclass_cpp->setTimePosition(timePos_cpp));
};

// Gets the total length of this animation (may be shorter than whole animation) 
extern "C" Ogre_LIB_EXPORT void ogre_anms_getLength(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  Ogre::AnimationState * thisclass_cpp = static_cast<Ogre::AnimationState*> (getHG3DClassPtr(*thisclass_c, "Ogre::AnimationState"));
  Real result_cpp;
  result_cpp = (thisclass_cpp->getLength());
  *result_c = (float)result_cpp;
};

// Sets the total length of this animation (may be shorter than whole animation) 
extern "C" Ogre_LIB_EXPORT void ogre_anms_setLength(struct hg3dclass_struct * thisclass_c, float len_c)
{
  Ogre::AnimationState * thisclass_cpp = static_cast<Ogre::AnimationState*> (getHG3DClassPtr(*thisclass_c, "Ogre::AnimationState"));
  Real len_cpp = (Real)len_c;
  (thisclass_cpp->setLength(len_cpp));
};

// Gets the weight (influence) of this animation. 
extern "C" Ogre_LIB_EXPORT void ogre_anms_getWeight(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  Ogre::AnimationState * thisclass_cpp = static_cast<Ogre::AnimationState*> (getHG3DClassPtr(*thisclass_c, "Ogre::AnimationState"));
  Real result_cpp;
  result_cpp = (thisclass_cpp->getWeight());
  *result_c = (float)result_cpp;
};

// Sets the weight (influence) of this animation. 
extern "C" Ogre_LIB_EXPORT void ogre_anms_setWeight(struct hg3dclass_struct * thisclass_c, float weight_c)
{
  Ogre::AnimationState * thisclass_cpp = static_cast<Ogre::AnimationState*> (getHG3DClassPtr(*thisclass_c, "Ogre::AnimationState"));
  Real weight_cpp = (Real)weight_c;
  (thisclass_cpp->setWeight(weight_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_anms_addTime(struct hg3dclass_struct * thisclass_c, float offset_c)
{
  Ogre::AnimationState * thisclass_cpp = static_cast<Ogre::AnimationState*> (getHG3DClassPtr(*thisclass_c, "Ogre::AnimationState"));
  Real offset_cpp = (Real)offset_c;
  (thisclass_cpp->addTime(offset_cpp));
};

// Returns true if the animation has reached the end and is not looping. 
extern "C" Ogre_LIB_EXPORT void ogre_anms_hasEnded(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::AnimationState * thisclass_cpp = static_cast<Ogre::AnimationState*> (getHG3DClassPtr(*thisclass_c, "Ogre::AnimationState"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->hasEnded());
  *result_c = (long)result_cpp;
};

// Returns true if this animation is currently enabled. 
extern "C" Ogre_LIB_EXPORT void ogre_anms_getEnabled(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::AnimationState * thisclass_cpp = static_cast<Ogre::AnimationState*> (getHG3DClassPtr(*thisclass_c, "Ogre::AnimationState"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->getEnabled());
  *result_c = (long)result_cpp;
};

// Sets whether this animation is enabled. 
extern "C" Ogre_LIB_EXPORT void ogre_anms_setEnabled(struct hg3dclass_struct * thisclass_c, long enabled_c)
{
  Ogre::AnimationState * thisclass_cpp = static_cast<Ogre::AnimationState*> (getHG3DClassPtr(*thisclass_c, "Ogre::AnimationState"));
  bool enabled_cpp = (bool)enabled_c;
  (thisclass_cpp->setEnabled(enabled_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_anms_setLoop(struct hg3dclass_struct * thisclass_c, long loop_c)
{
  Ogre::AnimationState * thisclass_cpp = static_cast<Ogre::AnimationState*> (getHG3DClassPtr(*thisclass_c, "Ogre::AnimationState"));
  bool loop_cpp = (bool)loop_c;
  (thisclass_cpp->setLoop(loop_cpp));
};

// Gets whether or not this animation loops. 
extern "C" Ogre_LIB_EXPORT void ogre_anms_getLoop(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::AnimationState * thisclass_cpp = static_cast<Ogre::AnimationState*> (getHG3DClassPtr(*thisclass_c, "Ogre::AnimationState"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->getLoop());
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_anms_copyStateFrom(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * animState_c)
{
  Ogre::AnimationState * thisclass_cpp = static_cast<Ogre::AnimationState*> (getHG3DClassPtr(*thisclass_c, "Ogre::AnimationState"));
  const Ogre::AnimationState * animState_cpp = static_cast<Ogre::AnimationState*> (getHG3DClassPtr(*animState_c, "Ogre::AnimationState"));
  (thisclass_cpp->copyStateFrom(*animState_cpp));
};

// Get the parent animation state set. 
extern "C" Ogre_LIB_EXPORT void ogre_anms_getParent(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  Ogre::AnimationState * thisclass_cpp = static_cast<Ogre::AnimationState*> (getHG3DClassPtr(*thisclass_c, "Ogre::AnimationState"));
  Ogre::AnimationStateSet * result_cpp;
  result_cpp = (thisclass_cpp->getParent());
  *result_c = getHG3DClass_AnimationStateSet((void *) result_cpp);
;
};

// Create a new blend mask with the given number of entries. 
extern "C" Ogre_LIB_EXPORT void ogre_anms_createBlendMask(struct hg3dclass_struct * thisclass_c, long blendMaskSizeHint_c, float initialWeight_c)
{
  Ogre::AnimationState * thisclass_cpp = static_cast<Ogre::AnimationState*> (getHG3DClassPtr(*thisclass_c, "Ogre::AnimationState"));
  size_t blendMaskSizeHint_cpp = (size_t)blendMaskSizeHint_c;
  float initialWeight_cpp = (float)initialWeight_c;
  (thisclass_cpp->createBlendMask(blendMaskSizeHint_cpp, initialWeight_cpp));
};

// Destroy the currently set blend mask. 
extern "C" Ogre_LIB_EXPORT void ogre_anms_destroyBlendMask(struct hg3dclass_struct * thisclass_c)
{
  Ogre::AnimationState * thisclass_cpp = static_cast<Ogre::AnimationState*> (getHG3DClassPtr(*thisclass_c, "Ogre::AnimationState"));
  (thisclass_cpp->destroyBlendMask());
};

// Return whether there is currently a valid blend mask set. 
extern "C" Ogre_LIB_EXPORT void ogre_anms_hasBlendMask(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::AnimationState * thisclass_cpp = static_cast<Ogre::AnimationState*> (getHG3DClassPtr(*thisclass_c, "Ogre::AnimationState"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->hasBlendMask());
  *result_c = (long)result_cpp;
};

// Set the weight for the bone identified by the given handle. 
extern "C" Ogre_LIB_EXPORT void ogre_anms_setBlendMaskEntry(struct hg3dclass_struct * thisclass_c, long boneHandle_c, float weight_c)
{
  Ogre::AnimationState * thisclass_cpp = static_cast<Ogre::AnimationState*> (getHG3DClassPtr(*thisclass_c, "Ogre::AnimationState"));
  size_t boneHandle_cpp = (size_t)boneHandle_c;
  float weight_cpp = (float)weight_c;
  (thisclass_cpp->setBlendMaskEntry(boneHandle_cpp, weight_cpp));
};

// Get the weight for the bone identified by the given handle. 
extern "C" Ogre_LIB_EXPORT void ogre_anms_getBlendMaskEntry(struct hg3dclass_struct * thisclass_c, long boneHandle_c, float * result_c)
{
  Ogre::AnimationState * thisclass_cpp = static_cast<Ogre::AnimationState*> (getHG3DClassPtr(*thisclass_c, "Ogre::AnimationState"));
  size_t boneHandle_cpp = (size_t)boneHandle_c;
  float result_cpp;
  result_cpp = (thisclass_cpp->getBlendMaskEntry(boneHandle_cpp));
  *result_c = (float)result_cpp;
};

