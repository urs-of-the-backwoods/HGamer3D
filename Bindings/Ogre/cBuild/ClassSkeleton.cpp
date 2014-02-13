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

// ClassSkeleton.cpp

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
	#include "EnumSkeletonAnimationBlendMode.h"
#include "./Ogre.h"
#include "./OgreString.h"
#include "./WindowUtilsHG3D.h"
#include "HG3DUtilities.h"

using namespace Ogre;



// 
extern "C" Ogre_LIB_EXPORT void ogre_skl_destruct(struct hg3dclass_struct * thisclass_c)
{
  Ogre::Skeleton * thisclass_cpp = static_cast<Ogre::Skeleton*> (getHG3DClassPtr(*thisclass_c, "Ogre::Skeleton"));
  (delete thisclass_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_skl_createBone(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  Ogre::Skeleton * thisclass_cpp = static_cast<Ogre::Skeleton*> (getHG3DClassPtr(*thisclass_c, "Ogre::Skeleton"));
  Ogre::Bone * result_cpp;
  result_cpp = (thisclass_cpp->createBone());
  *result_c = getHG3DClass_Bone((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_skl_createBone2(struct hg3dclass_struct * thisclass_c, unsigned short handle_c, struct hg3dclass_struct * result_c)
{
  Ogre::Skeleton * thisclass_cpp = static_cast<Ogre::Skeleton*> (getHG3DClassPtr(*thisclass_c, "Ogre::Skeleton"));
  unsigned short handle_cpp = (unsigned short)handle_c;
  Ogre::Bone * result_cpp;
  result_cpp = (thisclass_cpp->createBone(handle_cpp));
  *result_c = getHG3DClass_Bone((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_skl_createBone3(struct hg3dclass_struct * thisclass_c, char * name_c, struct hg3dclass_struct * result_c)
{
  Ogre::Skeleton * thisclass_cpp = static_cast<Ogre::Skeleton*> (getHG3DClassPtr(*thisclass_c, "Ogre::Skeleton"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  Ogre::Bone * result_cpp;
  result_cpp = (thisclass_cpp->createBone(name_cpp));
  *result_c = getHG3DClass_Bone((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_skl_createBone4(struct hg3dclass_struct * thisclass_c, char * name_c, unsigned short handle_c, struct hg3dclass_struct * result_c)
{
  Ogre::Skeleton * thisclass_cpp = static_cast<Ogre::Skeleton*> (getHG3DClassPtr(*thisclass_c, "Ogre::Skeleton"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  unsigned short handle_cpp = (unsigned short)handle_c;
  Ogre::Bone * result_cpp;
  result_cpp = (thisclass_cpp->createBone(name_cpp, handle_cpp));
  *result_c = getHG3DClass_Bone((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_skl_getNumBones(struct hg3dclass_struct * thisclass_c, unsigned short * result_c)
{
  Ogre::Skeleton * thisclass_cpp = static_cast<Ogre::Skeleton*> (getHG3DClassPtr(*thisclass_c, "Ogre::Skeleton"));
  unsigned short result_cpp;
  result_cpp = (thisclass_cpp->getNumBones());
  *result_c = (unsigned short)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_skl_getRootBone(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  Ogre::Skeleton * thisclass_cpp = static_cast<Ogre::Skeleton*> (getHG3DClassPtr(*thisclass_c, "Ogre::Skeleton"));
  Ogre::Bone * result_cpp;
  result_cpp = (thisclass_cpp->getRootBone());
  *result_c = getHG3DClass_Bone((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_skl_getBone(struct hg3dclass_struct * thisclass_c, unsigned short handle_c, struct hg3dclass_struct * result_c)
{
  Ogre::Skeleton * thisclass_cpp = static_cast<Ogre::Skeleton*> (getHG3DClassPtr(*thisclass_c, "Ogre::Skeleton"));
  unsigned short handle_cpp = (unsigned short)handle_c;
  Ogre::Bone * result_cpp;
  result_cpp = (thisclass_cpp->getBone(handle_cpp));
  *result_c = getHG3DClass_Bone((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_skl_getBone2(struct hg3dclass_struct * thisclass_c, char * name_c, struct hg3dclass_struct * result_c)
{
  Ogre::Skeleton * thisclass_cpp = static_cast<Ogre::Skeleton*> (getHG3DClassPtr(*thisclass_c, "Ogre::Skeleton"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  Ogre::Bone * result_cpp;
  result_cpp = (thisclass_cpp->getBone(name_cpp));
  *result_c = getHG3DClass_Bone((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_skl_hasBone(struct hg3dclass_struct * thisclass_c, char * name_c, int * result_c)
{
  Ogre::Skeleton * thisclass_cpp = static_cast<Ogre::Skeleton*> (getHG3DClassPtr(*thisclass_c, "Ogre::Skeleton"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  bool result_cpp;
  result_cpp = (thisclass_cpp->hasBone(name_cpp));
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_skl_setBindingPose(struct hg3dclass_struct * thisclass_c)
{
  Ogre::Skeleton * thisclass_cpp = static_cast<Ogre::Skeleton*> (getHG3DClassPtr(*thisclass_c, "Ogre::Skeleton"));
  (thisclass_cpp->setBindingPose());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_skl_reset(struct hg3dclass_struct * thisclass_c, int resetManualBones_c)
{
  Ogre::Skeleton * thisclass_cpp = static_cast<Ogre::Skeleton*> (getHG3DClassPtr(*thisclass_c, "Ogre::Skeleton"));
  bool resetManualBones_cpp = (bool)resetManualBones_c;
  (thisclass_cpp->reset(resetManualBones_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_skl_createAnimation(struct hg3dclass_struct * thisclass_c, char * name_c, float length_c, struct hg3dclass_struct * result_c)
{
  Ogre::Skeleton * thisclass_cpp = static_cast<Ogre::Skeleton*> (getHG3DClassPtr(*thisclass_c, "Ogre::Skeleton"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  Real length_cpp = (Real)length_c;
  Ogre::Animation * result_cpp;
  result_cpp = (thisclass_cpp->createAnimation(name_cpp, length_cpp));
  *result_c = getHG3DClass_Animation((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_skl_getAnimation2(struct hg3dclass_struct * thisclass_c, char * name_c, struct hg3dclass_struct * result_c)
{
  Ogre::Skeleton * thisclass_cpp = static_cast<Ogre::Skeleton*> (getHG3DClassPtr(*thisclass_c, "Ogre::Skeleton"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  Ogre::Animation * result_cpp;
  result_cpp = (thisclass_cpp->getAnimation(name_cpp));
  *result_c = getHG3DClass_Animation((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_skl_hasAnimation(struct hg3dclass_struct * thisclass_c, char * name_c, int * result_c)
{
  Ogre::Skeleton * thisclass_cpp = static_cast<Ogre::Skeleton*> (getHG3DClassPtr(*thisclass_c, "Ogre::Skeleton"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  bool result_cpp;
  result_cpp = (thisclass_cpp->hasAnimation(name_cpp));
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_skl_removeAnimation(struct hg3dclass_struct * thisclass_c, char * name_c)
{
  Ogre::Skeleton * thisclass_cpp = static_cast<Ogre::Skeleton*> (getHG3DClassPtr(*thisclass_c, "Ogre::Skeleton"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  (thisclass_cpp->removeAnimation(name_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_skl_setAnimationState(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * animSet_c)
{
  Ogre::Skeleton * thisclass_cpp = static_cast<Ogre::Skeleton*> (getHG3DClassPtr(*thisclass_c, "Ogre::Skeleton"));
  const Ogre::AnimationStateSet * animSet_cpp = static_cast<Ogre::AnimationStateSet*> (getHG3DClassPtr(*animSet_c, "Ogre::AnimationStateSet"));
  (thisclass_cpp->setAnimationState(*animSet_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_skl_getNumAnimations(struct hg3dclass_struct * thisclass_c, unsigned short * result_c)
{
  Ogre::Skeleton * thisclass_cpp = static_cast<Ogre::Skeleton*> (getHG3DClassPtr(*thisclass_c, "Ogre::Skeleton"));
  unsigned short result_cpp;
  result_cpp = (thisclass_cpp->getNumAnimations());
  *result_c = (unsigned short)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_skl_getAnimation3(struct hg3dclass_struct * thisclass_c, unsigned short index_c, struct hg3dclass_struct * result_c)
{
  Ogre::Skeleton * thisclass_cpp = static_cast<Ogre::Skeleton*> (getHG3DClassPtr(*thisclass_c, "Ogre::Skeleton"));
  unsigned short index_cpp = (unsigned short)index_c;
  Ogre::Animation * result_cpp;
  result_cpp = (thisclass_cpp->getAnimation(index_cpp));
  *result_c = getHG3DClass_Animation((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_skl_getBlendMode(struct hg3dclass_struct * thisclass_c, enum EnumSkeletonAnimationBlendMode * result_c)
{
  Ogre::Skeleton * thisclass_cpp = static_cast<Ogre::Skeleton*> (getHG3DClassPtr(*thisclass_c, "Ogre::Skeleton"));
  enum Ogre::SkeletonAnimationBlendMode result_cpp;
  result_cpp = (thisclass_cpp->getBlendMode());
  *result_c = (enum EnumSkeletonAnimationBlendMode) result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_skl_setBlendMode(struct hg3dclass_struct * thisclass_c, enum EnumSkeletonAnimationBlendMode state_c)
{
  Ogre::Skeleton * thisclass_cpp = static_cast<Ogre::Skeleton*> (getHG3DClassPtr(*thisclass_c, "Ogre::Skeleton"));
  enum Ogre::SkeletonAnimationBlendMode state_cpp = (enum Ogre::SkeletonAnimationBlendMode)state_c;
  (thisclass_cpp->setBlendMode(state_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_skl_optimiseAllAnimations(struct hg3dclass_struct * thisclass_c, int preservingIdentityNodeTracks_c)
{
  Ogre::Skeleton * thisclass_cpp = static_cast<Ogre::Skeleton*> (getHG3DClassPtr(*thisclass_c, "Ogre::Skeleton"));
  bool preservingIdentityNodeTracks_cpp = (bool)preservingIdentityNodeTracks_c;
  (thisclass_cpp->optimiseAllAnimations(preservingIdentityNodeTracks_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_skl_addLinkedSkeletonAnimationSource(struct hg3dclass_struct * thisclass_c, char * skelName_c, float scale_c)
{
  Ogre::Skeleton * thisclass_cpp = static_cast<Ogre::Skeleton*> (getHG3DClassPtr(*thisclass_c, "Ogre::Skeleton"));
  Ogre::String skelName_cpp = Ogre::String((const char*) skelName_c);
  Real scale_cpp = (Real)scale_c;
  (thisclass_cpp->addLinkedSkeletonAnimationSource(skelName_cpp, scale_cpp));
};

// Remove all links to other skeletons for the purposes of sharing animation. 
extern "C" Ogre_LIB_EXPORT void ogre_skl_removeAllLinkedSkeletonAnimationSources(struct hg3dclass_struct * thisclass_c)
{
  Ogre::Skeleton * thisclass_cpp = static_cast<Ogre::Skeleton*> (getHG3DClassPtr(*thisclass_c, "Ogre::Skeleton"));
  (thisclass_cpp->removeAllLinkedSkeletonAnimationSources());
};

// Have manual bones been modified since the skeleton was last updated? 
extern "C" Ogre_LIB_EXPORT void ogre_skl_getManualBonesDirty(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::Skeleton * thisclass_cpp = static_cast<Ogre::Skeleton*> (getHG3DClassPtr(*thisclass_c, "Ogre::Skeleton"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->getManualBonesDirty());
  *result_c = (int)result_cpp;
};

// Are there any manually controlled bones? 
extern "C" Ogre_LIB_EXPORT void ogre_skl_hasManualBones(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::Skeleton * thisclass_cpp = static_cast<Ogre::Skeleton*> (getHG3DClassPtr(*thisclass_c, "Ogre::Skeleton"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->hasManualBones());
  *result_c = (int)result_cpp;
};

