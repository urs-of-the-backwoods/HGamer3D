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

// ClassAnimation.cpp

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
	#include "EnumVertexAnimationType.h"
#include "EnumAnimationInterpolationMode.h"
#include "EnumAnimationRotationInterpolationMode.h"
#include "./Ogre.h"
#include "./OgreString.h"
#include "./WindowUtilsHG3D.h"
#include "HG3DUtilities.h"

using namespace Ogre;



// 
extern "C" Ogre_LIB_EXPORT void ogre_anm_destruct(struct hg3dclass_struct * thisclass_c)
{
  Ogre::Animation * thisclass_cpp = static_cast<Ogre::Animation*> (getHG3DClassPtr(*thisclass_c, "Ogre::Animation"));
  (delete thisclass_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_anm_getName(struct hg3dclass_struct * thisclass_c, char * result_c)
{
  Ogre::Animation * thisclass_cpp = static_cast<Ogre::Animation*> (getHG3DClassPtr(*thisclass_c, "Ogre::Animation"));
  Ogre::String result_cpp;
  result_cpp = (thisclass_cpp->getName());
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_anm_getLength(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  Ogre::Animation * thisclass_cpp = static_cast<Ogre::Animation*> (getHG3DClassPtr(*thisclass_c, "Ogre::Animation"));
  Real result_cpp;
  result_cpp = (thisclass_cpp->getLength());
  *result_c = (float)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_anm_setLength(struct hg3dclass_struct * thisclass_c, float len_c)
{
  Ogre::Animation * thisclass_cpp = static_cast<Ogre::Animation*> (getHG3DClassPtr(*thisclass_c, "Ogre::Animation"));
  Real len_cpp = (Real)len_c;
  (thisclass_cpp->setLength(len_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_anm_createNodeTrack(struct hg3dclass_struct * thisclass_c, unsigned short handle_c, struct hg3dclass_struct * result_c)
{
  Ogre::Animation * thisclass_cpp = static_cast<Ogre::Animation*> (getHG3DClassPtr(*thisclass_c, "Ogre::Animation"));
  unsigned short handle_cpp = (unsigned short)handle_c;
  Ogre::NodeAnimationTrack * result_cpp;
  result_cpp = (thisclass_cpp->createNodeTrack(handle_cpp));
  *result_c = getHG3DClass_NodeAnimationTrack((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_anm_createNumericTrack(struct hg3dclass_struct * thisclass_c, unsigned short handle_c, struct hg3dclass_struct * result_c)
{
  Ogre::Animation * thisclass_cpp = static_cast<Ogre::Animation*> (getHG3DClassPtr(*thisclass_c, "Ogre::Animation"));
  unsigned short handle_cpp = (unsigned short)handle_c;
  Ogre::NumericAnimationTrack * result_cpp;
  result_cpp = (thisclass_cpp->createNumericTrack(handle_cpp));
  *result_c = getHG3DClass_NumericAnimationTrack((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_anm_createVertexTrack(struct hg3dclass_struct * thisclass_c, unsigned short handle_c, enum EnumVertexAnimationType animType_c, struct hg3dclass_struct * result_c)
{
  Ogre::Animation * thisclass_cpp = static_cast<Ogre::Animation*> (getHG3DClassPtr(*thisclass_c, "Ogre::Animation"));
  unsigned short handle_cpp = (unsigned short)handle_c;
  enum Ogre::VertexAnimationType animType_cpp = (enum Ogre::VertexAnimationType)animType_c;
  Ogre::VertexAnimationTrack * result_cpp;
  result_cpp = (thisclass_cpp->createVertexTrack(handle_cpp, animType_cpp));
  *result_c = getHG3DClass_VertexAnimationTrack((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_anm_createNodeTrack2(struct hg3dclass_struct * thisclass_c, unsigned short handle_c, struct hg3dclass_struct * node_c, struct hg3dclass_struct * result_c)
{
  Ogre::Animation * thisclass_cpp = static_cast<Ogre::Animation*> (getHG3DClassPtr(*thisclass_c, "Ogre::Animation"));
  unsigned short handle_cpp = (unsigned short)handle_c;
  Ogre::Node * node_cpp = static_cast<Ogre::Node*> (getHG3DClassPtr(*node_c, "Ogre::Node"));
  Ogre::NodeAnimationTrack * result_cpp;
  result_cpp = (thisclass_cpp->createNodeTrack(handle_cpp, node_cpp));
  *result_c = getHG3DClass_NodeAnimationTrack((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_anm_getNumNodeTracks(struct hg3dclass_struct * thisclass_c, unsigned short * result_c)
{
  Ogre::Animation * thisclass_cpp = static_cast<Ogre::Animation*> (getHG3DClassPtr(*thisclass_c, "Ogre::Animation"));
  unsigned short result_cpp;
  result_cpp = (thisclass_cpp->getNumNodeTracks());
  *result_c = (unsigned short)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_anm_getNodeTrack(struct hg3dclass_struct * thisclass_c, unsigned short handle_c, struct hg3dclass_struct * result_c)
{
  Ogre::Animation * thisclass_cpp = static_cast<Ogre::Animation*> (getHG3DClassPtr(*thisclass_c, "Ogre::Animation"));
  unsigned short handle_cpp = (unsigned short)handle_c;
  Ogre::NodeAnimationTrack * result_cpp;
  result_cpp = (thisclass_cpp->getNodeTrack(handle_cpp));
  *result_c = getHG3DClass_NodeAnimationTrack((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_anm_hasNodeTrack(struct hg3dclass_struct * thisclass_c, unsigned short handle_c, int * result_c)
{
  Ogre::Animation * thisclass_cpp = static_cast<Ogre::Animation*> (getHG3DClassPtr(*thisclass_c, "Ogre::Animation"));
  unsigned short handle_cpp = (unsigned short)handle_c;
  bool result_cpp;
  result_cpp = (thisclass_cpp->hasNodeTrack(handle_cpp));
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_anm_getNumNumericTracks(struct hg3dclass_struct * thisclass_c, unsigned short * result_c)
{
  Ogre::Animation * thisclass_cpp = static_cast<Ogre::Animation*> (getHG3DClassPtr(*thisclass_c, "Ogre::Animation"));
  unsigned short result_cpp;
  result_cpp = (thisclass_cpp->getNumNumericTracks());
  *result_c = (unsigned short)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_anm_getNumericTrack(struct hg3dclass_struct * thisclass_c, unsigned short handle_c, struct hg3dclass_struct * result_c)
{
  Ogre::Animation * thisclass_cpp = static_cast<Ogre::Animation*> (getHG3DClassPtr(*thisclass_c, "Ogre::Animation"));
  unsigned short handle_cpp = (unsigned short)handle_c;
  Ogre::NumericAnimationTrack * result_cpp;
  result_cpp = (thisclass_cpp->getNumericTrack(handle_cpp));
  *result_c = getHG3DClass_NumericAnimationTrack((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_anm_hasNumericTrack(struct hg3dclass_struct * thisclass_c, unsigned short handle_c, int * result_c)
{
  Ogre::Animation * thisclass_cpp = static_cast<Ogre::Animation*> (getHG3DClassPtr(*thisclass_c, "Ogre::Animation"));
  unsigned short handle_cpp = (unsigned short)handle_c;
  bool result_cpp;
  result_cpp = (thisclass_cpp->hasNumericTrack(handle_cpp));
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_anm_getNumVertexTracks(struct hg3dclass_struct * thisclass_c, unsigned short * result_c)
{
  Ogre::Animation * thisclass_cpp = static_cast<Ogre::Animation*> (getHG3DClassPtr(*thisclass_c, "Ogre::Animation"));
  unsigned short result_cpp;
  result_cpp = (thisclass_cpp->getNumVertexTracks());
  *result_c = (unsigned short)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_anm_getVertexTrack(struct hg3dclass_struct * thisclass_c, unsigned short handle_c, struct hg3dclass_struct * result_c)
{
  Ogre::Animation * thisclass_cpp = static_cast<Ogre::Animation*> (getHG3DClassPtr(*thisclass_c, "Ogre::Animation"));
  unsigned short handle_cpp = (unsigned short)handle_c;
  Ogre::VertexAnimationTrack * result_cpp;
  result_cpp = (thisclass_cpp->getVertexTrack(handle_cpp));
  *result_c = getHG3DClass_VertexAnimationTrack((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_anm_hasVertexTrack(struct hg3dclass_struct * thisclass_c, unsigned short handle_c, int * result_c)
{
  Ogre::Animation * thisclass_cpp = static_cast<Ogre::Animation*> (getHG3DClassPtr(*thisclass_c, "Ogre::Animation"));
  unsigned short handle_cpp = (unsigned short)handle_c;
  bool result_cpp;
  result_cpp = (thisclass_cpp->hasVertexTrack(handle_cpp));
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_anm_destroyNodeTrack(struct hg3dclass_struct * thisclass_c, unsigned short handle_c)
{
  Ogre::Animation * thisclass_cpp = static_cast<Ogre::Animation*> (getHG3DClassPtr(*thisclass_c, "Ogre::Animation"));
  unsigned short handle_cpp = (unsigned short)handle_c;
  (thisclass_cpp->destroyNodeTrack(handle_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_anm_destroyNumericTrack(struct hg3dclass_struct * thisclass_c, unsigned short handle_c)
{
  Ogre::Animation * thisclass_cpp = static_cast<Ogre::Animation*> (getHG3DClassPtr(*thisclass_c, "Ogre::Animation"));
  unsigned short handle_cpp = (unsigned short)handle_c;
  (thisclass_cpp->destroyNumericTrack(handle_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_anm_destroyVertexTrack(struct hg3dclass_struct * thisclass_c, unsigned short handle_c)
{
  Ogre::Animation * thisclass_cpp = static_cast<Ogre::Animation*> (getHG3DClassPtr(*thisclass_c, "Ogre::Animation"));
  unsigned short handle_cpp = (unsigned short)handle_c;
  (thisclass_cpp->destroyVertexTrack(handle_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_anm_destroyAllTracks(struct hg3dclass_struct * thisclass_c)
{
  Ogre::Animation * thisclass_cpp = static_cast<Ogre::Animation*> (getHG3DClassPtr(*thisclass_c, "Ogre::Animation"));
  (thisclass_cpp->destroyAllTracks());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_anm_destroyAllNodeTracks(struct hg3dclass_struct * thisclass_c)
{
  Ogre::Animation * thisclass_cpp = static_cast<Ogre::Animation*> (getHG3DClassPtr(*thisclass_c, "Ogre::Animation"));
  (thisclass_cpp->destroyAllNodeTracks());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_anm_destroyAllNumericTracks(struct hg3dclass_struct * thisclass_c)
{
  Ogre::Animation * thisclass_cpp = static_cast<Ogre::Animation*> (getHG3DClassPtr(*thisclass_c, "Ogre::Animation"));
  (thisclass_cpp->destroyAllNumericTracks());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_anm_destroyAllVertexTracks(struct hg3dclass_struct * thisclass_c)
{
  Ogre::Animation * thisclass_cpp = static_cast<Ogre::Animation*> (getHG3DClassPtr(*thisclass_c, "Ogre::Animation"));
  (thisclass_cpp->destroyAllVertexTracks());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_anm_apply(struct hg3dclass_struct * thisclass_c, float timePos_c, float weight_c, float scale_c)
{
  Ogre::Animation * thisclass_cpp = static_cast<Ogre::Animation*> (getHG3DClassPtr(*thisclass_c, "Ogre::Animation"));
  Real timePos_cpp = (Real)timePos_c;
  Real weight_cpp = (Real)weight_c;
  Real scale_cpp = (Real)scale_c;
  (thisclass_cpp->apply(timePos_cpp, weight_cpp, scale_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_anm_applyToNode(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * node_c, float timePos_c, float weight_c, float scale_c)
{
  Ogre::Animation * thisclass_cpp = static_cast<Ogre::Animation*> (getHG3DClassPtr(*thisclass_c, "Ogre::Animation"));
  Ogre::Node * node_cpp = static_cast<Ogre::Node*> (getHG3DClassPtr(*node_c, "Ogre::Node"));
  Real timePos_cpp = (Real)timePos_c;
  Real weight_cpp = (Real)weight_c;
  Real scale_cpp = (Real)scale_c;
  (thisclass_cpp->applyToNode(node_cpp, timePos_cpp, weight_cpp, scale_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_anm_apply2(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * skeleton_c, float timePos_c, float weight_c, float scale_c)
{
  Ogre::Animation * thisclass_cpp = static_cast<Ogre::Animation*> (getHG3DClassPtr(*thisclass_c, "Ogre::Animation"));
  Ogre::Skeleton * skeleton_cpp = static_cast<Ogre::Skeleton*> (getHG3DClassPtr(*skeleton_c, "Ogre::Skeleton"));
  Real timePos_cpp = (Real)timePos_c;
  Real weight_cpp = (Real)weight_c;
  Real scale_cpp = (Real)scale_c;
  (thisclass_cpp->apply(skeleton_cpp, timePos_cpp, weight_cpp, scale_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_anm_apply4(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * entity_c, float timePos_c, float weight_c, int software_c, int hardware_c)
{
  Ogre::Animation * thisclass_cpp = static_cast<Ogre::Animation*> (getHG3DClassPtr(*thisclass_c, "Ogre::Animation"));
  Ogre::Entity * entity_cpp = static_cast<Ogre::Entity*> (getHG3DClassPtr(*entity_c, "Ogre::Entity"));
  Real timePos_cpp = (Real)timePos_c;
  Real weight_cpp = (Real)weight_c;
  bool software_cpp = (bool)software_c;
  bool hardware_cpp = (bool)hardware_c;
  (thisclass_cpp->apply(entity_cpp, timePos_cpp, weight_cpp, software_cpp, hardware_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_anm_setInterpolationMode(struct hg3dclass_struct * thisclass_c, enum EnumAnimationInterpolationMode im_c)
{
  Ogre::Animation * thisclass_cpp = static_cast<Ogre::Animation*> (getHG3DClassPtr(*thisclass_c, "Ogre::Animation"));
  enum Ogre::Animation::InterpolationMode im_cpp = (enum Ogre::Animation::InterpolationMode)im_c;
  (thisclass_cpp->setInterpolationMode(im_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_anm_getInterpolationMode(struct hg3dclass_struct * thisclass_c, enum EnumAnimationInterpolationMode * result_c)
{
  Ogre::Animation * thisclass_cpp = static_cast<Ogre::Animation*> (getHG3DClassPtr(*thisclass_c, "Ogre::Animation"));
  enum Ogre::Animation::InterpolationMode result_cpp;
  result_cpp = (thisclass_cpp->getInterpolationMode());
  *result_c = (enum EnumAnimationInterpolationMode) result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_anm_setRotationInterpolationMode(struct hg3dclass_struct * thisclass_c, enum EnumAnimationRotationInterpolationMode im_c)
{
  Ogre::Animation * thisclass_cpp = static_cast<Ogre::Animation*> (getHG3DClassPtr(*thisclass_c, "Ogre::Animation"));
  enum Ogre::Animation::RotationInterpolationMode im_cpp = (enum Ogre::Animation::RotationInterpolationMode)im_c;
  (thisclass_cpp->setRotationInterpolationMode(im_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_anm_getRotationInterpolationMode(struct hg3dclass_struct * thisclass_c, enum EnumAnimationRotationInterpolationMode * result_c)
{
  Ogre::Animation * thisclass_cpp = static_cast<Ogre::Animation*> (getHG3DClassPtr(*thisclass_c, "Ogre::Animation"));
  enum Ogre::Animation::RotationInterpolationMode result_cpp;
  result_cpp = (thisclass_cpp->getRotationInterpolationMode());
  *result_c = (enum EnumAnimationRotationInterpolationMode) result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_anm_optimise(struct hg3dclass_struct * thisclass_c, int discardIdentityNodeTracks_c)
{
  Ogre::Animation * thisclass_cpp = static_cast<Ogre::Animation*> (getHG3DClassPtr(*thisclass_c, "Ogre::Animation"));
  bool discardIdentityNodeTracks_cpp = (bool)discardIdentityNodeTracks_c;
  (thisclass_cpp->optimise(discardIdentityNodeTracks_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_anm_clone(struct hg3dclass_struct * thisclass_c, char * newName_c, struct hg3dclass_struct * result_c)
{
  Ogre::Animation * thisclass_cpp = static_cast<Ogre::Animation*> (getHG3DClassPtr(*thisclass_c, "Ogre::Animation"));
  Ogre::String newName_cpp = Ogre::String((const char*) newName_c);
  Ogre::Animation * result_cpp;
  result_cpp = (thisclass_cpp->clone(newName_cpp));
  *result_c = getHG3DClass_Animation((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_anm_setUseBaseKeyFrame(struct hg3dclass_struct * thisclass_c, int useBaseKeyFrame_c, float keyframeTime_c, char * baseAnimName_c)
{
  Ogre::Animation * thisclass_cpp = static_cast<Ogre::Animation*> (getHG3DClassPtr(*thisclass_c, "Ogre::Animation"));
  bool useBaseKeyFrame_cpp = (bool)useBaseKeyFrame_c;
  Real keyframeTime_cpp = (Real)keyframeTime_c;
  Ogre::String baseAnimName_cpp = Ogre::String((const char*) baseAnimName_c);
  (thisclass_cpp->setUseBaseKeyFrame(useBaseKeyFrame_cpp, keyframeTime_cpp, baseAnimName_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_anm_getUseBaseKeyFrame(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::Animation * thisclass_cpp = static_cast<Ogre::Animation*> (getHG3DClassPtr(*thisclass_c, "Ogre::Animation"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->getUseBaseKeyFrame());
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_anm_getBaseKeyFrameTime(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  Ogre::Animation * thisclass_cpp = static_cast<Ogre::Animation*> (getHG3DClassPtr(*thisclass_c, "Ogre::Animation"));
  Real result_cpp;
  result_cpp = (thisclass_cpp->getBaseKeyFrameTime());
  *result_c = (float)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_anm_getBaseKeyFrameAnimationName(struct hg3dclass_struct * thisclass_c, char * result_c)
{
  Ogre::Animation * thisclass_cpp = static_cast<Ogre::Animation*> (getHG3DClassPtr(*thisclass_c, "Ogre::Animation"));
  Ogre::String result_cpp;
  result_cpp = (thisclass_cpp->getBaseKeyFrameAnimationName());
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_anm_setDefaultInterpolationMode(enum EnumAnimationInterpolationMode im_c)
{
  enum Ogre::Animation::InterpolationMode im_cpp = (enum Ogre::Animation::InterpolationMode)im_c;
  (Ogre::Animation::setDefaultInterpolationMode(im_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_anm_getDefaultInterpolationMode(enum EnumAnimationInterpolationMode * result_c)
{
  enum Ogre::Animation::InterpolationMode result_cpp;
  result_cpp = (Ogre::Animation::getDefaultInterpolationMode());
  *result_c = (enum EnumAnimationInterpolationMode) result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_anm_setDefaultRotationInterpolationMode(enum EnumAnimationRotationInterpolationMode im_c)
{
  enum Ogre::Animation::RotationInterpolationMode im_cpp = (enum Ogre::Animation::RotationInterpolationMode)im_c;
  (Ogre::Animation::setDefaultRotationInterpolationMode(im_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_anm_getDefaultRotationInterpolationMode(enum EnumAnimationRotationInterpolationMode * result_c)
{
  enum Ogre::Animation::RotationInterpolationMode result_cpp;
  result_cpp = (Ogre::Animation::getDefaultRotationInterpolationMode());
  *result_c = (enum EnumAnimationRotationInterpolationMode) result_cpp;
};

