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

// ClassAnimation.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassAnimation
#define _DEFINED_HG3D_ClassAnimation

#include "ClassPtr.h"
#include "ClassNodeAnimationTrack.h"
#include "ClassNumericAnimationTrack.h"
#include "ClassVertexAnimationTrack.h"
#include "EnumVertexAnimationType.h"
#include "ClassNode.h"
#include "ClassSkeleton.h"
#include "ClassEntity.h"
#include "EnumAnimationInterpolationMode.h"
#include "EnumAnimationRotationInterpolationMode.h"


// 
void ogre_anm_destruct(struct hg3dclass_struct * thisclass_c);

// 
void ogre_anm_getName(struct hg3dclass_struct * thisclass_c, char * result_c);

// 
void ogre_anm_getLength(struct hg3dclass_struct * thisclass_c, float * result_c);

// 
void ogre_anm_setLength(struct hg3dclass_struct * thisclass_c, float len_c);

// 
void ogre_anm_createNodeTrack(struct hg3dclass_struct * thisclass_c, unsigned short handle_c, struct hg3dclass_struct * result_c);

// 
void ogre_anm_createNumericTrack(struct hg3dclass_struct * thisclass_c, unsigned short handle_c, struct hg3dclass_struct * result_c);

// 
void ogre_anm_createVertexTrack(struct hg3dclass_struct * thisclass_c, unsigned short handle_c, enum EnumVertexAnimationType animType_c, struct hg3dclass_struct * result_c);

// 
void ogre_anm_createNodeTrack2(struct hg3dclass_struct * thisclass_c, unsigned short handle_c, struct hg3dclass_struct * node_c, struct hg3dclass_struct * result_c);

// 
void ogre_anm_getNumNodeTracks(struct hg3dclass_struct * thisclass_c, unsigned short * result_c);

// 
void ogre_anm_getNodeTrack(struct hg3dclass_struct * thisclass_c, unsigned short handle_c, struct hg3dclass_struct * result_c);

// 
void ogre_anm_hasNodeTrack(struct hg3dclass_struct * thisclass_c, unsigned short handle_c, int * result_c);

// 
void ogre_anm_getNumNumericTracks(struct hg3dclass_struct * thisclass_c, unsigned short * result_c);

// 
void ogre_anm_getNumericTrack(struct hg3dclass_struct * thisclass_c, unsigned short handle_c, struct hg3dclass_struct * result_c);

// 
void ogre_anm_hasNumericTrack(struct hg3dclass_struct * thisclass_c, unsigned short handle_c, int * result_c);

// 
void ogre_anm_getNumVertexTracks(struct hg3dclass_struct * thisclass_c, unsigned short * result_c);

// 
void ogre_anm_getVertexTrack(struct hg3dclass_struct * thisclass_c, unsigned short handle_c, struct hg3dclass_struct * result_c);

// 
void ogre_anm_hasVertexTrack(struct hg3dclass_struct * thisclass_c, unsigned short handle_c, int * result_c);

// 
void ogre_anm_destroyNodeTrack(struct hg3dclass_struct * thisclass_c, unsigned short handle_c);

// 
void ogre_anm_destroyNumericTrack(struct hg3dclass_struct * thisclass_c, unsigned short handle_c);

// 
void ogre_anm_destroyVertexTrack(struct hg3dclass_struct * thisclass_c, unsigned short handle_c);

// 
void ogre_anm_destroyAllTracks(struct hg3dclass_struct * thisclass_c);

// 
void ogre_anm_destroyAllNodeTracks(struct hg3dclass_struct * thisclass_c);

// 
void ogre_anm_destroyAllNumericTracks(struct hg3dclass_struct * thisclass_c);

// 
void ogre_anm_destroyAllVertexTracks(struct hg3dclass_struct * thisclass_c);

// 
void ogre_anm_apply(struct hg3dclass_struct * thisclass_c, float timePos_c, float weight_c, float scale_c);

// 
void ogre_anm_applyToNode(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * node_c, float timePos_c, float weight_c, float scale_c);

// 
void ogre_anm_apply2(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * skeleton_c, float timePos_c, float weight_c, float scale_c);

// 
void ogre_anm_apply4(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * entity_c, float timePos_c, float weight_c, int software_c, int hardware_c);

// 
void ogre_anm_setInterpolationMode(struct hg3dclass_struct * thisclass_c, enum EnumAnimationInterpolationMode im_c);

// 
void ogre_anm_getInterpolationMode(struct hg3dclass_struct * thisclass_c, enum EnumAnimationInterpolationMode * result_c);

// 
void ogre_anm_setRotationInterpolationMode(struct hg3dclass_struct * thisclass_c, enum EnumAnimationRotationInterpolationMode im_c);

// 
void ogre_anm_getRotationInterpolationMode(struct hg3dclass_struct * thisclass_c, enum EnumAnimationRotationInterpolationMode * result_c);

// 
void ogre_anm_optimise(struct hg3dclass_struct * thisclass_c, int discardIdentityNodeTracks_c);

// 
void ogre_anm_clone(struct hg3dclass_struct * thisclass_c, char * newName_c, struct hg3dclass_struct * result_c);

// 
void ogre_anm_setUseBaseKeyFrame(struct hg3dclass_struct * thisclass_c, int useBaseKeyFrame_c, float keyframeTime_c, char * baseAnimName_c);

// 
void ogre_anm_getUseBaseKeyFrame(struct hg3dclass_struct * thisclass_c, int * result_c);

// 
void ogre_anm_getBaseKeyFrameTime(struct hg3dclass_struct * thisclass_c, float * result_c);

// 
void ogre_anm_getBaseKeyFrameAnimationName(struct hg3dclass_struct * thisclass_c, char * result_c);

// 
void ogre_anm_setDefaultInterpolationMode(enum EnumAnimationInterpolationMode im_c);

// 
void ogre_anm_getDefaultInterpolationMode(enum EnumAnimationInterpolationMode * result_c);

// 
void ogre_anm_setDefaultRotationInterpolationMode(enum EnumAnimationRotationInterpolationMode im_c);

// 
void ogre_anm_getDefaultRotationInterpolationMode(enum EnumAnimationRotationInterpolationMode * result_c);

#endif 
