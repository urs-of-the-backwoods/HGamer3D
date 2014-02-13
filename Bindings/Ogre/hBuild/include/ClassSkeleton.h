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

// ClassSkeleton.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassSkeleton
#define _DEFINED_HG3D_ClassSkeleton

#include "ClassPtr.h"
#include "ClassBone.h"
#include "ClassAnimation.h"
#include "ClassAnimationStateSet.h"
#include "EnumSkeletonAnimationBlendMode.h"


// 
void ogre_skl_destruct(struct hg3dclass_struct * thisclass_c);

// 
void ogre_skl_createBone(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// 
void ogre_skl_createBone2(struct hg3dclass_struct * thisclass_c, unsigned short handle_c, struct hg3dclass_struct * result_c);

// 
void ogre_skl_createBone3(struct hg3dclass_struct * thisclass_c, char * name_c, struct hg3dclass_struct * result_c);

// 
void ogre_skl_createBone4(struct hg3dclass_struct * thisclass_c, char * name_c, unsigned short handle_c, struct hg3dclass_struct * result_c);

// 
void ogre_skl_getNumBones(struct hg3dclass_struct * thisclass_c, unsigned short * result_c);

// 
void ogre_skl_getRootBone(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// 
void ogre_skl_getBone(struct hg3dclass_struct * thisclass_c, unsigned short handle_c, struct hg3dclass_struct * result_c);

// 
void ogre_skl_getBone2(struct hg3dclass_struct * thisclass_c, char * name_c, struct hg3dclass_struct * result_c);

// 
void ogre_skl_hasBone(struct hg3dclass_struct * thisclass_c, char * name_c, int * result_c);

// 
void ogre_skl_setBindingPose(struct hg3dclass_struct * thisclass_c);

// 
void ogre_skl_reset(struct hg3dclass_struct * thisclass_c, int resetManualBones_c);

// 
void ogre_skl_createAnimation(struct hg3dclass_struct * thisclass_c, char * name_c, float length_c, struct hg3dclass_struct * result_c);

// 
void ogre_skl_getAnimation2(struct hg3dclass_struct * thisclass_c, char * name_c, struct hg3dclass_struct * result_c);

// 
void ogre_skl_hasAnimation(struct hg3dclass_struct * thisclass_c, char * name_c, int * result_c);

// 
void ogre_skl_removeAnimation(struct hg3dclass_struct * thisclass_c, char * name_c);

// 
void ogre_skl_setAnimationState(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * animSet_c);

// 
void ogre_skl_getNumAnimations(struct hg3dclass_struct * thisclass_c, unsigned short * result_c);

// 
void ogre_skl_getAnimation3(struct hg3dclass_struct * thisclass_c, unsigned short index_c, struct hg3dclass_struct * result_c);

// 
void ogre_skl_getBlendMode(struct hg3dclass_struct * thisclass_c, enum EnumSkeletonAnimationBlendMode * result_c);

// 
void ogre_skl_setBlendMode(struct hg3dclass_struct * thisclass_c, enum EnumSkeletonAnimationBlendMode state_c);

// 
void ogre_skl_optimiseAllAnimations(struct hg3dclass_struct * thisclass_c, int preservingIdentityNodeTracks_c);

// 
void ogre_skl_addLinkedSkeletonAnimationSource(struct hg3dclass_struct * thisclass_c, char * skelName_c, float scale_c);

// Remove all links to other skeletons for the purposes of sharing animation. 
void ogre_skl_removeAllLinkedSkeletonAnimationSources(struct hg3dclass_struct * thisclass_c);

// Have manual bones been modified since the skeleton was last updated? 
void ogre_skl_getManualBonesDirty(struct hg3dclass_struct * thisclass_c, int * result_c);

// Are there any manually controlled bones? 
void ogre_skl_hasManualBones(struct hg3dclass_struct * thisclass_c, int * result_c);

#endif 
