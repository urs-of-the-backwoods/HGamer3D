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

// ClassAnimationState.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassAnimationState
#define _DEFINED_HG3D_ClassAnimationState

#include "ClassPtr.h"
#include "ClassAnimationStateSet.h"


// 
void ogre_anms_construct(char * animName_c, struct hg3dclass_struct * parent_c, float timePos_c, float length_c, float weight_c, long enabled_c, struct hg3dclass_struct * result_c);

// 
void ogre_anms_destruct(struct hg3dclass_struct * thisclass_c);

// Gets the name of the animation to which this state applies. 
void ogre_anms_getAnimationName(struct hg3dclass_struct * thisclass_c, char * result_c);

// Gets the time position for this animation. 
void ogre_anms_getTimePosition(struct hg3dclass_struct * thisclass_c, float * result_c);

// Sets the time position for this animation. 
void ogre_anms_setTimePosition(struct hg3dclass_struct * thisclass_c, float timePos_c);

// Gets the total length of this animation (may be shorter than whole animation) 
void ogre_anms_getLength(struct hg3dclass_struct * thisclass_c, float * result_c);

// Sets the total length of this animation (may be shorter than whole animation) 
void ogre_anms_setLength(struct hg3dclass_struct * thisclass_c, float len_c);

// Gets the weight (influence) of this animation. 
void ogre_anms_getWeight(struct hg3dclass_struct * thisclass_c, float * result_c);

// Sets the weight (influence) of this animation. 
void ogre_anms_setWeight(struct hg3dclass_struct * thisclass_c, float weight_c);

// 
void ogre_anms_addTime(struct hg3dclass_struct * thisclass_c, float offset_c);

// Returns true if the animation has reached the end and is not looping. 
void ogre_anms_hasEnded(struct hg3dclass_struct * thisclass_c, long * result_c);

// Returns true if this animation is currently enabled. 
void ogre_anms_getEnabled(struct hg3dclass_struct * thisclass_c, long * result_c);

// Sets whether this animation is enabled. 
void ogre_anms_setEnabled(struct hg3dclass_struct * thisclass_c, long enabled_c);

// 
void ogre_anms_setLoop(struct hg3dclass_struct * thisclass_c, long loop_c);

// Gets whether or not this animation loops. 
void ogre_anms_getLoop(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_anms_copyStateFrom(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * animState_c);

// Get the parent animation state set. 
void ogre_anms_getParent(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// Create a new blend mask with the given number of entries. 
void ogre_anms_createBlendMask(struct hg3dclass_struct * thisclass_c, long blendMaskSizeHint_c, float initialWeight_c);

// Destroy the currently set blend mask. 
void ogre_anms_destroyBlendMask(struct hg3dclass_struct * thisclass_c);

// Return whether there is currently a valid blend mask set. 
void ogre_anms_hasBlendMask(struct hg3dclass_struct * thisclass_c, long * result_c);

// Set the weight for the bone identified by the given handle. 
void ogre_anms_setBlendMaskEntry(struct hg3dclass_struct * thisclass_c, long boneHandle_c, float weight_c);

// Get the weight for the bone identified by the given handle. 
void ogre_anms_getBlendMaskEntry(struct hg3dclass_struct * thisclass_c, long boneHandle_c, float * result_c);

#endif 
