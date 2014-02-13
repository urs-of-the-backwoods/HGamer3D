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

// ClassAnimationStateSet.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassAnimationStateSet
#define _DEFINED_HG3D_ClassAnimationStateSet

#include "ClassPtr.h"
#include "ClassAnimationState.h"


// 
void ogre_ass_destruct(struct hg3dclass_struct * thisclass_c);

// 
void ogre_ass_createAnimationState(struct hg3dclass_struct * thisclass_c, char * animName_c, float timePos_c, float length_c, float weight_c, int enabled_c, struct hg3dclass_struct * result_c);

// Get an animation state by the name of the animation. 
void ogre_ass_getAnimationState(struct hg3dclass_struct * thisclass_c, char * name_c, struct hg3dclass_struct * result_c);

// Tests if state for the named animation is present. 
void ogre_ass_hasAnimationState(struct hg3dclass_struct * thisclass_c, char * name_c, int * result_c);

// Remove animation state with the given name. 
void ogre_ass_removeAnimationState(struct hg3dclass_struct * thisclass_c, char * name_c);

// Remove all animation states. 
void ogre_ass_removeAllAnimationStates(struct hg3dclass_struct * thisclass_c);

// Copy the state of any matching animation states from this to another. 
void ogre_ass_copyMatchingState(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * target_c);

// Tests if exists enabled animation state in this set. 
void ogre_ass_hasEnabledAnimationState(struct hg3dclass_struct * thisclass_c, int * result_c);

#endif 
