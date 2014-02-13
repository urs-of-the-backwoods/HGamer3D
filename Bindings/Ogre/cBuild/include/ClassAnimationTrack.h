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

// ClassAnimationTrack.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassAnimationTrack
#define _DEFINED_HG3D_ClassAnimationTrack

#include "ClassPtr.h"
#include "ClassTimeIndex.h"
#include "ClassAnimation.h"


// 
void ogre_at_destruct(struct hg3dclass_struct * thisclass_c);

// 
void ogre_at_getHandle(struct hg3dclass_struct * thisclass_c, unsigned short * result_c);

// 
void ogre_at_getNumKeyFrames(struct hg3dclass_struct * thisclass_c, unsigned short * result_c);

// 
void ogre_at_removeKeyFrame(struct hg3dclass_struct * thisclass_c, unsigned short index_c);

// 
void ogre_at_removeAllKeyFrames(struct hg3dclass_struct * thisclass_c);

// 
void ogre_at_apply(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * timeIndex_c, float weight_c, float scale_c);

// 
void ogre_at_hasNonZeroKeyFrames(struct hg3dclass_struct * thisclass_c, int * result_c);

// 
void ogre_at_optimise(struct hg3dclass_struct * thisclass_c);

// 
void ogre_at_getParent(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

#endif 
