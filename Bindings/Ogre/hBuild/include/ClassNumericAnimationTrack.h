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

// ClassNumericAnimationTrack.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassNumericAnimationTrack
#define _DEFINED_HG3D_ClassNumericAnimationTrack

#include "ClassPtr.h"
#include "ClassAnimation.h"
#include "ClassTimeIndex.h"


// Constructor. 
void ogre_nuat_construct(struct hg3dclass_struct * parent_c, unsigned short handle_c, struct hg3dclass_struct * result_c);

// 
void ogre_nuat_apply(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * timeIndex_c, float weight_c, float scale_c);

#endif 
