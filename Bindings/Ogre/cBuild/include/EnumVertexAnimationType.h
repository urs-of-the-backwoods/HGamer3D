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

// EnumVertexAnimationType.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_EnumVertexAnimationType
#define _DEFINED_HG3D_EnumVertexAnimationType


enum EnumVertexAnimationType
{
  VAT_NONE =  0, // No animation. 
  VAT_MORPH =  1, // Morph animation is made up of many interpolated snapshot keyframes. 
  VAT_POSE =  2 // Pose animation is made up of a single delta pose keyframe. 
};
#endif 
