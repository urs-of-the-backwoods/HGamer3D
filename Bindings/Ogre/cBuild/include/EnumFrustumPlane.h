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

// EnumFrustumPlane.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_EnumFrustumPlane
#define _DEFINED_HG3D_EnumFrustumPlane


enum EnumFrustumPlane
{
  FRUSTUM_PLANE_NEAR =  0, // 
  FRUSTUM_PLANE_FAR =  1, // 
  FRUSTUM_PLANE_LEFT =  2, // 
  FRUSTUM_PLANE_RIGHT =  3, // 
  FRUSTUM_PLANE_TOP =  4, // 
  FRUSTUM_PLANE_BOTTOM =  5 // 
};
#endif 
