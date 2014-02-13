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

// EnumBillboardType.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_EnumBillboardType
#define _DEFINED_HG3D_EnumBillboardType


enum EnumBillboardType
{
  BBT_POINT, // Standard point billboard (default), always faces the camera completely and is always upright. 
  BBT_ORIENTED_COMMON, // Billboards are oriented around a shared direction vector (used as Y axis) and only rotate around this to face the camera. 
  BBT_ORIENTED_SELF, // Billboards are oriented around their own direction vector (their own Y axis) and only rotate around this to face the camera. 
  BBT_PERPENDICULAR_COMMON, // Billboards are perpendicular to a shared direction vector (used as Z axis, the facing direction) and X, Y axis are determined by a shared up-vertor. 
  BBT_PERPENDICULAR_SELF // Billboards are perpendicular to their own direction vector (their own Z axis, the facing direction) and X, Y axis are determined by a shared up-vertor. 
};
#endif 
