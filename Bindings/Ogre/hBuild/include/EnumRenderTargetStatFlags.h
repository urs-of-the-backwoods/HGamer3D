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

// EnumRenderTargetStatFlags.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_EnumRenderTargetStatFlags
#define _DEFINED_HG3D_EnumRenderTargetStatFlags


enum EnumRenderTargetStatFlags
{
  SF_NONE =  0, // 
  SF_FPS =  1, // 
  SF_AVG_FPS =  2, // 
  SF_BEST_FPS =  4, // 
  SF_WORST_FPS =  8, // 
  SF_TRIANGLE_COUNT =  16, // 
  SF_ALL =  0xFFFF // 
};
#endif 
