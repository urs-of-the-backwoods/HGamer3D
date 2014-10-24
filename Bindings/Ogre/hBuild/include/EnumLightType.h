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

// EnumLightType.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_EnumLightType
#define _DEFINED_HG3D_EnumLightType


enum EnumLightType
{
  LT_POINT =  0, // Point light sources give off light equally in all directions, so require only position not direction. 
  LT_DIRECTIONAL =  1, // Directional lights simulate parallel light beams from a distant source, hence have direction but no position. 
  LT_SPOTLIGHT =  2 // Spotlights simulate a cone of light from a source so require position and direction, plus extra values for falloff. 
};
#endif 
