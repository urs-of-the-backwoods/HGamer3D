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

// EnumSDLGameControllerAxis.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_EnumSDLGameControllerAxis
#define _DEFINED_HG3D_EnumSDLGameControllerAxis


enum EnumSDLGameControllerAxis
{
  SDL_CONTROLLER_AXIS_INVALID =  -1, // 
  SDL_CONTROLLER_AXIS_LEFTX, // 
  SDL_CONTROLLER_AXIS_LEFTY, // 
  SDL_CONTROLLER_AXIS_RIGHTX, // 
  SDL_CONTROLLER_AXIS_RIGHTY, // 
  SDL_CONTROLLER_AXIS_TRIGGERLEFT, // 
  SDL_CONTROLLER_AXIS_TRIGGERRIGHT, // 
  SDL_CONTROLLER_AXIS_MAX // 
};
#endif 
