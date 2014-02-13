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

// EnumStencilOperation.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_EnumStencilOperation
#define _DEFINED_HG3D_EnumStencilOperation


enum EnumStencilOperation
{
  SOP_KEEP, // Leave the stencil buffer unchanged. 
  SOP_ZERO, // Set the stencil value to zero. 
  SOP_REPLACE, // Set the stencil value to the reference value. 
  SOP_INCREMENT, // Increase the stencil value by 1, clamping at the maximum value. 
  SOP_DECREMENT, // Decrease the stencil value by 1, clamping at 0. 
  SOP_INCREMENT_WRAP, // Increase the stencil value by 1, wrapping back to 0 when incrementing the maximum value. 
  SOP_DECREMENT_WRAP, // Decrease the stencil value by 1, wrapping when decrementing 0. 
  SOP_INVERT // Invert the bits of the stencil buffer. 
};
#endif 
