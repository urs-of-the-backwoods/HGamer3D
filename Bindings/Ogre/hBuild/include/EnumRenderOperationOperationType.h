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

// EnumRenderOperationOperationType.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_EnumRenderOperationOperationType
#define _DEFINED_HG3D_EnumRenderOperationOperationType


enum EnumRenderOperationOperationType
{
  OT_POINT_LIST =  1, // A list of points, 1 vertex per point. 
  OT_LINE_LIST =  2, // A list of lines, 2 vertices per line. 
  OT_LINE_STRIP =  3, // A strip of connected lines, 1 vertex per line plus 1 start vertex. 
  OT_TRIANGLE_LIST =  4, // A list of triangles, 3 vertices per triangle. 
  OT_TRIANGLE_STRIP =  5, // A strip of triangles, 3 vertices for the first triangle, and 1 per triangle after that. 
  OT_TRIANGLE_FAN =  6 // A fan of triangles, 3 vertices for the first triangle, and 1 per triangle after that. 
};
#endif 
