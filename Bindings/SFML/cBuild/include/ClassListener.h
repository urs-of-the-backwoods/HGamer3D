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

// ClassListener.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassListener
#define _DEFINED_HG3D_ClassListener

#include "ClassPtr.h"
#include "StructVec3.h"


// Change the global volume of all the sounds and musics. 
void sfml_lst_setGlobalVolume(float volume_c);

// Get the current value of the global volume. 
void sfml_lst_getGlobalVolume(float * result_c);

// Set the position of the listener in the scene. 
void sfml_lst_setPosition(float x_c, float y_c, float z_c);

// Get the current position of the listener in the scene. 
void sfml_lst_getPosition(struct vector3f_struct * result_c);

// Set the orientation of the listener in the scene. 
void sfml_lst_setDirection(float x_c, float y_c, float z_c);

// Get the current orientation of the listener in the scene. 
void sfml_lst_getDirection(struct vector3f_struct * result_c);

#endif 
