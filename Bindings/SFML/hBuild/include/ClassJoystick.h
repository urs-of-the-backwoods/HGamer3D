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

// ClassJoystick.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassJoystick
#define _DEFINED_HG3D_ClassJoystick

#include "ClassPtr.h"
#include "EnumJoystickAxis.h"


// Check if a joystick is connected. 
void sfml_jst_isConnected(unsigned int joystick_c, int * result_c);

// Return the number of buttons supported by a joystick. 
void sfml_jst_getButtonCount(unsigned int joystick_c, unsigned int * result_c);

// Check if a joystick supports a given axis. 
void sfml_jst_hasAxis(unsigned int joystick_c, enum EnumJoystickAxis axis_c, int * result_c);

// Check if a joystick button is pressed. 
void sfml_jst_isButtonPressed(unsigned int joystick_c, unsigned int button_c, int * result_c);

// Get the current position of a joystick axis. 
void sfml_jst_getAxisPosition(unsigned int joystick_c, enum EnumJoystickAxis axis_c, float * result_c);

// Update the states of all joysticks. 
void sfml_jst_update();

#endif 
