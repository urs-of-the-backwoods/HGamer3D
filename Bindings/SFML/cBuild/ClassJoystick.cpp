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

// ClassJoystick.cpp

// 

#include <wchar.h>
#include <string>
#include <iostream>

#include <iostream>
	#include <typeinfo>
	#include <stdio.h>
	#include <cstring>
	#include <exception>
	#include "SFMLDllDefines.h"
	#include "ClassPtr.h"
	#include "EnumJoystickAxis.h"
#include "SFML/Audio.hpp"
#include "SFML/System.hpp"
#include "SFML/Window.hpp"
#include "./MouseHG3D.h"
#include "SFML/System/Vector3.hpp"

using namespace sf;



// Check if a joystick is connected. 
extern "C" SFML_LIB_EXPORT void sfml_jst_isConnected(unsigned int joystick_c, int * result_c)
{
  unsigned int joystick_cpp = (unsigned int)joystick_c;
  bool result_cpp;
  result_cpp = (sf::Joystick::isConnected(joystick_cpp));
  *result_c = (int)result_cpp;
};

// Return the number of buttons supported by a joystick. 
extern "C" SFML_LIB_EXPORT void sfml_jst_getButtonCount(unsigned int joystick_c, unsigned int * result_c)
{
  unsigned int joystick_cpp = (unsigned int)joystick_c;
  unsigned int result_cpp;
  result_cpp = (sf::Joystick::getButtonCount(joystick_cpp));
  *result_c = (unsigned int)result_cpp;
};

// Check if a joystick supports a given axis. 
extern "C" SFML_LIB_EXPORT void sfml_jst_hasAxis(unsigned int joystick_c, enum EnumJoystickAxis axis_c, int * result_c)
{
  unsigned int joystick_cpp = (unsigned int)joystick_c;
  enum sf::Joystick::Axis axis_cpp = (enum sf::Joystick::Axis)axis_c;
  bool result_cpp;
  result_cpp = (sf::Joystick::hasAxis(joystick_cpp, axis_cpp));
  *result_c = (int)result_cpp;
};

// Check if a joystick button is pressed. 
extern "C" SFML_LIB_EXPORT void sfml_jst_isButtonPressed(unsigned int joystick_c, unsigned int button_c, int * result_c)
{
  unsigned int joystick_cpp = (unsigned int)joystick_c;
  unsigned int button_cpp = (unsigned int)button_c;
  bool result_cpp;
  result_cpp = (sf::Joystick::isButtonPressed(joystick_cpp, button_cpp));
  *result_c = (int)result_cpp;
};

// Get the current position of a joystick axis. 
extern "C" SFML_LIB_EXPORT void sfml_jst_getAxisPosition(unsigned int joystick_c, enum EnumJoystickAxis axis_c, float * result_c)
{
  unsigned int joystick_cpp = (unsigned int)joystick_c;
  enum sf::Joystick::Axis axis_cpp = (enum sf::Joystick::Axis)axis_c;
  float result_cpp;
  result_cpp = (sf::Joystick::getAxisPosition(joystick_cpp, axis_cpp));
  *result_c = (float)result_cpp;
};

// Update the states of all joysticks. 
extern "C" SFML_LIB_EXPORT void sfml_jst_update()
{
  (sf::Joystick::update());
};

