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

// ClassListener.cpp

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
	#include "StructVec3.h"
#include "SFML/Audio.hpp"
#include "SFML/System.hpp"
#include "SFML/Window.hpp"
#include "./MouseHG3D.h"
#include "SFML/System/Vector3.hpp"

using namespace sf;



// Change the global volume of all the sounds and musics. 
extern "C" SFML_LIB_EXPORT void sfml_lst_setGlobalVolume(float volume_c)
{
  float volume_cpp = (float)volume_c;
  (sf::Listener::setGlobalVolume(volume_cpp));
};

// Get the current value of the global volume. 
extern "C" SFML_LIB_EXPORT void sfml_lst_getGlobalVolume(float * result_c)
{
  float result_cpp;
  result_cpp = (sf::Listener::getGlobalVolume());
  *result_c = (float)result_cpp;
};

// Set the position of the listener in the scene. 
extern "C" SFML_LIB_EXPORT void sfml_lst_setPosition(float x_c, float y_c, float z_c)
{
  float x_cpp = (float)x_c;
  float y_cpp = (float)y_c;
  float z_cpp = (float)z_c;
  (sf::Listener::setPosition(x_cpp, y_cpp, z_cpp));
};

// Get the current position of the listener in the scene. 
extern "C" SFML_LIB_EXPORT void sfml_lst_getPosition(struct vector3f_struct * result_c)
{
  Vector3f result_cpp;
  result_cpp = (sf::Listener::getPosition());
  *result_c = *((struct vector3f_struct*) &result_cpp);
};

// Set the orientation of the listener in the scene. 
extern "C" SFML_LIB_EXPORT void sfml_lst_setDirection(float x_c, float y_c, float z_c)
{
  float x_cpp = (float)x_c;
  float y_cpp = (float)y_c;
  float z_cpp = (float)z_c;
  (sf::Listener::setDirection(x_cpp, y_cpp, z_cpp));
};

// Get the current orientation of the listener in the scene. 
extern "C" SFML_LIB_EXPORT void sfml_lst_getDirection(struct vector3f_struct * result_c)
{
  Vector3f result_cpp;
  result_cpp = (sf::Listener::getDirection());
  *result_c = *((struct vector3f_struct*) &result_cpp);
};

