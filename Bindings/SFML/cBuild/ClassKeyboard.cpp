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

// ClassKeyboard.cpp

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
	#include "EnumKey.h"
#include "SFML/Audio.hpp"
#include "SFML/System.hpp"
#include "SFML/Window.hpp"
#include "./MouseHG3D.h"
#include "SFML/System/Vector3.hpp"

using namespace sf;



// Check if a key is pressed. 
extern "C" SFML_LIB_EXPORT void sfml_kbd_isKeyPressed(enum EnumKey key_c, int * result_c)
{
  enum sf::Keyboard::Key key_cpp = (enum sf::Keyboard::Key)key_c;
  bool result_cpp;
  result_cpp = (sf::Keyboard::isKeyPressed(key_cpp));
  *result_c = (int)result_cpp;
};

