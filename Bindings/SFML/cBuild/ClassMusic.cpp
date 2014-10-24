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

// ClassMusic.cpp

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
	#include "SFML/Audio.hpp"
#include "SFML/System.hpp"
#include "SFML/Window.hpp"
#include "./MouseHG3D.h"
#include "SFML/System/Vector3.hpp"

using namespace sf;



// Default constructor. 
extern "C" SFML_LIB_EXPORT void sfml_msc_construct(struct hg3dclass_struct * result_c)
{
  sf::Music * result_cpp;
  result_cpp = (new sf::Music());
  *result_c = getHG3DClass_Music((void *) result_cpp);
;
};

// Destructor. 
extern "C" SFML_LIB_EXPORT void sfml_msc_destruct(struct hg3dclass_struct * thisclass_c)
{
  sf::Music * thisclass_cpp = static_cast<sf::Music*> (getHG3DClassPtr(*thisclass_c, "sf::Music"));
  (delete thisclass_cpp);
};

// Open a music from an audio file. 
extern "C" SFML_LIB_EXPORT void sfml_msc_openFromFile(struct hg3dclass_struct * thisclass_c, char * filename_c, int * result_c)
{
  sf::Music * thisclass_cpp = static_cast<sf::Music*> (getHG3DClassPtr(*thisclass_c, "sf::Music"));
  std::string filename_cpp = std::string((const char*) filename_c);
  bool result_cpp;
  result_cpp = (thisclass_cpp->openFromFile(filename_cpp));
  *result_c = (int)result_cpp;
};

