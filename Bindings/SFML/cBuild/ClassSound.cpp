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

// ClassSound.cpp

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
extern "C" SFML_LIB_EXPORT void sfml_snd_construct(struct hg3dclass_struct * result_c)
{
  sf::Sound * result_cpp;
  result_cpp = (new sf::Sound());
  *result_c = getHG3DClass_Sound((void *) result_cpp);
;
};

// Destructor. 
extern "C" SFML_LIB_EXPORT void sfml_snd_destruct(struct hg3dclass_struct * thisclass_c)
{
  sf::Sound * thisclass_cpp = static_cast<sf::Sound*> (getHG3DClassPtr(*thisclass_c, "sf::Sound"));
  (delete thisclass_cpp);
};

// Start or resume playing the sound. 
extern "C" SFML_LIB_EXPORT void sfml_snd_play(struct hg3dclass_struct * thisclass_c)
{
  sf::Sound * thisclass_cpp = static_cast<sf::Sound*> (getHG3DClassPtr(*thisclass_c, "sf::Sound"));
  (thisclass_cpp->play());
};

// Pause the sound. 
extern "C" SFML_LIB_EXPORT void sfml_snd_pause(struct hg3dclass_struct * thisclass_c)
{
  sf::Sound * thisclass_cpp = static_cast<sf::Sound*> (getHG3DClassPtr(*thisclass_c, "sf::Sound"));
  (thisclass_cpp->pause());
};

// stop playing the sound 
extern "C" SFML_LIB_EXPORT void sfml_snd_stop(struct hg3dclass_struct * thisclass_c)
{
  sf::Sound * thisclass_cpp = static_cast<sf::Sound*> (getHG3DClassPtr(*thisclass_c, "sf::Sound"));
  (thisclass_cpp->stop());
};

// Set the source buffer containing the audio data to play. 
extern "C" SFML_LIB_EXPORT void sfml_snd_setBuffer(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * buffer_c)
{
  sf::Sound * thisclass_cpp = static_cast<sf::Sound*> (getHG3DClassPtr(*thisclass_c, "sf::Sound"));
  const sf::SoundBuffer * buffer_cpp = static_cast<sf::SoundBuffer*> (getHG3DClassPtr(*buffer_c, "sf::SoundBuffer"));
  (thisclass_cpp->setBuffer(*buffer_cpp));
};

// Set whether or not the sound should loop after reaching the end. 
extern "C" SFML_LIB_EXPORT void sfml_snd_setLoop(struct hg3dclass_struct * thisclass_c, int loop_c)
{
  sf::Sound * thisclass_cpp = static_cast<sf::Sound*> (getHG3DClassPtr(*thisclass_c, "sf::Sound"));
  bool loop_cpp = (bool)loop_c;
  (thisclass_cpp->setLoop(loop_cpp));
};

// Tell whether or not the sound is in loop mode. 
extern "C" SFML_LIB_EXPORT void sfml_snd_getLoop(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  sf::Sound * thisclass_cpp = static_cast<sf::Sound*> (getHG3DClassPtr(*thisclass_c, "sf::Sound"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->getLoop());
  *result_c = (int)result_cpp;
};

// Reset the internal buffer of the sound. 
extern "C" SFML_LIB_EXPORT void sfml_snd_resetBuffer(struct hg3dclass_struct * thisclass_c)
{
  sf::Sound * thisclass_cpp = static_cast<sf::Sound*> (getHG3DClassPtr(*thisclass_c, "sf::Sound"));
  (thisclass_cpp->resetBuffer());
};

