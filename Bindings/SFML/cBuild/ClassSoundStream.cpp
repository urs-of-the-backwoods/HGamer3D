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

// ClassSoundStream.cpp

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



// Destructor. 
extern "C" SFML_LIB_EXPORT void sfml_snst_destruct(struct hg3dclass_struct * thisclass_c)
{
  sf::SoundStream * thisclass_cpp = static_cast<sf::SoundStream*> (getHG3DClassPtr(*thisclass_c, "sf::SoundStream"));
  (delete thisclass_cpp);
};

// Start or resume playing the audio stream. 
extern "C" SFML_LIB_EXPORT void sfml_snst_play(struct hg3dclass_struct * thisclass_c)
{
  sf::SoundStream * thisclass_cpp = static_cast<sf::SoundStream*> (getHG3DClassPtr(*thisclass_c, "sf::SoundStream"));
  (thisclass_cpp->play());
};

// Pause the audio stream. 
extern "C" SFML_LIB_EXPORT void sfml_snst_pause(struct hg3dclass_struct * thisclass_c)
{
  sf::SoundStream * thisclass_cpp = static_cast<sf::SoundStream*> (getHG3DClassPtr(*thisclass_c, "sf::SoundStream"));
  (thisclass_cpp->pause());
};

// Stop playing the audio stream. 
extern "C" SFML_LIB_EXPORT void sfml_snst_stop(struct hg3dclass_struct * thisclass_c)
{
  sf::SoundStream * thisclass_cpp = static_cast<sf::SoundStream*> (getHG3DClassPtr(*thisclass_c, "sf::SoundStream"));
  (thisclass_cpp->stop());
};

// Return the number of channels of the stream. 
extern "C" SFML_LIB_EXPORT void sfml_snst_getChannelCount(struct hg3dclass_struct * thisclass_c, unsigned int * result_c)
{
  sf::SoundStream * thisclass_cpp = static_cast<sf::SoundStream*> (getHG3DClassPtr(*thisclass_c, "sf::SoundStream"));
  unsigned int result_cpp;
  result_cpp = (thisclass_cpp->getChannelCount());
  *result_c = (unsigned int)result_cpp;
};

// Get the stream sample rate of the stream. 
extern "C" SFML_LIB_EXPORT void sfml_snst_getSampleRate(struct hg3dclass_struct * thisclass_c, unsigned int * result_c)
{
  sf::SoundStream * thisclass_cpp = static_cast<sf::SoundStream*> (getHG3DClassPtr(*thisclass_c, "sf::SoundStream"));
  unsigned int result_cpp;
  result_cpp = (thisclass_cpp->getSampleRate());
  *result_c = (unsigned int)result_cpp;
};

// Set whether or not the stream should loop after reaching the end. 
extern "C" SFML_LIB_EXPORT void sfml_snst_setLoop(struct hg3dclass_struct * thisclass_c, int loop_c)
{
  sf::SoundStream * thisclass_cpp = static_cast<sf::SoundStream*> (getHG3DClassPtr(*thisclass_c, "sf::SoundStream"));
  bool loop_cpp = (bool)loop_c;
  (thisclass_cpp->setLoop(loop_cpp));
};

// Tell whether or not the stream is in loop mode. 
extern "C" SFML_LIB_EXPORT void sfml_snst_getLoop(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  sf::SoundStream * thisclass_cpp = static_cast<sf::SoundStream*> (getHG3DClassPtr(*thisclass_c, "sf::SoundStream"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->getLoop());
  *result_c = (int)result_cpp;
};

