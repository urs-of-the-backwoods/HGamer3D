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

// ClassSoundSource.cpp

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



// Destructor. 
extern "C" SFML_LIB_EXPORT void sfml_snsr_destruct(struct hg3dclass_struct * thisclass_c)
{
  sf::SoundSource * thisclass_cpp = static_cast<sf::SoundSource*> (getHG3DClassPtr(*thisclass_c, "sf::SoundSource"));
  (delete thisclass_cpp);
};

// Set the pitch of the sound. 
extern "C" SFML_LIB_EXPORT void sfml_snsr_setPitch(struct hg3dclass_struct * thisclass_c, float pitch_c)
{
  sf::SoundSource * thisclass_cpp = static_cast<sf::SoundSource*> (getHG3DClassPtr(*thisclass_c, "sf::SoundSource"));
  float pitch_cpp = (float)pitch_c;
  (thisclass_cpp->setPitch(pitch_cpp));
};

// Set the volume of the sound. 
extern "C" SFML_LIB_EXPORT void sfml_snsr_setVolume(struct hg3dclass_struct * thisclass_c, float volume_c)
{
  sf::SoundSource * thisclass_cpp = static_cast<sf::SoundSource*> (getHG3DClassPtr(*thisclass_c, "sf::SoundSource"));
  float volume_cpp = (float)volume_c;
  (thisclass_cpp->setVolume(volume_cpp));
};

// Set the 3D position of the sound in the audio scene. 
extern "C" SFML_LIB_EXPORT void sfml_snsr_setPosition(struct hg3dclass_struct * thisclass_c, float x_c, float y_c, float z_c)
{
  sf::SoundSource * thisclass_cpp = static_cast<sf::SoundSource*> (getHG3DClassPtr(*thisclass_c, "sf::SoundSource"));
  float x_cpp = (float)x_c;
  float y_cpp = (float)y_c;
  float z_cpp = (float)z_c;
  (thisclass_cpp->setPosition(x_cpp, y_cpp, z_cpp));
};

// Make the sound's position relative to the listener or absolute. 
extern "C" SFML_LIB_EXPORT void sfml_snsr_setRelativeToListener(struct hg3dclass_struct * thisclass_c, int relative_c)
{
  sf::SoundSource * thisclass_cpp = static_cast<sf::SoundSource*> (getHG3DClassPtr(*thisclass_c, "sf::SoundSource"));
  bool relative_cpp = (bool)relative_c;
  (thisclass_cpp->setRelativeToListener(relative_cpp));
};

// Set the minimum distance of the sound. 
extern "C" SFML_LIB_EXPORT void sfml_snsr_setMinDistance(struct hg3dclass_struct * thisclass_c, float distance_c)
{
  sf::SoundSource * thisclass_cpp = static_cast<sf::SoundSource*> (getHG3DClassPtr(*thisclass_c, "sf::SoundSource"));
  float distance_cpp = (float)distance_c;
  (thisclass_cpp->setMinDistance(distance_cpp));
};

// Set the attenuation factor of the sound. 
extern "C" SFML_LIB_EXPORT void sfml_snsr_setAttenuation(struct hg3dclass_struct * thisclass_c, float attenuation_c)
{
  sf::SoundSource * thisclass_cpp = static_cast<sf::SoundSource*> (getHG3DClassPtr(*thisclass_c, "sf::SoundSource"));
  float attenuation_cpp = (float)attenuation_c;
  (thisclass_cpp->setAttenuation(attenuation_cpp));
};

// Get the pitch of the sound. 
extern "C" SFML_LIB_EXPORT void sfml_snsr_getPitch(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  sf::SoundSource * thisclass_cpp = static_cast<sf::SoundSource*> (getHG3DClassPtr(*thisclass_c, "sf::SoundSource"));
  float result_cpp;
  result_cpp = (thisclass_cpp->getPitch());
  *result_c = (float)result_cpp;
};

// Get the volume of the sound. 
extern "C" SFML_LIB_EXPORT void sfml_snsr_getVolume(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  sf::SoundSource * thisclass_cpp = static_cast<sf::SoundSource*> (getHG3DClassPtr(*thisclass_c, "sf::SoundSource"));
  float result_cpp;
  result_cpp = (thisclass_cpp->getVolume());
  *result_c = (float)result_cpp;
};

// Get the 3D position of the sound in the audio scene. 
extern "C" SFML_LIB_EXPORT void sfml_snsr_getPosition(struct hg3dclass_struct * thisclass_c, struct vector3f_struct * result_c)
{
  sf::SoundSource * thisclass_cpp = static_cast<sf::SoundSource*> (getHG3DClassPtr(*thisclass_c, "sf::SoundSource"));
  Vector3f result_cpp;
  result_cpp = (thisclass_cpp->getPosition());
  *result_c = *((struct vector3f_struct*) &result_cpp);
};

// Tell whether the sound's position is relative to the listener or is absolute. 
extern "C" SFML_LIB_EXPORT void sfml_snsr_isRelativeToListener(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  sf::SoundSource * thisclass_cpp = static_cast<sf::SoundSource*> (getHG3DClassPtr(*thisclass_c, "sf::SoundSource"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isRelativeToListener());
  *result_c = (int)result_cpp;
};

// Get the minimum distance of the sound. 
extern "C" SFML_LIB_EXPORT void sfml_snsr_getMinDistance(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  sf::SoundSource * thisclass_cpp = static_cast<sf::SoundSource*> (getHG3DClassPtr(*thisclass_c, "sf::SoundSource"));
  float result_cpp;
  result_cpp = (thisclass_cpp->getMinDistance());
  *result_c = (float)result_cpp;
};

// Get the attenuation factor of the sound. 
extern "C" SFML_LIB_EXPORT void sfml_snsr_getAttenuation(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  sf::SoundSource * thisclass_cpp = static_cast<sf::SoundSource*> (getHG3DClassPtr(*thisclass_c, "sf::SoundSource"));
  float result_cpp;
  result_cpp = (thisclass_cpp->getAttenuation());
  *result_c = (float)result_cpp;
};

