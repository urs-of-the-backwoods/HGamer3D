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

// ClassSoundSource.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassSoundSource
#define _DEFINED_HG3D_ClassSoundSource

#include "ClassPtr.h"
#include "StructVec3.h"


// Destructor. 
void sfml_snsr_destruct(struct hg3dclass_struct * thisclass_c);

// Set the pitch of the sound. 
void sfml_snsr_setPitch(struct hg3dclass_struct * thisclass_c, float pitch_c);

// Set the volume of the sound. 
void sfml_snsr_setVolume(struct hg3dclass_struct * thisclass_c, float volume_c);

// Set the 3D position of the sound in the audio scene. 
void sfml_snsr_setPosition(struct hg3dclass_struct * thisclass_c, float x_c, float y_c, float z_c);

// Make the sound's position relative to the listener or absolute. 
void sfml_snsr_setRelativeToListener(struct hg3dclass_struct * thisclass_c, int relative_c);

// Set the minimum distance of the sound. 
void sfml_snsr_setMinDistance(struct hg3dclass_struct * thisclass_c, float distance_c);

// Set the attenuation factor of the sound. 
void sfml_snsr_setAttenuation(struct hg3dclass_struct * thisclass_c, float attenuation_c);

// Get the pitch of the sound. 
void sfml_snsr_getPitch(struct hg3dclass_struct * thisclass_c, float * result_c);

// Get the volume of the sound. 
void sfml_snsr_getVolume(struct hg3dclass_struct * thisclass_c, float * result_c);

// Get the 3D position of the sound in the audio scene. 
void sfml_snsr_getPosition(struct hg3dclass_struct * thisclass_c, struct vector3f_struct * result_c);

// Tell whether the sound's position is relative to the listener or is absolute. 
void sfml_snsr_isRelativeToListener(struct hg3dclass_struct * thisclass_c, int * result_c);

// Get the minimum distance of the sound. 
void sfml_snsr_getMinDistance(struct hg3dclass_struct * thisclass_c, float * result_c);

// Get the attenuation factor of the sound. 
void sfml_snsr_getAttenuation(struct hg3dclass_struct * thisclass_c, float * result_c);

#endif 
