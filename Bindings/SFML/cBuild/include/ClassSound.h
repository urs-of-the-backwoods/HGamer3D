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

// ClassSound.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassSound
#define _DEFINED_HG3D_ClassSound

#include "ClassPtr.h"
#include "ClassSoundBuffer.h"


// Default constructor. 
void sfml_snd_construct(struct hg3dclass_struct * result_c);

// Destructor. 
void sfml_snd_destruct(struct hg3dclass_struct * thisclass_c);

// Start or resume playing the sound. 
void sfml_snd_play(struct hg3dclass_struct * thisclass_c);

// Pause the sound. 
void sfml_snd_pause(struct hg3dclass_struct * thisclass_c);

// stop playing the sound 
void sfml_snd_stop(struct hg3dclass_struct * thisclass_c);

// Set the source buffer containing the audio data to play. 
void sfml_snd_setBuffer(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * buffer_c);

// Set whether or not the sound should loop after reaching the end. 
void sfml_snd_setLoop(struct hg3dclass_struct * thisclass_c, int loop_c);

// Tell whether or not the sound is in loop mode. 
void sfml_snd_getLoop(struct hg3dclass_struct * thisclass_c, int * result_c);

// Reset the internal buffer of the sound. 
void sfml_snd_resetBuffer(struct hg3dclass_struct * thisclass_c);

#endif 
