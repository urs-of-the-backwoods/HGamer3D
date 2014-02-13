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

// ClassSoundStream.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassSoundStream
#define _DEFINED_HG3D_ClassSoundStream

#include "ClassPtr.h"


// Destructor. 
void sfml_snst_destruct(struct hg3dclass_struct * thisclass_c);

// Start or resume playing the audio stream. 
void sfml_snst_play(struct hg3dclass_struct * thisclass_c);

// Pause the audio stream. 
void sfml_snst_pause(struct hg3dclass_struct * thisclass_c);

// Stop playing the audio stream. 
void sfml_snst_stop(struct hg3dclass_struct * thisclass_c);

// Return the number of channels of the stream. 
void sfml_snst_getChannelCount(struct hg3dclass_struct * thisclass_c, unsigned int * result_c);

// Get the stream sample rate of the stream. 
void sfml_snst_getSampleRate(struct hg3dclass_struct * thisclass_c, unsigned int * result_c);

// Set whether or not the stream should loop after reaching the end. 
void sfml_snst_setLoop(struct hg3dclass_struct * thisclass_c, int loop_c);

// Tell whether or not the stream is in loop mode. 
void sfml_snst_getLoop(struct hg3dclass_struct * thisclass_c, int * result_c);

#endif 
