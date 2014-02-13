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

// ClassPtr.h

// Here are the methods defined, which do the class pointer
// marshalling and the casting of subclasses to higher classes

#include "wchar.h"


#ifndef CLASSPTR_INCLUDE_H
#define CLASSPTR_INCLUDE_H

typedef struct hg3dclass_struct {
	void *ptr;
	void *fptr;
} hg3dclass_struct;

void *getHG3DClassPtr(hg3dclass_struct inSt, const char* className);


typedef void ClassMouseHG3D; 
hg3dclass_struct getHG3DClass_MouseHG3D(void *ptrIn);

typedef void ClassJoystick; 
hg3dclass_struct getHG3DClass_Joystick(void *ptrIn);

typedef void ClassKeyboard; 
hg3dclass_struct getHG3DClass_Keyboard(void *ptrIn);

typedef void ClassListener; 
hg3dclass_struct getHG3DClass_Listener(void *ptrIn);

typedef void ClassMouse; 
hg3dclass_struct getHG3DClass_Mouse(void *ptrIn);

typedef void ClassMusic; 
hg3dclass_struct getHG3DClass_Music(void *ptrIn);

typedef void ClassSound; 
hg3dclass_struct getHG3DClass_Sound(void *ptrIn);

typedef void ClassSoundBuffer; 
hg3dclass_struct getHG3DClass_SoundBuffer(void *ptrIn);

typedef void ClassSoundSource; 
hg3dclass_struct getHG3DClass_SoundSource(void *ptrIn);

typedef void ClassSoundStream; 
hg3dclass_struct getHG3DClass_SoundStream(void *ptrIn);

#endif
