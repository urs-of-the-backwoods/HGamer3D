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

// ClassPtr.cpp

// Here are the methods defined, which do the class pointer
// marshalling and the casting of subclasses to higher classes

#include <wchar.h>
#include <string>
#include <iostream>

#include <iostream>
#include <typeinfo>
#include <stdio.h>
#include <cstring>
#include <exception>
#include <SFMLDllDefines.h>
#include "SFML/Audio.hpp"
#include "SFML/System.hpp"
#include "SFML/Window.hpp"
#include "./MouseHG3D.h"
#include "SFML/System/Vector3.hpp"



typedef struct hg3dclass_struct {
	void *ptr;
	void *fptr;
} hg3dclass_struct;

void *getHG3DClassPtr(hg3dclass_struct inSt, const char* className)
{
	void *(*ptrcaster)(const char*, void*);
	ptrcaster = (void *(*)(const char*, void*))(inSt.fptr);
	return ((*ptrcaster)(className, inSt.ptr));
}

//
// MouseHG3D
//

// Ptr Caster
void *internalHG3D_MouseHG3D_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "MouseHG3D") == 0) {
		return ptrIn;
	};
	printf("PtrCaster not successful, Class: MouseHG3D is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_MouseHG3D(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_MouseHG3D_PtrCaster);
	return st;
};

//
// sf::Joystick
//

// Ptr Caster
void *internalHG3D_Joystick_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "sf::Joystick") == 0) {
		return ptrIn;
	};
	printf("PtrCaster not successful, Class: sf::Joystick is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_Joystick(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_Joystick_PtrCaster);
	return st;
};

//
// sf::Keyboard
//

// Ptr Caster
void *internalHG3D_Keyboard_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "sf::Keyboard") == 0) {
		return ptrIn;
	};
	printf("PtrCaster not successful, Class: sf::Keyboard is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_Keyboard(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_Keyboard_PtrCaster);
	return st;
};

//
// sf::Listener
//

// Ptr Caster
void *internalHG3D_Listener_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "sf::Listener") == 0) {
		return ptrIn;
	};
	printf("PtrCaster not successful, Class: sf::Listener is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_Listener(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_Listener_PtrCaster);
	return st;
};

//
// sf::Mouse
//

// Ptr Caster
void *internalHG3D_Mouse_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "sf::Mouse") == 0) {
		return ptrIn;
	};
	printf("PtrCaster not successful, Class: sf::Mouse is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_Mouse(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_Mouse_PtrCaster);
	return st;
};

//
// sf::Music
//

// Ptr Caster
void *internalHG3D_Music_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "sf::Music") == 0) {
		return ptrIn;
	};
	if (strcmp(className, "sf::SoundStream") == 0) {
		return (void *)(sf::SoundStream *)(sf::Music *)ptrIn;
	};
	if (strcmp(className, "sf::SoundSource") == 0) {
		return (void *)(sf::SoundSource *)(sf::Music *)ptrIn;
	};
	printf("PtrCaster not successful, Class: sf::Music is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_Music(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_Music_PtrCaster);
	return st;
};

//
// sf::Sound
//

// Ptr Caster
void *internalHG3D_Sound_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "sf::Sound") == 0) {
		return ptrIn;
	};
	if (strcmp(className, "sf::SoundSource") == 0) {
		return (void *)(sf::SoundSource *)(sf::Sound *)ptrIn;
	};
	printf("PtrCaster not successful, Class: sf::Sound is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_Sound(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_Sound_PtrCaster);
	return st;
};

//
// sf::SoundBuffer
//

// Ptr Caster
void *internalHG3D_SoundBuffer_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "sf::SoundBuffer") == 0) {
		return ptrIn;
	};
	printf("PtrCaster not successful, Class: sf::SoundBuffer is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_SoundBuffer(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_SoundBuffer_PtrCaster);
	return st;
};

//
// sf::SoundSource
//

// Ptr Caster
void *internalHG3D_SoundSource_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "sf::SoundSource") == 0) {
		return ptrIn;
	};
	printf("PtrCaster not successful, Class: sf::SoundSource is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_SoundSource(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_SoundSource_PtrCaster);
	return st;
};

//
// sf::SoundStream
//

// Ptr Caster
void *internalHG3D_SoundStream_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "sf::SoundStream") == 0) {
		return ptrIn;
	};
	if (strcmp(className, "sf::SoundSource") == 0) {
		return (void *)(sf::SoundSource *)(sf::SoundStream *)ptrIn;
	};
	printf("PtrCaster not successful, Class: sf::SoundStream is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_SoundStream(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_SoundStream_PtrCaster);
	return st;
};

