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
#include <SDL2DllDefines.h>
#include "./SDL.h"
#include "HG3DUtilities.h"



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
// HG3DUtilities
//

// Ptr Caster
void *internalHG3D_HG3DUtilities_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "HG3DUtilities") == 0) {
		return ptrIn;
	};
	printf("PtrCaster not successful, Class: HG3DUtilities is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_HG3DUtilities(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_HG3DUtilities_PtrCaster);
	return st;
};

