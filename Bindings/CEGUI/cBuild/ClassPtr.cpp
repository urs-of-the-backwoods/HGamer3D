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
#include <CEGUIDllDefines.h>
#include "./CEGUI.h"
#include "./CEGUIString.h"
#include "RendererModules/Ogre/CEGUIOgreRenderer.h"
#include "./WindowManagerHG3D.h"
#include "./SystemHG3D.h"
#include "HG3DCommandHandler.h"
#include "HG3DEventStaticFunctions.h"
#include "HG3DListboxStaticFunctions.h"
#include "HG3DEventController.h"
#include "HG3DEventModule.h"
#include "HG3DWindowStaticFunctions.h"



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
// CEGUI::Checkbox
//

// Ptr Caster
void *internalHG3D_Checkbox_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "CEGUI::Checkbox") == 0) {
		return ptrIn;
	};
	printf("PtrCaster not successful, Class: CEGUI::Checkbox is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_Checkbox(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_Checkbox_PtrCaster);
	return st;
};

//
// CEGUI::Combobox
//

// Ptr Caster
void *internalHG3D_Combobox_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "CEGUI::Combobox") == 0) {
		return ptrIn;
	};
	if (strcmp(className, "CEGUI::Window") == 0) {
		return (void *)(CEGUI::Window *)(CEGUI::Combobox *)ptrIn;
	};
	if (strcmp(className, "CEGUI::PropertySet") == 0) {
		return (void *)(CEGUI::PropertySet *)(CEGUI::Combobox *)ptrIn;
	};
	if (strcmp(className, "CEGUI::EventSet") == 0) {
		return (void *)(CEGUI::EventSet *)(CEGUI::Combobox *)ptrIn;
	};
	printf("PtrCaster not successful, Class: CEGUI::Combobox is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_Combobox(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_Combobox_PtrCaster);
	return st;
};

//
// CEGUI::ComboDropList
//

// Ptr Caster
void *internalHG3D_ComboDropList_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "CEGUI::ComboDropList") == 0) {
		return ptrIn;
	};
	if (strcmp(className, "CEGUI::Listbox") == 0) {
		return (void *)(CEGUI::Listbox *)(CEGUI::ComboDropList *)ptrIn;
	};
	if (strcmp(className, "CEGUI::Window") == 0) {
		return (void *)(CEGUI::Window *)(CEGUI::ComboDropList *)ptrIn;
	};
	if (strcmp(className, "CEGUI::PropertySet") == 0) {
		return (void *)(CEGUI::PropertySet *)(CEGUI::ComboDropList *)ptrIn;
	};
	if (strcmp(className, "CEGUI::EventSet") == 0) {
		return (void *)(CEGUI::EventSet *)(CEGUI::ComboDropList *)ptrIn;
	};
	printf("PtrCaster not successful, Class: CEGUI::ComboDropList is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_ComboDropList(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_ComboDropList_PtrCaster);
	return st;
};

//
// CEGUI::DefaultLogger
//

// Ptr Caster
void *internalHG3D_DefaultLogger_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "CEGUI::DefaultLogger") == 0) {
		return ptrIn;
	};
	if (strcmp(className, "CEGUI::Logger") == 0) {
		return (void *)(CEGUI::Logger *)(CEGUI::DefaultLogger *)ptrIn;
	};
	printf("PtrCaster not successful, Class: CEGUI::DefaultLogger is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_DefaultLogger(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_DefaultLogger_PtrCaster);
	return st;
};

//
// CEGUI::DefaultResourceProvider
//

// Ptr Caster
void *internalHG3D_DefaultResourceProvider_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "CEGUI::DefaultResourceProvider") == 0) {
		return ptrIn;
	};
	if (strcmp(className, "CEGUI::ResourceProvider") == 0) {
		return (void *)(CEGUI::ResourceProvider *)(CEGUI::DefaultResourceProvider *)ptrIn;
	};
	printf("PtrCaster not successful, Class: CEGUI::DefaultResourceProvider is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_DefaultResourceProvider(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_DefaultResourceProvider_PtrCaster);
	return st;
};

//
// CEGUI::DragContainer
//

// Ptr Caster
void *internalHG3D_DragContainer_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "CEGUI::DragContainer") == 0) {
		return ptrIn;
	};
	if (strcmp(className, "CEGUI::Window") == 0) {
		return (void *)(CEGUI::Window *)(CEGUI::DragContainer *)ptrIn;
	};
	if (strcmp(className, "CEGUI::PropertySet") == 0) {
		return (void *)(CEGUI::PropertySet *)(CEGUI::DragContainer *)ptrIn;
	};
	if (strcmp(className, "CEGUI::EventSet") == 0) {
		return (void *)(CEGUI::EventSet *)(CEGUI::DragContainer *)ptrIn;
	};
	printf("PtrCaster not successful, Class: CEGUI::DragContainer is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_DragContainer(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_DragContainer_PtrCaster);
	return st;
};

//
// CEGUI::Editbox
//

// Ptr Caster
void *internalHG3D_Editbox_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "CEGUI::Editbox") == 0) {
		return ptrIn;
	};
	if (strcmp(className, "CEGUI::Window") == 0) {
		return (void *)(CEGUI::Window *)(CEGUI::Editbox *)ptrIn;
	};
	if (strcmp(className, "CEGUI::PropertySet") == 0) {
		return (void *)(CEGUI::PropertySet *)(CEGUI::Editbox *)ptrIn;
	};
	if (strcmp(className, "CEGUI::EventSet") == 0) {
		return (void *)(CEGUI::EventSet *)(CEGUI::Editbox *)ptrIn;
	};
	printf("PtrCaster not successful, Class: CEGUI::Editbox is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_Editbox(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_Editbox_PtrCaster);
	return st;
};

//
// CEGUI::EventArgs
//

// Ptr Caster
void *internalHG3D_EventArgs_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "CEGUI::EventArgs") == 0) {
		return ptrIn;
	};
	printf("PtrCaster not successful, Class: CEGUI::EventArgs is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_EventArgs(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_EventArgs_PtrCaster);
	return st;
};

//
// CEGUI::EventSet
//

// Ptr Caster
void *internalHG3D_EventSet_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "CEGUI::EventSet") == 0) {
		return ptrIn;
	};
	printf("PtrCaster not successful, Class: CEGUI::EventSet is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_EventSet(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_EventSet_PtrCaster);
	return st;
};

//
// CEGUI::Font
//

// Ptr Caster
void *internalHG3D_Font_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "CEGUI::Font") == 0) {
		return ptrIn;
	};
	if (strcmp(className, "CEGUI::PropertySet") == 0) {
		return (void *)(CEGUI::PropertySet *)(CEGUI::Font *)ptrIn;
	};
	printf("PtrCaster not successful, Class: CEGUI::Font is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_Font(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_Font_PtrCaster);
	return st;
};

//
// CEGUI::FontManager
//

// Ptr Caster
void *internalHG3D_FontManager_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "CEGUI::FontManager") == 0) {
		return ptrIn;
	};
	printf("PtrCaster not successful, Class: CEGUI::FontManager is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_FontManager(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_FontManager_PtrCaster);
	return st;
};

//
// CEGUI::FrameWindow
//

// Ptr Caster
void *internalHG3D_FrameWindow_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "CEGUI::FrameWindow") == 0) {
		return ptrIn;
	};
	if (strcmp(className, "CEGUI::Window") == 0) {
		return (void *)(CEGUI::Window *)(CEGUI::FrameWindow *)ptrIn;
	};
	if (strcmp(className, "CEGUI::PropertySet") == 0) {
		return (void *)(CEGUI::PropertySet *)(CEGUI::FrameWindow *)ptrIn;
	};
	if (strcmp(className, "CEGUI::EventSet") == 0) {
		return (void *)(CEGUI::EventSet *)(CEGUI::FrameWindow *)ptrIn;
	};
	printf("PtrCaster not successful, Class: CEGUI::FrameWindow is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_FrameWindow(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_FrameWindow_PtrCaster);
	return st;
};

//
// CEGUI::ImageCodec
//

// Ptr Caster
void *internalHG3D_ImageCodec_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "CEGUI::ImageCodec") == 0) {
		return ptrIn;
	};
	printf("PtrCaster not successful, Class: CEGUI::ImageCodec is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_ImageCodec(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_ImageCodec_PtrCaster);
	return st;
};

//
// CEGUI::Imageset
//

// Ptr Caster
void *internalHG3D_Imageset_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "CEGUI::Imageset") == 0) {
		return ptrIn;
	};
	printf("PtrCaster not successful, Class: CEGUI::Imageset is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_Imageset(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_Imageset_PtrCaster);
	return st;
};

//
// CEGUI::ItemEntry
//

// Ptr Caster
void *internalHG3D_ItemEntry_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "CEGUI::ItemEntry") == 0) {
		return ptrIn;
	};
	if (strcmp(className, "CEGUI::Window") == 0) {
		return (void *)(CEGUI::Window *)(CEGUI::ItemEntry *)ptrIn;
	};
	if (strcmp(className, "CEGUI::PropertySet") == 0) {
		return (void *)(CEGUI::PropertySet *)(CEGUI::ItemEntry *)ptrIn;
	};
	if (strcmp(className, "CEGUI::EventSet") == 0) {
		return (void *)(CEGUI::EventSet *)(CEGUI::ItemEntry *)ptrIn;
	};
	printf("PtrCaster not successful, Class: CEGUI::ItemEntry is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_ItemEntry(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_ItemEntry_PtrCaster);
	return st;
};

//
// CEGUI::ItemListbox
//

// Ptr Caster
void *internalHG3D_ItemListbox_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "CEGUI::ItemListbox") == 0) {
		return ptrIn;
	};
	if (strcmp(className, "CEGUI::ScrolledItemListBase") == 0) {
		return (void *)(CEGUI::ScrolledItemListBase *)(CEGUI::ItemListbox *)ptrIn;
	};
	printf("PtrCaster not successful, Class: CEGUI::ItemListbox is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_ItemListbox(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_ItemListbox_PtrCaster);
	return st;
};

//
// CEGUI::Listbox
//

// Ptr Caster
void *internalHG3D_Listbox_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "CEGUI::Listbox") == 0) {
		return ptrIn;
	};
	if (strcmp(className, "CEGUI::Window") == 0) {
		return (void *)(CEGUI::Window *)(CEGUI::Listbox *)ptrIn;
	};
	if (strcmp(className, "CEGUI::PropertySet") == 0) {
		return (void *)(CEGUI::PropertySet *)(CEGUI::Listbox *)ptrIn;
	};
	if (strcmp(className, "CEGUI::EventSet") == 0) {
		return (void *)(CEGUI::EventSet *)(CEGUI::Listbox *)ptrIn;
	};
	printf("PtrCaster not successful, Class: CEGUI::Listbox is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_Listbox(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_Listbox_PtrCaster);
	return st;
};

//
// CEGUI::ListboxItem
//

// Ptr Caster
void *internalHG3D_ListboxItem_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "CEGUI::ListboxItem") == 0) {
		return ptrIn;
	};
	printf("PtrCaster not successful, Class: CEGUI::ListboxItem is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_ListboxItem(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_ListboxItem_PtrCaster);
	return st;
};

//
// CEGUI::ListboxTextItem
//

// Ptr Caster
void *internalHG3D_ListboxTextItem_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "CEGUI::ListboxTextItem") == 0) {
		return ptrIn;
	};
	if (strcmp(className, "CEGUI::ListboxItem") == 0) {
		return (void *)(CEGUI::ListboxItem *)(CEGUI::ListboxTextItem *)ptrIn;
	};
	printf("PtrCaster not successful, Class: CEGUI::ListboxTextItem is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_ListboxTextItem(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_ListboxTextItem_PtrCaster);
	return st;
};

//
// CEGUI::ListHeader
//

// Ptr Caster
void *internalHG3D_ListHeader_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "CEGUI::ListHeader") == 0) {
		return ptrIn;
	};
	if (strcmp(className, "CEGUI::Window") == 0) {
		return (void *)(CEGUI::Window *)(CEGUI::ListHeader *)ptrIn;
	};
	if (strcmp(className, "CEGUI::PropertySet") == 0) {
		return (void *)(CEGUI::PropertySet *)(CEGUI::ListHeader *)ptrIn;
	};
	if (strcmp(className, "CEGUI::EventSet") == 0) {
		return (void *)(CEGUI::EventSet *)(CEGUI::ListHeader *)ptrIn;
	};
	printf("PtrCaster not successful, Class: CEGUI::ListHeader is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_ListHeader(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_ListHeader_PtrCaster);
	return st;
};

//
// CEGUI::ListHeaderSegment
//

// Ptr Caster
void *internalHG3D_ListHeaderSegment_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "CEGUI::ListHeaderSegment") == 0) {
		return ptrIn;
	};
	if (strcmp(className, "CEGUI::Window") == 0) {
		return (void *)(CEGUI::Window *)(CEGUI::ListHeaderSegment *)ptrIn;
	};
	if (strcmp(className, "CEGUI::PropertySet") == 0) {
		return (void *)(CEGUI::PropertySet *)(CEGUI::ListHeaderSegment *)ptrIn;
	};
	if (strcmp(className, "CEGUI::EventSet") == 0) {
		return (void *)(CEGUI::EventSet *)(CEGUI::ListHeaderSegment *)ptrIn;
	};
	printf("PtrCaster not successful, Class: CEGUI::ListHeaderSegment is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_ListHeaderSegment(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_ListHeaderSegment_PtrCaster);
	return st;
};

//
// CEGUI::Logger
//

// Ptr Caster
void *internalHG3D_Logger_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "CEGUI::Logger") == 0) {
		return ptrIn;
	};
	printf("PtrCaster not successful, Class: CEGUI::Logger is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_Logger(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_Logger_PtrCaster);
	return st;
};

//
// CEGUI::MenuBase
//

// Ptr Caster
void *internalHG3D_MenuBase_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "CEGUI::MenuBase") == 0) {
		return ptrIn;
	};
	printf("PtrCaster not successful, Class: CEGUI::MenuBase is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_MenuBase(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_MenuBase_PtrCaster);
	return st;
};

//
// CEGUI::MenuItem
//

// Ptr Caster
void *internalHG3D_MenuItem_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "CEGUI::MenuItem") == 0) {
		return ptrIn;
	};
	if (strcmp(className, "CEGUI::ItemEntry") == 0) {
		return (void *)(CEGUI::ItemEntry *)(CEGUI::MenuItem *)ptrIn;
	};
	if (strcmp(className, "CEGUI::Window") == 0) {
		return (void *)(CEGUI::Window *)(CEGUI::MenuItem *)ptrIn;
	};
	if (strcmp(className, "CEGUI::PropertySet") == 0) {
		return (void *)(CEGUI::PropertySet *)(CEGUI::MenuItem *)ptrIn;
	};
	if (strcmp(className, "CEGUI::EventSet") == 0) {
		return (void *)(CEGUI::EventSet *)(CEGUI::MenuItem *)ptrIn;
	};
	printf("PtrCaster not successful, Class: CEGUI::MenuItem is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_MenuItem(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_MenuItem_PtrCaster);
	return st;
};

//
// CEGUI::MultiColumnList
//

// Ptr Caster
void *internalHG3D_MultiColumnList_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "CEGUI::MultiColumnList") == 0) {
		return ptrIn;
	};
	if (strcmp(className, "CEGUI::Window") == 0) {
		return (void *)(CEGUI::Window *)(CEGUI::MultiColumnList *)ptrIn;
	};
	if (strcmp(className, "CEGUI::PropertySet") == 0) {
		return (void *)(CEGUI::PropertySet *)(CEGUI::MultiColumnList *)ptrIn;
	};
	if (strcmp(className, "CEGUI::EventSet") == 0) {
		return (void *)(CEGUI::EventSet *)(CEGUI::MultiColumnList *)ptrIn;
	};
	printf("PtrCaster not successful, Class: CEGUI::MultiColumnList is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_MultiColumnList(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_MultiColumnList_PtrCaster);
	return st;
};

//
// CEGUI::MultiLineEditbox
//

// Ptr Caster
void *internalHG3D_MultiLineEditbox_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "CEGUI::MultiLineEditbox") == 0) {
		return ptrIn;
	};
	if (strcmp(className, "CEGUI::Window") == 0) {
		return (void *)(CEGUI::Window *)(CEGUI::MultiLineEditbox *)ptrIn;
	};
	if (strcmp(className, "CEGUI::PropertySet") == 0) {
		return (void *)(CEGUI::PropertySet *)(CEGUI::MultiLineEditbox *)ptrIn;
	};
	if (strcmp(className, "CEGUI::EventSet") == 0) {
		return (void *)(CEGUI::EventSet *)(CEGUI::MultiLineEditbox *)ptrIn;
	};
	printf("PtrCaster not successful, Class: CEGUI::MultiLineEditbox is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_MultiLineEditbox(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_MultiLineEditbox_PtrCaster);
	return st;
};

//
// CEGUI::OgreRenderer
//

// Ptr Caster
void *internalHG3D_OgreRenderer_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "CEGUI::OgreRenderer") == 0) {
		return ptrIn;
	};
	if (strcmp(className, "CEGUI::Renderer") == 0) {
		return (void *)(CEGUI::Renderer *)(CEGUI::OgreRenderer *)ptrIn;
	};
	printf("PtrCaster not successful, Class: CEGUI::OgreRenderer is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_OgreRenderer(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_OgreRenderer_PtrCaster);
	return st;
};

//
// CEGUI::OgreResourceProvider
//

// Ptr Caster
void *internalHG3D_OgreResourceProvider_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "CEGUI::OgreResourceProvider") == 0) {
		return ptrIn;
	};
	if (strcmp(className, "CEGUI::ResourceProvider") == 0) {
		return (void *)(CEGUI::ResourceProvider *)(CEGUI::OgreResourceProvider *)ptrIn;
	};
	printf("PtrCaster not successful, Class: CEGUI::OgreResourceProvider is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_OgreResourceProvider(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_OgreResourceProvider_PtrCaster);
	return st;
};

//
// CEGUI::ProgressBar
//

// Ptr Caster
void *internalHG3D_ProgressBar_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "CEGUI::ProgressBar") == 0) {
		return ptrIn;
	};
	if (strcmp(className, "CEGUI::Window") == 0) {
		return (void *)(CEGUI::Window *)(CEGUI::ProgressBar *)ptrIn;
	};
	if (strcmp(className, "CEGUI::PropertySet") == 0) {
		return (void *)(CEGUI::PropertySet *)(CEGUI::ProgressBar *)ptrIn;
	};
	if (strcmp(className, "CEGUI::EventSet") == 0) {
		return (void *)(CEGUI::EventSet *)(CEGUI::ProgressBar *)ptrIn;
	};
	printf("PtrCaster not successful, Class: CEGUI::ProgressBar is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_ProgressBar(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_ProgressBar_PtrCaster);
	return st;
};

//
// CEGUI::PropertySet
//

// Ptr Caster
void *internalHG3D_PropertySet_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "CEGUI::PropertySet") == 0) {
		return ptrIn;
	};
	printf("PtrCaster not successful, Class: CEGUI::PropertySet is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_PropertySet(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_PropertySet_PtrCaster);
	return st;
};

//
// CEGUI::PushButton
//

// Ptr Caster
void *internalHG3D_PushButton_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "CEGUI::PushButton") == 0) {
		return ptrIn;
	};
	printf("PtrCaster not successful, Class: CEGUI::PushButton is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_PushButton(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_PushButton_PtrCaster);
	return st;
};

//
// CEGUI::RadioButton
//

// Ptr Caster
void *internalHG3D_RadioButton_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "CEGUI::RadioButton") == 0) {
		return ptrIn;
	};
	printf("PtrCaster not successful, Class: CEGUI::RadioButton is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_RadioButton(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_RadioButton_PtrCaster);
	return st;
};

//
// CEGUI::Renderer
//

// Ptr Caster
void *internalHG3D_Renderer_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "CEGUI::Renderer") == 0) {
		return ptrIn;
	};
	printf("PtrCaster not successful, Class: CEGUI::Renderer is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_Renderer(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_Renderer_PtrCaster);
	return st;
};

//
// CEGUI::ResourceProvider
//

// Ptr Caster
void *internalHG3D_ResourceProvider_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "CEGUI::ResourceProvider") == 0) {
		return ptrIn;
	};
	printf("PtrCaster not successful, Class: CEGUI::ResourceProvider is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_ResourceProvider(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_ResourceProvider_PtrCaster);
	return st;
};

//
// CEGUI::Scheme
//

// Ptr Caster
void *internalHG3D_Scheme_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "CEGUI::Scheme") == 0) {
		return ptrIn;
	};
	printf("PtrCaster not successful, Class: CEGUI::Scheme is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_Scheme(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_Scheme_PtrCaster);
	return st;
};

//
// CEGUI::SchemeManager
//

// Ptr Caster
void *internalHG3D_SchemeManager_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "CEGUI::SchemeManager") == 0) {
		return ptrIn;
	};
	printf("PtrCaster not successful, Class: CEGUI::SchemeManager is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_SchemeManager(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_SchemeManager_PtrCaster);
	return st;
};

//
// CEGUI::ScriptFunctor
//

// Ptr Caster
void *internalHG3D_ScriptFunctor_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "CEGUI::ScriptFunctor") == 0) {
		return ptrIn;
	};
	printf("PtrCaster not successful, Class: CEGUI::ScriptFunctor is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_ScriptFunctor(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_ScriptFunctor_PtrCaster);
	return st;
};

//
// CEGUI::ScriptModule
//

// Ptr Caster
void *internalHG3D_ScriptModule_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "CEGUI::ScriptModule") == 0) {
		return ptrIn;
	};
	printf("PtrCaster not successful, Class: CEGUI::ScriptModule is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_ScriptModule(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_ScriptModule_PtrCaster);
	return st;
};

//
// CEGUI::ScrollablePane
//

// Ptr Caster
void *internalHG3D_ScrollablePane_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "CEGUI::ScrollablePane") == 0) {
		return ptrIn;
	};
	if (strcmp(className, "CEGUI::Window") == 0) {
		return (void *)(CEGUI::Window *)(CEGUI::ScrollablePane *)ptrIn;
	};
	if (strcmp(className, "CEGUI::PropertySet") == 0) {
		return (void *)(CEGUI::PropertySet *)(CEGUI::ScrollablePane *)ptrIn;
	};
	if (strcmp(className, "CEGUI::EventSet") == 0) {
		return (void *)(CEGUI::EventSet *)(CEGUI::ScrollablePane *)ptrIn;
	};
	printf("PtrCaster not successful, Class: CEGUI::ScrollablePane is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_ScrollablePane(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_ScrollablePane_PtrCaster);
	return st;
};

//
// CEGUI::Scrollbar
//

// Ptr Caster
void *internalHG3D_Scrollbar_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "CEGUI::Scrollbar") == 0) {
		return ptrIn;
	};
	if (strcmp(className, "CEGUI::Window") == 0) {
		return (void *)(CEGUI::Window *)(CEGUI::Scrollbar *)ptrIn;
	};
	if (strcmp(className, "CEGUI::PropertySet") == 0) {
		return (void *)(CEGUI::PropertySet *)(CEGUI::Scrollbar *)ptrIn;
	};
	if (strcmp(className, "CEGUI::EventSet") == 0) {
		return (void *)(CEGUI::EventSet *)(CEGUI::Scrollbar *)ptrIn;
	};
	printf("PtrCaster not successful, Class: CEGUI::Scrollbar is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_Scrollbar(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_Scrollbar_PtrCaster);
	return st;
};

//
// CEGUI::ScrolledContainer
//

// Ptr Caster
void *internalHG3D_ScrolledContainer_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "CEGUI::ScrolledContainer") == 0) {
		return ptrIn;
	};
	if (strcmp(className, "CEGUI::Window") == 0) {
		return (void *)(CEGUI::Window *)(CEGUI::ScrolledContainer *)ptrIn;
	};
	if (strcmp(className, "CEGUI::PropertySet") == 0) {
		return (void *)(CEGUI::PropertySet *)(CEGUI::ScrolledContainer *)ptrIn;
	};
	if (strcmp(className, "CEGUI::EventSet") == 0) {
		return (void *)(CEGUI::EventSet *)(CEGUI::ScrolledContainer *)ptrIn;
	};
	printf("PtrCaster not successful, Class: CEGUI::ScrolledContainer is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_ScrolledContainer(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_ScrolledContainer_PtrCaster);
	return st;
};

//
// CEGUI::ScrolledItemListBase
//

// Ptr Caster
void *internalHG3D_ScrolledItemListBase_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "CEGUI::ScrolledItemListBase") == 0) {
		return ptrIn;
	};
	printf("PtrCaster not successful, Class: CEGUI::ScrolledItemListBase is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_ScrolledItemListBase(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_ScrolledItemListBase_PtrCaster);
	return st;
};

//
// CEGUI::Slider
//

// Ptr Caster
void *internalHG3D_Slider_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "CEGUI::Slider") == 0) {
		return ptrIn;
	};
	if (strcmp(className, "CEGUI::Window") == 0) {
		return (void *)(CEGUI::Window *)(CEGUI::Slider *)ptrIn;
	};
	if (strcmp(className, "CEGUI::PropertySet") == 0) {
		return (void *)(CEGUI::PropertySet *)(CEGUI::Slider *)ptrIn;
	};
	if (strcmp(className, "CEGUI::EventSet") == 0) {
		return (void *)(CEGUI::EventSet *)(CEGUI::Slider *)ptrIn;
	};
	printf("PtrCaster not successful, Class: CEGUI::Slider is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_Slider(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_Slider_PtrCaster);
	return st;
};

//
// CEGUI::Spinner
//

// Ptr Caster
void *internalHG3D_Spinner_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "CEGUI::Spinner") == 0) {
		return ptrIn;
	};
	if (strcmp(className, "CEGUI::Window") == 0) {
		return (void *)(CEGUI::Window *)(CEGUI::Spinner *)ptrIn;
	};
	if (strcmp(className, "CEGUI::PropertySet") == 0) {
		return (void *)(CEGUI::PropertySet *)(CEGUI::Spinner *)ptrIn;
	};
	if (strcmp(className, "CEGUI::EventSet") == 0) {
		return (void *)(CEGUI::EventSet *)(CEGUI::Spinner *)ptrIn;
	};
	printf("PtrCaster not successful, Class: CEGUI::Spinner is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_Spinner(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_Spinner_PtrCaster);
	return st;
};

//
// CEGUI::System
//

// Ptr Caster
void *internalHG3D_System_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "CEGUI::System") == 0) {
		return ptrIn;
	};
	if (strcmp(className, "CEGUI::EventSet") == 0) {
		return (void *)(CEGUI::EventSet *)(CEGUI::System *)ptrIn;
	};
	printf("PtrCaster not successful, Class: CEGUI::System is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_System(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_System_PtrCaster);
	return st;
};

//
// CEGUI::TabButton
//

// Ptr Caster
void *internalHG3D_TabButton_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "CEGUI::TabButton") == 0) {
		return ptrIn;
	};
	printf("PtrCaster not successful, Class: CEGUI::TabButton is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_TabButton(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_TabButton_PtrCaster);
	return st;
};

//
// CEGUI::Thumb
//

// Ptr Caster
void *internalHG3D_Thumb_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "CEGUI::Thumb") == 0) {
		return ptrIn;
	};
	if (strcmp(className, "CEGUI::PushButton") == 0) {
		return (void *)(CEGUI::PushButton *)(CEGUI::Thumb *)ptrIn;
	};
	printf("PtrCaster not successful, Class: CEGUI::Thumb is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_Thumb(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_Thumb_PtrCaster);
	return st;
};

//
// CEGUI::Tooltip
//

// Ptr Caster
void *internalHG3D_Tooltip_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "CEGUI::Tooltip") == 0) {
		return ptrIn;
	};
	if (strcmp(className, "CEGUI::Window") == 0) {
		return (void *)(CEGUI::Window *)(CEGUI::Tooltip *)ptrIn;
	};
	if (strcmp(className, "CEGUI::PropertySet") == 0) {
		return (void *)(CEGUI::PropertySet *)(CEGUI::Tooltip *)ptrIn;
	};
	if (strcmp(className, "CEGUI::EventSet") == 0) {
		return (void *)(CEGUI::EventSet *)(CEGUI::Tooltip *)ptrIn;
	};
	printf("PtrCaster not successful, Class: CEGUI::Tooltip is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_Tooltip(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_Tooltip_PtrCaster);
	return st;
};

//
// CEGUI::Tree
//

// Ptr Caster
void *internalHG3D_Tree_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "CEGUI::Tree") == 0) {
		return ptrIn;
	};
	if (strcmp(className, "CEGUI::Window") == 0) {
		return (void *)(CEGUI::Window *)(CEGUI::Tree *)ptrIn;
	};
	if (strcmp(className, "CEGUI::PropertySet") == 0) {
		return (void *)(CEGUI::PropertySet *)(CEGUI::Tree *)ptrIn;
	};
	if (strcmp(className, "CEGUI::EventSet") == 0) {
		return (void *)(CEGUI::EventSet *)(CEGUI::Tree *)ptrIn;
	};
	printf("PtrCaster not successful, Class: CEGUI::Tree is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_Tree(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_Tree_PtrCaster);
	return st;
};

//
// CEGUI::WidgetLookManager
//

// Ptr Caster
void *internalHG3D_WidgetLookManager_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "CEGUI::WidgetLookManager") == 0) {
		return ptrIn;
	};
	printf("PtrCaster not successful, Class: CEGUI::WidgetLookManager is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_WidgetLookManager(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_WidgetLookManager_PtrCaster);
	return st;
};

//
// CEGUI::Window
//

// Ptr Caster
void *internalHG3D_Window_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "CEGUI::Window") == 0) {
		return ptrIn;
	};
	if (strcmp(className, "CEGUI::PropertySet") == 0) {
		return (void *)(CEGUI::PropertySet *)(CEGUI::Window *)ptrIn;
	};
	if (strcmp(className, "CEGUI::EventSet") == 0) {
		return (void *)(CEGUI::EventSet *)(CEGUI::Window *)ptrIn;
	};
	printf("PtrCaster not successful, Class: CEGUI::Window is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_Window(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_Window_PtrCaster);
	return st;
};

//
// CEGUI::WindowManager
//

// Ptr Caster
void *internalHG3D_WindowManager_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "CEGUI::WindowManager") == 0) {
		return ptrIn;
	};
	if (strcmp(className, "CEGUI::EventSet") == 0) {
		return (void *)(CEGUI::EventSet *)(CEGUI::WindowManager *)ptrIn;
	};
	printf("PtrCaster not successful, Class: CEGUI::WindowManager is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_WindowManager(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_WindowManager_PtrCaster);
	return st;
};

//
// CEGUI::XMLParser
//

// Ptr Caster
void *internalHG3D_XMLParser_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "CEGUI::XMLParser") == 0) {
		return ptrIn;
	};
	if (strcmp(className, "CEGUI::PropertySet") == 0) {
		return (void *)(CEGUI::PropertySet *)(CEGUI::XMLParser *)ptrIn;
	};
	printf("PtrCaster not successful, Class: CEGUI::XMLParser is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_XMLParser(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_XMLParser_PtrCaster);
	return st;
};

//
// HG3DEventController
//

// Ptr Caster
void *internalHG3D_HG3DEventController_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "HG3DEventController") == 0) {
		return ptrIn;
	};
	printf("PtrCaster not successful, Class: HG3DEventController is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_HG3DEventController(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_HG3DEventController_PtrCaster);
	return st;
};

//
// HG3DEventStaticFunctions
//

// Ptr Caster
void *internalHG3D_HG3DEventStaticFunctions_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "HG3DEventStaticFunctions") == 0) {
		return ptrIn;
	};
	printf("PtrCaster not successful, Class: HG3DEventStaticFunctions is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_HG3DEventStaticFunctions(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_HG3DEventStaticFunctions_PtrCaster);
	return st;
};

//
// HG3DListboxStaticFunctions
//

// Ptr Caster
void *internalHG3D_HG3DListboxStaticFunctions_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "HG3DListboxStaticFunctions") == 0) {
		return ptrIn;
	};
	printf("PtrCaster not successful, Class: HG3DListboxStaticFunctions is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_HG3DListboxStaticFunctions(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_HG3DListboxStaticFunctions_PtrCaster);
	return st;
};

//
// HG3DWindowStaticFunctions
//

// Ptr Caster
void *internalHG3D_HG3DWindowStaticFunctions_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "HG3DWindowStaticFunctions") == 0) {
		return ptrIn;
	};
	printf("PtrCaster not successful, Class: HG3DWindowStaticFunctions is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_HG3DWindowStaticFunctions(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_HG3DWindowStaticFunctions_PtrCaster);
	return st;
};

//
// SystemHG3D
//

// Ptr Caster
void *internalHG3D_SystemHG3D_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "SystemHG3D") == 0) {
		return ptrIn;
	};
	printf("PtrCaster not successful, Class: SystemHG3D is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_SystemHG3D(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_SystemHG3D_PtrCaster);
	return st;
};

//
// WindowManagerHG3D
//

// Ptr Caster
void *internalHG3D_WindowManagerHG3D_PtrCaster(const char* className, void* ptrIn) {
	if (strcmp(className, "WindowManagerHG3D") == 0) {
		return ptrIn;
	};
	printf("PtrCaster not successful, Class: WindowManagerHG3D is not a subclass of %s!\n",className);
	return (void *)0;
};

// getHG3DClass
hg3dclass_struct getHG3DClass_WindowManagerHG3D(void *ptrIn)
{
	hg3dclass_struct st;
	st.ptr = ptrIn;
	st.fptr = (void *)(&internalHG3D_WindowManagerHG3D_PtrCaster);
	return st;
};

