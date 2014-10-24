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


typedef void ClassCheckbox; 
hg3dclass_struct getHG3DClass_Checkbox(void *ptrIn);

typedef void ClassCombobox; 
hg3dclass_struct getHG3DClass_Combobox(void *ptrIn);

typedef void ClassComboDropList; 
hg3dclass_struct getHG3DClass_ComboDropList(void *ptrIn);

typedef void ClassDefaultLogger; 
hg3dclass_struct getHG3DClass_DefaultLogger(void *ptrIn);

typedef void ClassDefaultResourceProvider; 
hg3dclass_struct getHG3DClass_DefaultResourceProvider(void *ptrIn);

typedef void ClassDragContainer; 
hg3dclass_struct getHG3DClass_DragContainer(void *ptrIn);

typedef void ClassEditbox; 
hg3dclass_struct getHG3DClass_Editbox(void *ptrIn);

typedef void ClassEventArgs; 
hg3dclass_struct getHG3DClass_EventArgs(void *ptrIn);

typedef void ClassEventSet; 
hg3dclass_struct getHG3DClass_EventSet(void *ptrIn);

typedef void ClassFont; 
hg3dclass_struct getHG3DClass_Font(void *ptrIn);

typedef void ClassFontManager; 
hg3dclass_struct getHG3DClass_FontManager(void *ptrIn);

typedef void ClassFrameWindow; 
hg3dclass_struct getHG3DClass_FrameWindow(void *ptrIn);

typedef void ClassImageCodec; 
hg3dclass_struct getHG3DClass_ImageCodec(void *ptrIn);

typedef void ClassImageset; 
hg3dclass_struct getHG3DClass_Imageset(void *ptrIn);

typedef void ClassItemEntry; 
hg3dclass_struct getHG3DClass_ItemEntry(void *ptrIn);

typedef void ClassItemListbox; 
hg3dclass_struct getHG3DClass_ItemListbox(void *ptrIn);

typedef void ClassListbox; 
hg3dclass_struct getHG3DClass_Listbox(void *ptrIn);

typedef void ClassListboxItem; 
hg3dclass_struct getHG3DClass_ListboxItem(void *ptrIn);

typedef void ClassListboxTextItem; 
hg3dclass_struct getHG3DClass_ListboxTextItem(void *ptrIn);

typedef void ClassListHeader; 
hg3dclass_struct getHG3DClass_ListHeader(void *ptrIn);

typedef void ClassListHeaderSegment; 
hg3dclass_struct getHG3DClass_ListHeaderSegment(void *ptrIn);

typedef void ClassLogger; 
hg3dclass_struct getHG3DClass_Logger(void *ptrIn);

typedef void ClassMenuBase; 
hg3dclass_struct getHG3DClass_MenuBase(void *ptrIn);

typedef void ClassMenuItem; 
hg3dclass_struct getHG3DClass_MenuItem(void *ptrIn);

typedef void ClassMultiColumnList; 
hg3dclass_struct getHG3DClass_MultiColumnList(void *ptrIn);

typedef void ClassMultiLineEditbox; 
hg3dclass_struct getHG3DClass_MultiLineEditbox(void *ptrIn);

typedef void ClassOgreRenderer; 
hg3dclass_struct getHG3DClass_OgreRenderer(void *ptrIn);

typedef void ClassOgreResourceProvider; 
hg3dclass_struct getHG3DClass_OgreResourceProvider(void *ptrIn);

typedef void ClassProgressBar; 
hg3dclass_struct getHG3DClass_ProgressBar(void *ptrIn);

typedef void ClassPropertySet; 
hg3dclass_struct getHG3DClass_PropertySet(void *ptrIn);

typedef void ClassPushButton; 
hg3dclass_struct getHG3DClass_PushButton(void *ptrIn);

typedef void ClassRadioButton; 
hg3dclass_struct getHG3DClass_RadioButton(void *ptrIn);

typedef void ClassRenderer; 
hg3dclass_struct getHG3DClass_Renderer(void *ptrIn);

typedef void ClassResourceProvider; 
hg3dclass_struct getHG3DClass_ResourceProvider(void *ptrIn);

typedef void ClassScheme; 
hg3dclass_struct getHG3DClass_Scheme(void *ptrIn);

typedef void ClassSchemeManager; 
hg3dclass_struct getHG3DClass_SchemeManager(void *ptrIn);

typedef void ClassScriptFunctor; 
hg3dclass_struct getHG3DClass_ScriptFunctor(void *ptrIn);

typedef void ClassScriptModule; 
hg3dclass_struct getHG3DClass_ScriptModule(void *ptrIn);

typedef void ClassScrollablePane; 
hg3dclass_struct getHG3DClass_ScrollablePane(void *ptrIn);

typedef void ClassScrollbar; 
hg3dclass_struct getHG3DClass_Scrollbar(void *ptrIn);

typedef void ClassScrolledContainer; 
hg3dclass_struct getHG3DClass_ScrolledContainer(void *ptrIn);

typedef void ClassScrolledItemListBase; 
hg3dclass_struct getHG3DClass_ScrolledItemListBase(void *ptrIn);

typedef void ClassSlider; 
hg3dclass_struct getHG3DClass_Slider(void *ptrIn);

typedef void ClassSpinner; 
hg3dclass_struct getHG3DClass_Spinner(void *ptrIn);

typedef void ClassSystem; 
hg3dclass_struct getHG3DClass_System(void *ptrIn);

typedef void ClassTabButton; 
hg3dclass_struct getHG3DClass_TabButton(void *ptrIn);

typedef void ClassThumb; 
hg3dclass_struct getHG3DClass_Thumb(void *ptrIn);

typedef void ClassTooltip; 
hg3dclass_struct getHG3DClass_Tooltip(void *ptrIn);

typedef void ClassTree; 
hg3dclass_struct getHG3DClass_Tree(void *ptrIn);

typedef void ClassUDim; 
hg3dclass_struct getHG3DClass_UDim(void *ptrIn);

typedef void ClassUVector2; 
hg3dclass_struct getHG3DClass_UVector2(void *ptrIn);

typedef void ClassWidgetLookManager; 
hg3dclass_struct getHG3DClass_WidgetLookManager(void *ptrIn);

typedef void ClassWindow; 
hg3dclass_struct getHG3DClass_Window(void *ptrIn);

typedef void ClassWindowManager; 
hg3dclass_struct getHG3DClass_WindowManager(void *ptrIn);

typedef void ClassXMLParser; 
hg3dclass_struct getHG3DClass_XMLParser(void *ptrIn);

typedef void ClassHG3DEventController; 
hg3dclass_struct getHG3DClass_HG3DEventController(void *ptrIn);

typedef void ClassHG3DEventStaticFunctions; 
hg3dclass_struct getHG3DClass_HG3DEventStaticFunctions(void *ptrIn);

typedef void ClassHG3DListboxStaticFunctions; 
hg3dclass_struct getHG3DClass_HG3DListboxStaticFunctions(void *ptrIn);

typedef void ClassHG3DWindowStaticFunctions; 
hg3dclass_struct getHG3DClass_HG3DWindowStaticFunctions(void *ptrIn);

typedef void ClassSystemHG3D; 
hg3dclass_struct getHG3DClass_SystemHG3D(void *ptrIn);

typedef void ClassWindowManagerHG3D; 
hg3dclass_struct getHG3DClass_WindowManagerHG3D(void *ptrIn);

#endif
