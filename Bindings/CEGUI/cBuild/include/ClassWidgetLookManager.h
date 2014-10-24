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

// ClassWidgetLookManager.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassWidgetLookManager
#define _DEFINED_HG3D_ClassWidgetLookManager

#include "ClassPtr.h"


// Constructor. 
void cegui_wdgtlmgr_construct(struct hg3dclass_struct * result_c);

// Destructor. 
void cegui_wdgtlmgr_destruct(struct hg3dclass_struct * thisclass_c);

// Parses a file containing window look & feel specifications (in the form of XML). 
void cegui_wdgtlmgr_parseLookNFeelSpecification(struct hg3dclass_struct * thisclass_c, char * filename_c, char * resourceGroup_c);

// Return whether a WidgetLookFeel has been created with the specified name. 
void cegui_wdgtlmgr_isWidgetLookAvailable(struct hg3dclass_struct * thisclass_c, char * widget_c, int * result_c);

// Erase the WidgetLookFeel that has the specified name. 
void cegui_wdgtlmgr_eraseWidgetLook(struct hg3dclass_struct * thisclass_c, char * widget_c);

// Return singleton WidgetLookManager
void cegui_wdgtlmgr_getSingleton(struct hg3dclass_struct * result_c);

// Return pointer to singleton WidgetLookManager
void cegui_wdgtlmgr_getSingletonPtr(struct hg3dclass_struct * result_c);

// Returns the default resource group currently set for LookNFeels. 
void cegui_wdgtlmgr_getDefaultResourceGroup(char * result_c);

// Sets the default resource group to be used when loading LookNFeel data. 
void cegui_wdgtlmgr_setDefaultResourceGroup(char * resourceGroup_c);

#endif 
