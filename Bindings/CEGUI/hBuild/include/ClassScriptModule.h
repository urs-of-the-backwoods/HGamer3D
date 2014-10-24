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

// ClassScriptModule.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassScriptModule
#define _DEFINED_HG3D_ClassScriptModule

#include "ClassPtr.h"
#include "ClassEventArgs.h"


// Destructor for ScriptModule
void cegui_scrmd_destruct(struct hg3dclass_struct * thisclass_c);

// Execute a script file. 
void cegui_scrmd_executeScriptFile(struct hg3dclass_struct * thisclass_c, char * filename_c, char * resourceGroup_c);

// Execute a scripted global function. The function should not take any parameters and should return an integer. 
void cegui_scrmd_executeScriptGlobal(struct hg3dclass_struct * thisclass_c, char * function_name_c, int * result_c);

// Execute a scripted global 'event handler' function. The function should take some kind of EventArgsEventArgs
void cegui_scrmd_executeScriptedEventHandler(struct hg3dclass_struct * thisclass_c, char * handler_name_c, struct hg3dclass_struct * e_c, int * result_c);

// Execute script code contained in the given CEGUI::String object. 
void cegui_scrmd_executeString(struct hg3dclass_struct * thisclass_c, char * str_c);

// Method called during system initialisation, prior to running any scripts via the ScriptModuleScriptModule
void cegui_scrmd_createBindings(struct hg3dclass_struct * thisclass_c);

// Method called during system destruction, after all scripts have been run via the ScriptModuleScriptModule
void cegui_scrmd_destroyBindings(struct hg3dclass_struct * thisclass_c);

// Return identification string for the ScriptModuleScriptModule
void cegui_scrmd_getIdentifierString(struct hg3dclass_struct * thisclass_c, char * result_c);

// Sets the default resource group to be used when loading script files. 
void cegui_scrmd_setDefaultResourceGroup(char * resourceGroup_c);

// Returns the default resource group used when loading script files. 
void cegui_scrmd_getDefaultResourceGroup(char * result_c);

#endif 
