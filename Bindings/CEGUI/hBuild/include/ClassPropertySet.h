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

// ClassPropertySet.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassPropertySet
#define _DEFINED_HG3D_ClassPropertySet

#include "ClassPtr.h"


// Constructs a new PropertySet
void cegui_prpst_construct(struct hg3dclass_struct * result_c);

// Destructor for PropertySet
void cegui_prpst_destruct(struct hg3dclass_struct * thisclass_c);

// Removes a Property from the PropertySet
void cegui_prpst_removeProperty(struct hg3dclass_struct * thisclass_c, char * name_c);

// Removes all Property objects from the PropertySet
void cegui_prpst_clearProperties(struct hg3dclass_struct * thisclass_c);

// Checks to see if a Property with the given name is in the PropertySet
void cegui_prpst_isPropertyPresent(struct hg3dclass_struct * thisclass_c, char * name_c, int * result_c);

// Return the help text for the specified Property. 
void cegui_prpst_getPropertyHelp(struct hg3dclass_struct * thisclass_c, char * name_c, char * result_c);

// Gets the current value of the specified Property. 
void cegui_prpst_getProperty(struct hg3dclass_struct * thisclass_c, char * name_c, char * result_c);

// Sets the current value of a Property. 
void cegui_prpst_setProperty(struct hg3dclass_struct * thisclass_c, char * name_c, char * value_c);

// Returns whether a Property is at it's default value. 
void cegui_prpst_isPropertyDefault(struct hg3dclass_struct * thisclass_c, char * name_c, int * result_c);

// Returns the default value of a Property as a String. 
void cegui_prpst_getPropertyDefault(struct hg3dclass_struct * thisclass_c, char * name_c, char * result_c);

#endif 
