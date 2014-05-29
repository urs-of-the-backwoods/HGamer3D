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

// ClassConfigFile.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassConfigFile
#define _DEFINED_HG3D_ClassConfigFile

#include "ClassPtr.h"


// 
void ogre_cf_construct(struct hg3dclass_struct * result_c);

// 
void ogre_cf_destruct(struct hg3dclass_struct * thisclass_c);

// load from a filename (not using resource group locations) 
void ogre_cf_load(struct hg3dclass_struct * thisclass_c, char * filename_c, char * separators_c, long trimWhitespace_c);

// load from a filename (using resource group locations) 
void ogre_cf_load2(struct hg3dclass_struct * thisclass_c, char * filename_c, char * resourceGroup_c, char * separators_c, long trimWhitespace_c);

// load from a filename (not using resource group locations) 
void ogre_cf_loadDirect(struct hg3dclass_struct * thisclass_c, char * filename_c, char * separators_c, long trimWhitespace_c);

// load from a filename (using resource group locations) 
void ogre_cf_loadFromResourceSystem(struct hg3dclass_struct * thisclass_c, char * filename_c, char * resourceGroup_c, char * separators_c, long trimWhitespace_c);

// 
void ogre_cf_getSetting(struct hg3dclass_struct * thisclass_c, char * key_c, char * section_c, char * defaultValue_c, char * result_c);

// 
void ogre_cf_clear(struct hg3dclass_struct * thisclass_c);

#endif 
