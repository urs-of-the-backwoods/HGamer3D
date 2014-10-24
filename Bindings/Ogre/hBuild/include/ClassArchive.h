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

// ClassArchive.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassArchive
#define _DEFINED_HG3D_ClassArchive

#include "ClassPtr.h"


// 
void ogre_arch_destruct(struct hg3dclass_struct * thisclass_c);

// Get the name of this archive. 
void ogre_arch_getName(struct hg3dclass_struct * thisclass_c, char * result_c);

// Returns whether this archive is case sensitive in the way it matches files. 
void ogre_arch_isCaseSensitive(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_arch_load(struct hg3dclass_struct * thisclass_c);

// 
void ogre_arch_unload(struct hg3dclass_struct * thisclass_c);

// 
void ogre_arch_isReadOnly(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_arch_remove(struct hg3dclass_struct * thisclass_c, char * filename_c);

// 
void ogre_arch_exists(struct hg3dclass_struct * thisclass_c, char * filename_c, long * result_c);

// Return the type code of this Archive
void ogre_arch_getType(struct hg3dclass_struct * thisclass_c, char * result_c);

#endif 
