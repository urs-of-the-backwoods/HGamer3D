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

// ClassBillboardSetFactory.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassBillboardSetFactory
#define _DEFINED_HG3D_ClassBillboardSetFactory

#include "ClassPtr.h"
#include "ClassMovableObject.h"


// 
void ogre_bbsf_construct(struct hg3dclass_struct * result_c);

// 
void ogre_bbsf_destruct(struct hg3dclass_struct * thisclass_c);

// Get the type of the object to be created. 
void ogre_bbsf_getType(struct hg3dclass_struct * thisclass_c, char * result_c);

// 
void ogre_bbsf_destroyInstance(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * obj_c);

#endif 
