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

// ClassSceneManagerFactory.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassSceneManagerFactory
#define _DEFINED_HG3D_ClassSceneManagerFactory

#include "ClassPtr.h"
#include "ClassSceneManager.h"


// 
void ogre_smf_destruct(struct hg3dclass_struct * thisclass_c);

// 
void ogre_smf_createInstance(struct hg3dclass_struct * thisclass_c, char * instanceName_c, struct hg3dclass_struct * result_c);

// 
void ogre_smf_destroyInstance(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * instance_c);

#endif 
