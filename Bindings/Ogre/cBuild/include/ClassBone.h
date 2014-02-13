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

// ClassBone.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassBone
#define _DEFINED_HG3D_ClassBone

#include "ClassPtr.h"
#include "StructVec3.h"
#include "StructQuaternion.h"


// 
void ogre_bn_destruct(struct hg3dclass_struct * thisclass_c);

// 
void ogre_bn_createChild(struct hg3dclass_struct * thisclass_c, unsigned short handle_c, struct vector3_struct * translate_c, struct quaternion_struct * rotate_c, struct hg3dclass_struct * result_c);

// 
void ogre_bn_getHandle(struct hg3dclass_struct * thisclass_c, unsigned short * result_c);

// 
void ogre_bn_setBindingPose(struct hg3dclass_struct * thisclass_c);

// 
void ogre_bn_reset(struct hg3dclass_struct * thisclass_c);

// 
void ogre_bn_setManuallyControlled(struct hg3dclass_struct * thisclass_c, int manuallyControlled_c);

// 
void ogre_bn_isManuallyControlled(struct hg3dclass_struct * thisclass_c, int * result_c);

// 
void ogre_bn_needUpdate(struct hg3dclass_struct * thisclass_c, int forceParentUpdate_c);

#endif 
