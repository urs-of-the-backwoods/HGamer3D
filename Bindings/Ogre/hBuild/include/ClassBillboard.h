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

// ClassBillboard.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassBillboard
#define _DEFINED_HG3D_ClassBillboard

#include "ClassPtr.h"
#include "StructRadians.h"
#include "StructVec3.h"
#include "StructColour.h"


// 
void ogre_bbd_construct(struct hg3dclass_struct * result_c);

// 
void ogre_bbd_destruct(struct hg3dclass_struct * thisclass_c);

// 
void ogre_bbd_getRotation(struct hg3dclass_struct * thisclass_c, struct radian_struct * result_c);

// 
void ogre_bbd_setRotation(struct hg3dclass_struct * thisclass_c, struct radian_struct * rotation_c);

// 
void ogre_bbd_setPosition(struct hg3dclass_struct * thisclass_c, struct vector3_struct * position_c);

// 
void ogre_bbd_setPosition2(struct hg3dclass_struct * thisclass_c, float x_c, float y_c, float z_c);

// 
void ogre_bbd_getPosition(struct hg3dclass_struct * thisclass_c, struct vector3_struct * result_c);

// 
void ogre_bbd_setDimensions(struct hg3dclass_struct * thisclass_c, float width_c, float height_c);

// 
void ogre_bbd_resetDimensions(struct hg3dclass_struct * thisclass_c);

// 
void ogre_bbd_setColour(struct hg3dclass_struct * thisclass_c, struct colourvalue_struct * colour_c);

// 
void ogre_bbd_getColour(struct hg3dclass_struct * thisclass_c, struct colourvalue_struct * result_c);

// 
void ogre_bbd_hasOwnDimensions(struct hg3dclass_struct * thisclass_c, int * result_c);

// 
void ogre_bbd_getOwnWidth(struct hg3dclass_struct * thisclass_c, float * result_c);

// 
void ogre_bbd_getOwnHeight(struct hg3dclass_struct * thisclass_c, float * result_c);

// 
void ogre_bbd_isUseTexcoordRect(struct hg3dclass_struct * thisclass_c, int * result_c);

// 
void ogre_bbd_setTexcoordIndex(struct hg3dclass_struct * thisclass_c, unsigned short texcoordIndex_c);

// 
void ogre_bbd_getTexcoordIndex(struct hg3dclass_struct * thisclass_c, unsigned short * result_c);

// 
void ogre_bbd_setTexcoordRect2(struct hg3dclass_struct * thisclass_c, float u0_c, float v0_c, float u1_c, float v1_c);

#endif 
