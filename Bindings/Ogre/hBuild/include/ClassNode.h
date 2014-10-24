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

// ClassNode.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassNode
#define _DEFINED_HG3D_ClassNode

#include "ClassPtr.h"
#include "StructQuaternion.h"
#include "StructVec3.h"
#include "EnumNodeTransformSpace.h"
#include "StructRadians.h"
#include "ClassCamera.h"


// 
void ogre_nd_destruct(struct hg3dclass_struct * thisclass_c);

// 
void ogre_nd_getName(struct hg3dclass_struct * thisclass_c, char * result_c);

// 
void ogre_nd_getParent(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// 
void ogre_nd_getOrientation(struct hg3dclass_struct * thisclass_c, struct quaternion_struct * result_c);

// 
void ogre_nd_setOrientation(struct hg3dclass_struct * thisclass_c, struct quaternion_struct * q_c);

// 
void ogre_nd_setOrientation2(struct hg3dclass_struct * thisclass_c, float w_c, float x_c, float y_c, float z_c);

// 
void ogre_nd_resetOrientation(struct hg3dclass_struct * thisclass_c);

// 
void ogre_nd_setPosition(struct hg3dclass_struct * thisclass_c, struct vector3_struct * pos_c);

// 
void ogre_nd_setPosition2(struct hg3dclass_struct * thisclass_c, float x_c, float y_c, float z_c);

// 
void ogre_nd_getPosition(struct hg3dclass_struct * thisclass_c, struct vector3_struct * result_c);

// 
void ogre_nd_setScale(struct hg3dclass_struct * thisclass_c, struct vector3_struct * scale_c);

// 
void ogre_nd_setScale2(struct hg3dclass_struct * thisclass_c, float x_c, float y_c, float z_c);

// 
void ogre_nd_getScale(struct hg3dclass_struct * thisclass_c, struct vector3_struct * result_c);

// 
void ogre_nd_setInheritOrientation(struct hg3dclass_struct * thisclass_c, long inherit_c);

// 
void ogre_nd_getInheritOrientation(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_nd_setInheritScale(struct hg3dclass_struct * thisclass_c, long inherit_c);

// 
void ogre_nd_getInheritScale(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_nd_scale(struct hg3dclass_struct * thisclass_c, struct vector3_struct * scale_c);

// 
void ogre_nd_scale2(struct hg3dclass_struct * thisclass_c, float x_c, float y_c, float z_c);

// 
void ogre_nd_translate(struct hg3dclass_struct * thisclass_c, struct vector3_struct * d_c, enum EnumNodeTransformSpace relativeTo_c);

// 
void ogre_nd_translate2(struct hg3dclass_struct * thisclass_c, float x_c, float y_c, float z_c, enum EnumNodeTransformSpace relativeTo_c);

// 
void ogre_nd_roll(struct hg3dclass_struct * thisclass_c, struct radian_struct * angle_c, enum EnumNodeTransformSpace relativeTo_c);

// 
void ogre_nd_pitch(struct hg3dclass_struct * thisclass_c, struct radian_struct * angle_c, enum EnumNodeTransformSpace relativeTo_c);

// 
void ogre_nd_yaw(struct hg3dclass_struct * thisclass_c, struct radian_struct * angle_c, enum EnumNodeTransformSpace relativeTo_c);

// 
void ogre_nd_rotate(struct hg3dclass_struct * thisclass_c, struct vector3_struct * axis_c, struct radian_struct * angle_c, enum EnumNodeTransformSpace relativeTo_c);

// 
void ogre_nd_rotate2(struct hg3dclass_struct * thisclass_c, struct quaternion_struct * q_c, enum EnumNodeTransformSpace relativeTo_c);

// 
void ogre_nd_createChild(struct hg3dclass_struct * thisclass_c, struct vector3_struct * translate_c, struct quaternion_struct * rotate_c, struct hg3dclass_struct * result_c);

// 
void ogre_nd_createChild2(struct hg3dclass_struct * thisclass_c, char * name_c, struct vector3_struct * translate_c, struct quaternion_struct * rotate_c, struct hg3dclass_struct * result_c);

// 
void ogre_nd_addChild(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * child_c);

// 
void ogre_nd_numChildren(struct hg3dclass_struct * thisclass_c, unsigned short * result_c);

// 
void ogre_nd_getChild(struct hg3dclass_struct * thisclass_c, unsigned short index_c, struct hg3dclass_struct * result_c);

// 
void ogre_nd_getChild2(struct hg3dclass_struct * thisclass_c, char * name_c, struct hg3dclass_struct * result_c);

// 
void ogre_nd_removeChild(struct hg3dclass_struct * thisclass_c, unsigned short index_c, struct hg3dclass_struct * result_c);

// 
void ogre_nd_removeChild2(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * child_c, struct hg3dclass_struct * result_c);

// 
void ogre_nd_removeChild3(struct hg3dclass_struct * thisclass_c, char * name_c, struct hg3dclass_struct * result_c);

// 
void ogre_nd_removeAllChildren(struct hg3dclass_struct * thisclass_c);

// 
void ogre_nd_setInitialState(struct hg3dclass_struct * thisclass_c);

// 
void ogre_nd_resetToInitialState(struct hg3dclass_struct * thisclass_c);

// 
void ogre_nd_getInitialPosition(struct hg3dclass_struct * thisclass_c, struct vector3_struct * result_c);

// 
void ogre_nd_convertWorldToLocalPosition(struct hg3dclass_struct * thisclass_c, struct vector3_struct * worldPos_c, struct vector3_struct * result_c);

// 
void ogre_nd_convertLocalToWorldPosition(struct hg3dclass_struct * thisclass_c, struct vector3_struct * localPos_c, struct vector3_struct * result_c);

// 
void ogre_nd_convertWorldToLocalOrientation(struct hg3dclass_struct * thisclass_c, struct quaternion_struct * worldOrientation_c, struct quaternion_struct * result_c);

// 
void ogre_nd_convertLocalToWorldOrientation(struct hg3dclass_struct * thisclass_c, struct quaternion_struct * localOrientation_c, struct quaternion_struct * result_c);

// 
void ogre_nd_getInitialOrientation(struct hg3dclass_struct * thisclass_c, struct quaternion_struct * result_c);

// 
void ogre_nd_getInitialScale(struct hg3dclass_struct * thisclass_c, struct vector3_struct * result_c);

// 
void ogre_nd_getSquaredViewDepth(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * cam_c, float * result_c);

// 
void ogre_nd_needUpdate(struct hg3dclass_struct * thisclass_c, long forceParentUpdate_c);

// 
void ogre_nd_requestUpdate(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * child_c, long forceParentUpdate_c);

// 
void ogre_nd_cancelUpdate(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * child_c);

// 
void ogre_nd_queueNeedUpdate(struct hg3dclass_struct * n_c);

// 
void ogre_nd_processQueuedUpdates();

#endif 
