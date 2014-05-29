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

// ClassFrustum.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassFrustum
#define _DEFINED_HG3D_ClassFrustum

#include "ClassPtr.h"
#include "StructRadians.h"
#include "StructVec2.h"
#include "StructVec3.h"
#include "EnumFrustumPlane.h"
#include "StructSharedPtr.h"
#include "ClassCamera.h"
#include "EnumProjectionType.h"
#include "StructQuaternion.h"
#include "EnumOrientationMode.h"


// Named constructor. 
void ogre_frst_construct(char * name_c, struct hg3dclass_struct * result_c);

// 
void ogre_frst_destruct(struct hg3dclass_struct * thisclass_c);

// 
void ogre_frst_setFOVy(struct hg3dclass_struct * thisclass_c, struct radian_struct * fovy_c);

// 
void ogre_frst_getFOVy(struct hg3dclass_struct * thisclass_c, struct radian_struct * result_c);

// 
void ogre_frst_setNearClipDistance(struct hg3dclass_struct * thisclass_c, float nearDist_c);

// 
void ogre_frst_getNearClipDistance(struct hg3dclass_struct * thisclass_c, float * result_c);

// 
void ogre_frst_setFarClipDistance(struct hg3dclass_struct * thisclass_c, float farDist_c);

// 
void ogre_frst_getFarClipDistance(struct hg3dclass_struct * thisclass_c, float * result_c);

// 
void ogre_frst_setAspectRatio(struct hg3dclass_struct * thisclass_c, float ratio_c);

// 
void ogre_frst_getAspectRatio(struct hg3dclass_struct * thisclass_c, float * result_c);

// 
void ogre_frst_setFrustumOffset(struct hg3dclass_struct * thisclass_c, struct vector2_struct * offset_c);

// 
void ogre_frst_setFrustumOffset2(struct hg3dclass_struct * thisclass_c, float horizontal_c, float vertical_c);

// 
void ogre_frst_getFrustumOffset(struct hg3dclass_struct * thisclass_c, struct vector2_struct * result_c);

// 
void ogre_frst_setFocalLength(struct hg3dclass_struct * thisclass_c, float focalLength_c);

// 
void ogre_frst_getFocalLength(struct hg3dclass_struct * thisclass_c, float * result_c);

// 
void ogre_frst_setFrustumExtents(struct hg3dclass_struct * thisclass_c, float left_c, float right_c, float top_c, float bottom_c);

// 
void ogre_frst_resetFrustumExtents(struct hg3dclass_struct * thisclass_c);

// 
void ogre_frst_getFrustumExtents(struct hg3dclass_struct * thisclass_c, float * outleft_c, float * outright_c, float * outtop_c, float * outbottom_c);

// Returns whether a custom view matrix is in use. 
void ogre_frst_isCustomViewMatrixEnabled(struct hg3dclass_struct * thisclass_c, long * result_c);

// Returns whether a custom projection matrix is in use. 
void ogre_frst_isCustomProjectionMatrixEnabled(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_frst_isVisible3(struct hg3dclass_struct * thisclass_c, struct vector3_struct * vert_c, enum EnumFrustumPlane * culledBy_c, long * result_c);

// Overridden from MovableObject::getTypeFlags
void ogre_frst_getTypeFlags(struct hg3dclass_struct * thisclass_c, unsigned long * result_c);

// 
void ogre_frst_getBoundingRadius(struct hg3dclass_struct * thisclass_c, float * result_c);

// 
void ogre_frst_getMovableType(struct hg3dclass_struct * thisclass_c, char * result_c);

// 
void ogre_frst_getMaterial(struct hg3dclass_struct * thisclass_c, struct sharedptr_struct * result_c);

// 
void ogre_frst_getSquaredViewDepth(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * cam_c, float * result_c);

// 
void ogre_frst_getWorldSpaceCorners(struct hg3dclass_struct * thisclass_c, struct vector3_struct * result_c);

// 
void ogre_frst_setProjectionType(struct hg3dclass_struct * thisclass_c, enum EnumProjectionType pt_c);

// 
void ogre_frst_getProjectionType(struct hg3dclass_struct * thisclass_c, enum EnumProjectionType * result_c);

// 
void ogre_frst_setOrthoWindow(struct hg3dclass_struct * thisclass_c, float w_c, float h_c);

// 
void ogre_frst_setOrthoWindowHeight(struct hg3dclass_struct * thisclass_c, float h_c);

// 
void ogre_frst_setOrthoWindowWidth(struct hg3dclass_struct * thisclass_c, float w_c);

// 
void ogre_frst_getOrthoWindowHeight(struct hg3dclass_struct * thisclass_c, float * result_c);

// 
void ogre_frst_getOrthoWindowWidth(struct hg3dclass_struct * thisclass_c, float * result_c);

// 
void ogre_frst_disableReflection(struct hg3dclass_struct * thisclass_c);

// Returns whether this frustum is being reflected. 
void ogre_frst_isReflected(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_frst_disableCustomNearClipPlane(struct hg3dclass_struct * thisclass_c);

// 
void ogre_frst_isCustomNearClipPlaneEnabled(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_frst_getPositionForViewUpdate(struct hg3dclass_struct * thisclass_c, struct vector3_struct * result_c);

// 
void ogre_frst_getOrientationForViewUpdate(struct hg3dclass_struct * thisclass_c, struct quaternion_struct * result_c);

// 
void ogre_frst_setOrientationMode(struct hg3dclass_struct * thisclass_c, enum EnumOrientationMode orientationMode_c);

// 
void ogre_frst_getOrientationMode(struct hg3dclass_struct * thisclass_c, enum EnumOrientationMode * result_c);

#endif 
