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

// ClassCamera.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassCamera
#define _DEFINED_HG3D_ClassCamera

#include "ClassPtr.h"
#include "ClassSceneManager.h"
#include "StructVec3.h"
#include "StructRadians.h"
#include "StructQuaternion.h"
#include "ClassSceneNode.h"
#include "ClassViewport.h"
#include "ClassFrustum.h"
#include "EnumFrustumPlane.h"


// 
void ogre_cam_construct(char * name_c, struct hg3dclass_struct * sm_c, struct hg3dclass_struct * result_c);

// 
void ogre_cam_destruct(struct hg3dclass_struct * thisclass_c);

// 
void ogre_cam_getSceneManager(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// 
void ogre_cam_setPosition(struct hg3dclass_struct * thisclass_c, float x_c, float y_c, float z_c);

// 
void ogre_cam_setPosition2(struct hg3dclass_struct * thisclass_c, struct vector3_struct * vec_c);

// 
void ogre_cam_getPosition(struct hg3dclass_struct * thisclass_c, struct vector3_struct * result_c);

// 
void ogre_cam_move(struct hg3dclass_struct * thisclass_c, struct vector3_struct * vec_c);

// 
void ogre_cam_moveRelative(struct hg3dclass_struct * thisclass_c, struct vector3_struct * vec_c);

// 
void ogre_cam_setDirection(struct hg3dclass_struct * thisclass_c, float x_c, float y_c, float z_c);

// 
void ogre_cam_setDirection2(struct hg3dclass_struct * thisclass_c, struct vector3_struct * vec_c);

// 
void ogre_cam_getDirection(struct hg3dclass_struct * thisclass_c, struct vector3_struct * result_c);

// 
void ogre_cam_getUp(struct hg3dclass_struct * thisclass_c, struct vector3_struct * result_c);

// 
void ogre_cam_getRight(struct hg3dclass_struct * thisclass_c, struct vector3_struct * result_c);

// 
void ogre_cam_lookAt(struct hg3dclass_struct * thisclass_c, struct vector3_struct * targetPoint_c);

// 
void ogre_cam_lookAt2(struct hg3dclass_struct * thisclass_c, float x_c, float y_c, float z_c);

// 
void ogre_cam_roll(struct hg3dclass_struct * thisclass_c, struct radian_struct * angle_c);

// 
void ogre_cam_yaw(struct hg3dclass_struct * thisclass_c, struct radian_struct * angle_c);

// 
void ogre_cam_pitch(struct hg3dclass_struct * thisclass_c, struct radian_struct * angle_c);

// 
void ogre_cam_rotate(struct hg3dclass_struct * thisclass_c, struct vector3_struct * axis_c, struct radian_struct * angle_c);

// 
void ogre_cam_rotate2(struct hg3dclass_struct * thisclass_c, struct quaternion_struct * q_c);

// 
void ogre_cam_setFixedYawAxis(struct hg3dclass_struct * thisclass_c, long useFixed_c, struct vector3_struct * fixedAxis_c);

// 
void ogre_cam_getOrientation(struct hg3dclass_struct * thisclass_c, struct quaternion_struct * result_c);

// 
void ogre_cam_setOrientation(struct hg3dclass_struct * thisclass_c, struct quaternion_struct * q_c);

// 
void ogre_cam_getDerivedOrientation(struct hg3dclass_struct * thisclass_c, struct quaternion_struct * result_c);

// 
void ogre_cam_getDerivedPosition(struct hg3dclass_struct * thisclass_c, struct vector3_struct * result_c);

// 
void ogre_cam_getDerivedDirection(struct hg3dclass_struct * thisclass_c, struct vector3_struct * result_c);

// 
void ogre_cam_getDerivedUp(struct hg3dclass_struct * thisclass_c, struct vector3_struct * result_c);

// 
void ogre_cam_getDerivedRight(struct hg3dclass_struct * thisclass_c, struct vector3_struct * result_c);

// 
void ogre_cam_getRealOrientation(struct hg3dclass_struct * thisclass_c, struct quaternion_struct * result_c);

// 
void ogre_cam_getRealPosition(struct hg3dclass_struct * thisclass_c, struct vector3_struct * result_c);

// 
void ogre_cam_getRealDirection(struct hg3dclass_struct * thisclass_c, struct vector3_struct * result_c);

// 
void ogre_cam_getRealUp(struct hg3dclass_struct * thisclass_c, struct vector3_struct * result_c);

// 
void ogre_cam_getRealRight(struct hg3dclass_struct * thisclass_c, struct vector3_struct * result_c);

// 
void ogre_cam_getMovableType(struct hg3dclass_struct * thisclass_c, char * result_c);

// 
void ogre_cam_setLodBias(struct hg3dclass_struct * thisclass_c, float factor_c);

// 
void ogre_cam_getLodBias(struct hg3dclass_struct * thisclass_c, float * result_c);

// 
void ogre_cam_setLodCamera(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * lodCam_c);

// 
void ogre_cam_getLodCamera(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// 
void ogre_cam_setWindow(struct hg3dclass_struct * thisclass_c, float Left_c, float Top_c, float Right_c, float Bottom_c);

// Cancel view window. 
void ogre_cam_resetWindow(struct hg3dclass_struct * thisclass_c);

// Returns if a viewport window is being used. 
void ogre_cam_isWindowSet(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_cam_getBoundingRadius(struct hg3dclass_struct * thisclass_c, float * result_c);

// 
void ogre_cam_getAutoTrackTarget(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// 
void ogre_cam_getAutoTrackOffset(struct hg3dclass_struct * thisclass_c, struct vector3_struct * result_c);

// 
void ogre_cam_getViewport(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// 
void ogre_cam_setAutoAspectRatio(struct hg3dclass_struct * thisclass_c, long autoratio_c);

// 
void ogre_cam_getAutoAspectRatio(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_cam_setCullingFrustum(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * frustum_c);

// 
void ogre_cam_getCullingFrustum(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// 
void ogre_cam_isVisible3(struct hg3dclass_struct * thisclass_c, struct vector3_struct * vert_c, enum EnumFrustumPlane * culledBy_c, long * result_c);

// 
void ogre_cam_getWorldSpaceCorners(struct hg3dclass_struct * thisclass_c, struct vector3_struct * result_c);

// 
void ogre_cam_getNearClipDistance(struct hg3dclass_struct * thisclass_c, float * result_c);

// 
void ogre_cam_getFarClipDistance(struct hg3dclass_struct * thisclass_c, float * result_c);

// 
void ogre_cam_setUseRenderingDistance(struct hg3dclass_struct * thisclass_c, long use_c);

// 
void ogre_cam_getUseRenderingDistance(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_cam_synchroniseBaseSettingsWith(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * cam_c);

// 
void ogre_cam_getPositionForViewUpdate(struct hg3dclass_struct * thisclass_c, struct vector3_struct * result_c);

// 
void ogre_cam_getOrientationForViewUpdate(struct hg3dclass_struct * thisclass_c, struct quaternion_struct * result_c);

// Sets whether to use min display size calculations 

void ogre_cam_setUseMinPixelSize(struct hg3dclass_struct * thisclass_c, long enable_c);

// 
void ogre_cam_getUseMinPixelSize(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_cam_getPixelDisplayRatio(struct hg3dclass_struct * thisclass_c, float * result_c);

#endif 
