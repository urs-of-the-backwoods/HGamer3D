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

// ClassCamera.cpp

// 

#include <wchar.h>
#include <string>
#include <iostream>

#include <iostream>
	#include <typeinfo>
	#include <stdio.h>
	#include <cstring>
	#include <exception>
	#include "OgreDllDefines.h"
	#include "ClassPtr.h"
	#include "StructVec3.h"
#include "StructRadians.h"
#include "StructQuaternion.h"
#include "EnumFrustumPlane.h"
#include "./Ogre.h"
#include "./OgreString.h"
#include "./WindowUtilsHG3D.h"
#include "HG3DUtilities.h"

using namespace Ogre;
using Ogre::uint;



// 
extern "C" Ogre_LIB_EXPORT void ogre_cam_construct(char * name_c, struct hg3dclass_struct * sm_c, struct hg3dclass_struct * result_c)
{
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  Ogre::SceneManager * sm_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*sm_c, "Ogre::SceneManager"));
  Ogre::Camera * result_cpp;
  result_cpp = (new Ogre::Camera(name_cpp, sm_cpp));
  *result_c = getHG3DClass_Camera((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_cam_destruct(struct hg3dclass_struct * thisclass_c)
{
  Ogre::Camera * thisclass_cpp = static_cast<Ogre::Camera*> (getHG3DClassPtr(*thisclass_c, "Ogre::Camera"));
  (delete thisclass_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_cam_getSceneManager(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  Ogre::Camera * thisclass_cpp = static_cast<Ogre::Camera*> (getHG3DClassPtr(*thisclass_c, "Ogre::Camera"));
  Ogre::SceneManager * result_cpp;
  result_cpp = (thisclass_cpp->getSceneManager());
  *result_c = getHG3DClass_SceneManager((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_cam_setPosition(struct hg3dclass_struct * thisclass_c, float x_c, float y_c, float z_c)
{
  Ogre::Camera * thisclass_cpp = static_cast<Ogre::Camera*> (getHG3DClassPtr(*thisclass_c, "Ogre::Camera"));
  Real x_cpp = (Real)x_c;
  Real y_cpp = (Real)y_c;
  Real z_cpp = (Real)z_c;
  (thisclass_cpp->setPosition(x_cpp, y_cpp, z_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_cam_setPosition2(struct hg3dclass_struct * thisclass_c, struct vector3_struct * vec_c)
{
  Ogre::Camera * thisclass_cpp = static_cast<Ogre::Camera*> (getHG3DClassPtr(*thisclass_c, "Ogre::Camera"));
  Vector3 vec_cpp = *((Vector3*) vec_c);
  (thisclass_cpp->setPosition(vec_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_cam_getPosition(struct hg3dclass_struct * thisclass_c, struct vector3_struct * result_c)
{
  Ogre::Camera * thisclass_cpp = static_cast<Ogre::Camera*> (getHG3DClassPtr(*thisclass_c, "Ogre::Camera"));
  Vector3 result_cpp;
  result_cpp = (thisclass_cpp->getPosition());
  *result_c = *((struct vector3_struct*) &result_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_cam_move(struct hg3dclass_struct * thisclass_c, struct vector3_struct * vec_c)
{
  Ogre::Camera * thisclass_cpp = static_cast<Ogre::Camera*> (getHG3DClassPtr(*thisclass_c, "Ogre::Camera"));
  Vector3 vec_cpp = *((Vector3*) vec_c);
  (thisclass_cpp->move(vec_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_cam_moveRelative(struct hg3dclass_struct * thisclass_c, struct vector3_struct * vec_c)
{
  Ogre::Camera * thisclass_cpp = static_cast<Ogre::Camera*> (getHG3DClassPtr(*thisclass_c, "Ogre::Camera"));
  Vector3 vec_cpp = *((Vector3*) vec_c);
  (thisclass_cpp->moveRelative(vec_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_cam_setDirection(struct hg3dclass_struct * thisclass_c, float x_c, float y_c, float z_c)
{
  Ogre::Camera * thisclass_cpp = static_cast<Ogre::Camera*> (getHG3DClassPtr(*thisclass_c, "Ogre::Camera"));
  Real x_cpp = (Real)x_c;
  Real y_cpp = (Real)y_c;
  Real z_cpp = (Real)z_c;
  (thisclass_cpp->setDirection(x_cpp, y_cpp, z_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_cam_setDirection2(struct hg3dclass_struct * thisclass_c, struct vector3_struct * vec_c)
{
  Ogre::Camera * thisclass_cpp = static_cast<Ogre::Camera*> (getHG3DClassPtr(*thisclass_c, "Ogre::Camera"));
  Vector3 vec_cpp = *((Vector3*) vec_c);
  (thisclass_cpp->setDirection(vec_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_cam_getDirection(struct hg3dclass_struct * thisclass_c, struct vector3_struct * result_c)
{
  Ogre::Camera * thisclass_cpp = static_cast<Ogre::Camera*> (getHG3DClassPtr(*thisclass_c, "Ogre::Camera"));
  Vector3 result_cpp;
  result_cpp = (thisclass_cpp->getDirection());
  *result_c = *((struct vector3_struct*) &result_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_cam_getUp(struct hg3dclass_struct * thisclass_c, struct vector3_struct * result_c)
{
  Ogre::Camera * thisclass_cpp = static_cast<Ogre::Camera*> (getHG3DClassPtr(*thisclass_c, "Ogre::Camera"));
  Vector3 result_cpp;
  result_cpp = (thisclass_cpp->getUp());
  *result_c = *((struct vector3_struct*) &result_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_cam_getRight(struct hg3dclass_struct * thisclass_c, struct vector3_struct * result_c)
{
  Ogre::Camera * thisclass_cpp = static_cast<Ogre::Camera*> (getHG3DClassPtr(*thisclass_c, "Ogre::Camera"));
  Vector3 result_cpp;
  result_cpp = (thisclass_cpp->getRight());
  *result_c = *((struct vector3_struct*) &result_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_cam_lookAt(struct hg3dclass_struct * thisclass_c, struct vector3_struct * targetPoint_c)
{
  Ogre::Camera * thisclass_cpp = static_cast<Ogre::Camera*> (getHG3DClassPtr(*thisclass_c, "Ogre::Camera"));
  Vector3 targetPoint_cpp = *((Vector3*) targetPoint_c);
  (thisclass_cpp->lookAt(targetPoint_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_cam_lookAt2(struct hg3dclass_struct * thisclass_c, float x_c, float y_c, float z_c)
{
  Ogre::Camera * thisclass_cpp = static_cast<Ogre::Camera*> (getHG3DClassPtr(*thisclass_c, "Ogre::Camera"));
  Real x_cpp = (Real)x_c;
  Real y_cpp = (Real)y_c;
  Real z_cpp = (Real)z_c;
  (thisclass_cpp->lookAt(x_cpp, y_cpp, z_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_cam_roll(struct hg3dclass_struct * thisclass_c, struct radian_struct * angle_c)
{
  Ogre::Camera * thisclass_cpp = static_cast<Ogre::Camera*> (getHG3DClassPtr(*thisclass_c, "Ogre::Camera"));
  Radian angle_cpp = *((Radian*) angle_c);
  (thisclass_cpp->roll(angle_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_cam_yaw(struct hg3dclass_struct * thisclass_c, struct radian_struct * angle_c)
{
  Ogre::Camera * thisclass_cpp = static_cast<Ogre::Camera*> (getHG3DClassPtr(*thisclass_c, "Ogre::Camera"));
  Radian angle_cpp = *((Radian*) angle_c);
  (thisclass_cpp->yaw(angle_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_cam_pitch(struct hg3dclass_struct * thisclass_c, struct radian_struct * angle_c)
{
  Ogre::Camera * thisclass_cpp = static_cast<Ogre::Camera*> (getHG3DClassPtr(*thisclass_c, "Ogre::Camera"));
  Radian angle_cpp = *((Radian*) angle_c);
  (thisclass_cpp->pitch(angle_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_cam_rotate(struct hg3dclass_struct * thisclass_c, struct vector3_struct * axis_c, struct radian_struct * angle_c)
{
  Ogre::Camera * thisclass_cpp = static_cast<Ogre::Camera*> (getHG3DClassPtr(*thisclass_c, "Ogre::Camera"));
  Vector3 axis_cpp = *((Vector3*) axis_c);
  Radian angle_cpp = *((Radian*) angle_c);
  (thisclass_cpp->rotate(axis_cpp, angle_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_cam_rotate2(struct hg3dclass_struct * thisclass_c, struct quaternion_struct * q_c)
{
  Ogre::Camera * thisclass_cpp = static_cast<Ogre::Camera*> (getHG3DClassPtr(*thisclass_c, "Ogre::Camera"));
  Quaternion q_cpp = *((Quaternion*) q_c);
  (thisclass_cpp->rotate(q_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_cam_setFixedYawAxis(struct hg3dclass_struct * thisclass_c, int useFixed_c, struct vector3_struct * fixedAxis_c)
{
  Ogre::Camera * thisclass_cpp = static_cast<Ogre::Camera*> (getHG3DClassPtr(*thisclass_c, "Ogre::Camera"));
  bool useFixed_cpp = (bool)useFixed_c;
  Vector3 fixedAxis_cpp = *((Vector3*) fixedAxis_c);
  (thisclass_cpp->setFixedYawAxis(useFixed_cpp, fixedAxis_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_cam_getOrientation(struct hg3dclass_struct * thisclass_c, struct quaternion_struct * result_c)
{
  Ogre::Camera * thisclass_cpp = static_cast<Ogre::Camera*> (getHG3DClassPtr(*thisclass_c, "Ogre::Camera"));
  Quaternion result_cpp;
  result_cpp = (thisclass_cpp->getOrientation());
  *result_c = *((struct quaternion_struct*) &result_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_cam_setOrientation(struct hg3dclass_struct * thisclass_c, struct quaternion_struct * q_c)
{
  Ogre::Camera * thisclass_cpp = static_cast<Ogre::Camera*> (getHG3DClassPtr(*thisclass_c, "Ogre::Camera"));
  Quaternion q_cpp = *((Quaternion*) q_c);
  (thisclass_cpp->setOrientation(q_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_cam_getDerivedOrientation(struct hg3dclass_struct * thisclass_c, struct quaternion_struct * result_c)
{
  Ogre::Camera * thisclass_cpp = static_cast<Ogre::Camera*> (getHG3DClassPtr(*thisclass_c, "Ogre::Camera"));
  Quaternion result_cpp;
  result_cpp = (thisclass_cpp->getDerivedOrientation());
  *result_c = *((struct quaternion_struct*) &result_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_cam_getDerivedPosition(struct hg3dclass_struct * thisclass_c, struct vector3_struct * result_c)
{
  Ogre::Camera * thisclass_cpp = static_cast<Ogre::Camera*> (getHG3DClassPtr(*thisclass_c, "Ogre::Camera"));
  Vector3 result_cpp;
  result_cpp = (thisclass_cpp->getDerivedPosition());
  *result_c = *((struct vector3_struct*) &result_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_cam_getDerivedDirection(struct hg3dclass_struct * thisclass_c, struct vector3_struct * result_c)
{
  Ogre::Camera * thisclass_cpp = static_cast<Ogre::Camera*> (getHG3DClassPtr(*thisclass_c, "Ogre::Camera"));
  Vector3 result_cpp;
  result_cpp = (thisclass_cpp->getDerivedDirection());
  *result_c = *((struct vector3_struct*) &result_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_cam_getDerivedUp(struct hg3dclass_struct * thisclass_c, struct vector3_struct * result_c)
{
  Ogre::Camera * thisclass_cpp = static_cast<Ogre::Camera*> (getHG3DClassPtr(*thisclass_c, "Ogre::Camera"));
  Vector3 result_cpp;
  result_cpp = (thisclass_cpp->getDerivedUp());
  *result_c = *((struct vector3_struct*) &result_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_cam_getDerivedRight(struct hg3dclass_struct * thisclass_c, struct vector3_struct * result_c)
{
  Ogre::Camera * thisclass_cpp = static_cast<Ogre::Camera*> (getHG3DClassPtr(*thisclass_c, "Ogre::Camera"));
  Vector3 result_cpp;
  result_cpp = (thisclass_cpp->getDerivedRight());
  *result_c = *((struct vector3_struct*) &result_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_cam_getRealOrientation(struct hg3dclass_struct * thisclass_c, struct quaternion_struct * result_c)
{
  Ogre::Camera * thisclass_cpp = static_cast<Ogre::Camera*> (getHG3DClassPtr(*thisclass_c, "Ogre::Camera"));
  Quaternion result_cpp;
  result_cpp = (thisclass_cpp->getRealOrientation());
  *result_c = *((struct quaternion_struct*) &result_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_cam_getRealPosition(struct hg3dclass_struct * thisclass_c, struct vector3_struct * result_c)
{
  Ogre::Camera * thisclass_cpp = static_cast<Ogre::Camera*> (getHG3DClassPtr(*thisclass_c, "Ogre::Camera"));
  Vector3 result_cpp;
  result_cpp = (thisclass_cpp->getRealPosition());
  *result_c = *((struct vector3_struct*) &result_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_cam_getRealDirection(struct hg3dclass_struct * thisclass_c, struct vector3_struct * result_c)
{
  Ogre::Camera * thisclass_cpp = static_cast<Ogre::Camera*> (getHG3DClassPtr(*thisclass_c, "Ogre::Camera"));
  Vector3 result_cpp;
  result_cpp = (thisclass_cpp->getRealDirection());
  *result_c = *((struct vector3_struct*) &result_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_cam_getRealUp(struct hg3dclass_struct * thisclass_c, struct vector3_struct * result_c)
{
  Ogre::Camera * thisclass_cpp = static_cast<Ogre::Camera*> (getHG3DClassPtr(*thisclass_c, "Ogre::Camera"));
  Vector3 result_cpp;
  result_cpp = (thisclass_cpp->getRealUp());
  *result_c = *((struct vector3_struct*) &result_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_cam_getRealRight(struct hg3dclass_struct * thisclass_c, struct vector3_struct * result_c)
{
  Ogre::Camera * thisclass_cpp = static_cast<Ogre::Camera*> (getHG3DClassPtr(*thisclass_c, "Ogre::Camera"));
  Vector3 result_cpp;
  result_cpp = (thisclass_cpp->getRealRight());
  *result_c = *((struct vector3_struct*) &result_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_cam_getMovableType(struct hg3dclass_struct * thisclass_c, char * result_c)
{
  Ogre::Camera * thisclass_cpp = static_cast<Ogre::Camera*> (getHG3DClassPtr(*thisclass_c, "Ogre::Camera"));
  Ogre::String result_cpp;
  result_cpp = (thisclass_cpp->getMovableType());
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_cam_setLodBias(struct hg3dclass_struct * thisclass_c, float factor_c)
{
  Ogre::Camera * thisclass_cpp = static_cast<Ogre::Camera*> (getHG3DClassPtr(*thisclass_c, "Ogre::Camera"));
  Real factor_cpp = (Real)factor_c;
  (thisclass_cpp->setLodBias(factor_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_cam_getLodBias(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  Ogre::Camera * thisclass_cpp = static_cast<Ogre::Camera*> (getHG3DClassPtr(*thisclass_c, "Ogre::Camera"));
  Real result_cpp;
  result_cpp = (thisclass_cpp->getLodBias());
  *result_c = (float)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_cam_setLodCamera(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * lodCam_c)
{
  Ogre::Camera * thisclass_cpp = static_cast<Ogre::Camera*> (getHG3DClassPtr(*thisclass_c, "Ogre::Camera"));
  const Ogre::Camera * lodCam_cpp = static_cast<Ogre::Camera*> (getHG3DClassPtr(*lodCam_c, "Ogre::Camera"));
  (thisclass_cpp->setLodCamera(lodCam_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_cam_getLodCamera(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  Ogre::Camera * thisclass_cpp = static_cast<Ogre::Camera*> (getHG3DClassPtr(*thisclass_c, "Ogre::Camera"));
  const Ogre::Camera * result_cpp;
  result_cpp = (thisclass_cpp->getLodCamera());
  *result_c = getHG3DClass_Camera((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_cam_setWindow(struct hg3dclass_struct * thisclass_c, float Left_c, float Top_c, float Right_c, float Bottom_c)
{
  Ogre::Camera * thisclass_cpp = static_cast<Ogre::Camera*> (getHG3DClassPtr(*thisclass_c, "Ogre::Camera"));
  Real Left_cpp = (Real)Left_c;
  Real Top_cpp = (Real)Top_c;
  Real Right_cpp = (Real)Right_c;
  Real Bottom_cpp = (Real)Bottom_c;
  (thisclass_cpp->setWindow(Left_cpp, Top_cpp, Right_cpp, Bottom_cpp));
};

// Cancel view window. 
extern "C" Ogre_LIB_EXPORT void ogre_cam_resetWindow(struct hg3dclass_struct * thisclass_c)
{
  Ogre::Camera * thisclass_cpp = static_cast<Ogre::Camera*> (getHG3DClassPtr(*thisclass_c, "Ogre::Camera"));
  (thisclass_cpp->resetWindow());
};

// Returns if a viewport window is being used. 
extern "C" Ogre_LIB_EXPORT void ogre_cam_isWindowSet(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::Camera * thisclass_cpp = static_cast<Ogre::Camera*> (getHG3DClassPtr(*thisclass_c, "Ogre::Camera"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isWindowSet());
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_cam_getBoundingRadius(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  Ogre::Camera * thisclass_cpp = static_cast<Ogre::Camera*> (getHG3DClassPtr(*thisclass_c, "Ogre::Camera"));
  Real result_cpp;
  result_cpp = (thisclass_cpp->getBoundingRadius());
  *result_c = (float)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_cam_getAutoTrackTarget(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  Ogre::Camera * thisclass_cpp = static_cast<Ogre::Camera*> (getHG3DClassPtr(*thisclass_c, "Ogre::Camera"));
  Ogre::SceneNode * result_cpp;
  result_cpp = (thisclass_cpp->getAutoTrackTarget());
  *result_c = getHG3DClass_SceneNode((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_cam_getAutoTrackOffset(struct hg3dclass_struct * thisclass_c, struct vector3_struct * result_c)
{
  Ogre::Camera * thisclass_cpp = static_cast<Ogre::Camera*> (getHG3DClassPtr(*thisclass_c, "Ogre::Camera"));
  Vector3 result_cpp;
  result_cpp = (thisclass_cpp->getAutoTrackOffset());
  *result_c = *((struct vector3_struct*) &result_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_cam_getViewport(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  Ogre::Camera * thisclass_cpp = static_cast<Ogre::Camera*> (getHG3DClassPtr(*thisclass_c, "Ogre::Camera"));
  Ogre::Viewport * result_cpp;
  result_cpp = (thisclass_cpp->getViewport());
  *result_c = getHG3DClass_Viewport((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_cam_setAutoAspectRatio(struct hg3dclass_struct * thisclass_c, int autoratio_c)
{
  Ogre::Camera * thisclass_cpp = static_cast<Ogre::Camera*> (getHG3DClassPtr(*thisclass_c, "Ogre::Camera"));
  bool autoratio_cpp = (bool)autoratio_c;
  (thisclass_cpp->setAutoAspectRatio(autoratio_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_cam_getAutoAspectRatio(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::Camera * thisclass_cpp = static_cast<Ogre::Camera*> (getHG3DClassPtr(*thisclass_c, "Ogre::Camera"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->getAutoAspectRatio());
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_cam_setCullingFrustum(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * frustum_c)
{
  Ogre::Camera * thisclass_cpp = static_cast<Ogre::Camera*> (getHG3DClassPtr(*thisclass_c, "Ogre::Camera"));
  Ogre::Frustum * frustum_cpp = static_cast<Ogre::Frustum*> (getHG3DClassPtr(*frustum_c, "Ogre::Frustum"));
  (thisclass_cpp->setCullingFrustum(frustum_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_cam_getCullingFrustum(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  Ogre::Camera * thisclass_cpp = static_cast<Ogre::Camera*> (getHG3DClassPtr(*thisclass_c, "Ogre::Camera"));
  Ogre::Frustum * result_cpp;
  result_cpp = (thisclass_cpp->getCullingFrustum());
  *result_c = getHG3DClass_Frustum((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_cam_isVisible3(struct hg3dclass_struct * thisclass_c, struct vector3_struct * vert_c, enum EnumFrustumPlane * culledBy_c, int * result_c)
{
  Ogre::Camera * thisclass_cpp = static_cast<Ogre::Camera*> (getHG3DClassPtr(*thisclass_c, "Ogre::Camera"));
  Vector3 vert_cpp = *((Vector3*) vert_c);
  enum Ogre::FrustumPlane culledBy_cpp;
  bool result_cpp;
  result_cpp = (thisclass_cpp->isVisible(vert_cpp, &culledBy_cpp));
  *culledBy_c = (enum EnumFrustumPlane) culledBy_cpp;
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_cam_getWorldSpaceCorners(struct hg3dclass_struct * thisclass_c, struct vector3_struct * result_c)
{
  Ogre::Camera * thisclass_cpp = static_cast<Ogre::Camera*> (getHG3DClassPtr(*thisclass_c, "Ogre::Camera"));
  Vector3 result_cpp;
  result_cpp = *(thisclass_cpp->getWorldSpaceCorners());
  *result_c = *((struct vector3_struct*) &result_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_cam_getNearClipDistance(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  Ogre::Camera * thisclass_cpp = static_cast<Ogre::Camera*> (getHG3DClassPtr(*thisclass_c, "Ogre::Camera"));
  Real result_cpp;
  result_cpp = (thisclass_cpp->getNearClipDistance());
  *result_c = (float)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_cam_getFarClipDistance(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  Ogre::Camera * thisclass_cpp = static_cast<Ogre::Camera*> (getHG3DClassPtr(*thisclass_c, "Ogre::Camera"));
  Real result_cpp;
  result_cpp = (thisclass_cpp->getFarClipDistance());
  *result_c = (float)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_cam_setUseRenderingDistance(struct hg3dclass_struct * thisclass_c, int use_c)
{
  Ogre::Camera * thisclass_cpp = static_cast<Ogre::Camera*> (getHG3DClassPtr(*thisclass_c, "Ogre::Camera"));
  bool use_cpp = (bool)use_c;
  (thisclass_cpp->setUseRenderingDistance(use_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_cam_getUseRenderingDistance(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::Camera * thisclass_cpp = static_cast<Ogre::Camera*> (getHG3DClassPtr(*thisclass_c, "Ogre::Camera"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->getUseRenderingDistance());
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_cam_synchroniseBaseSettingsWith(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * cam_c)
{
  Ogre::Camera * thisclass_cpp = static_cast<Ogre::Camera*> (getHG3DClassPtr(*thisclass_c, "Ogre::Camera"));
  const Ogre::Camera * cam_cpp = static_cast<Ogre::Camera*> (getHG3DClassPtr(*cam_c, "Ogre::Camera"));
  (thisclass_cpp->synchroniseBaseSettingsWith(cam_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_cam_getPositionForViewUpdate(struct hg3dclass_struct * thisclass_c, struct vector3_struct * result_c)
{
  Ogre::Camera * thisclass_cpp = static_cast<Ogre::Camera*> (getHG3DClassPtr(*thisclass_c, "Ogre::Camera"));
  Vector3 result_cpp;
  result_cpp = (thisclass_cpp->getPositionForViewUpdate());
  *result_c = *((struct vector3_struct*) &result_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_cam_getOrientationForViewUpdate(struct hg3dclass_struct * thisclass_c, struct quaternion_struct * result_c)
{
  Ogre::Camera * thisclass_cpp = static_cast<Ogre::Camera*> (getHG3DClassPtr(*thisclass_c, "Ogre::Camera"));
  Quaternion result_cpp;
  result_cpp = (thisclass_cpp->getOrientationForViewUpdate());
  *result_c = *((struct quaternion_struct*) &result_cpp);
};

// Sets whether to use min display size calculations 

extern "C" Ogre_LIB_EXPORT void ogre_cam_setUseMinPixelSize(struct hg3dclass_struct * thisclass_c, int enable_c)
{
  Ogre::Camera * thisclass_cpp = static_cast<Ogre::Camera*> (getHG3DClassPtr(*thisclass_c, "Ogre::Camera"));
  bool enable_cpp = (bool)enable_c;
  (thisclass_cpp->setUseMinPixelSize(enable_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_cam_getUseMinPixelSize(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::Camera * thisclass_cpp = static_cast<Ogre::Camera*> (getHG3DClassPtr(*thisclass_c, "Ogre::Camera"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->getUseMinPixelSize());
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_cam_getPixelDisplayRatio(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  Ogre::Camera * thisclass_cpp = static_cast<Ogre::Camera*> (getHG3DClassPtr(*thisclass_c, "Ogre::Camera"));
  Real result_cpp;
  result_cpp = (thisclass_cpp->getPixelDisplayRatio());
  *result_c = (float)result_cpp;
};

