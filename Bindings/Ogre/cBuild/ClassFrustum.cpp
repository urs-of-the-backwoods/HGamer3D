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

// ClassFrustum.cpp

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
	#include "StructRadians.h"
#include "StructVec2.h"
#include "StructVec3.h"
#include "EnumFrustumPlane.h"
#include "StructSharedPtr.h"
#include "EnumProjectionType.h"
#include "StructQuaternion.h"
#include "EnumOrientationMode.h"
#include "./Ogre.h"
#include "./OgreString.h"
#include "./WindowUtilsHG3D.h"
#include "HG3DUtilities.h"

using namespace Ogre;
using Ogre::uint;



// Named constructor. 
extern "C" Ogre_LIB_EXPORT void ogre_frst_construct(char * name_c, struct hg3dclass_struct * result_c)
{
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  Ogre::Frustum * result_cpp;
  result_cpp = (new Ogre::Frustum(name_cpp));
  *result_c = getHG3DClass_Frustum((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_frst_destruct(struct hg3dclass_struct * thisclass_c)
{
  Ogre::Frustum * thisclass_cpp = static_cast<Ogre::Frustum*> (getHG3DClassPtr(*thisclass_c, "Ogre::Frustum"));
  (delete thisclass_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_frst_setFOVy(struct hg3dclass_struct * thisclass_c, struct radian_struct * fovy_c)
{
  Ogre::Frustum * thisclass_cpp = static_cast<Ogre::Frustum*> (getHG3DClassPtr(*thisclass_c, "Ogre::Frustum"));
  Radian fovy_cpp = *((Radian*) fovy_c);
  (thisclass_cpp->setFOVy(fovy_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_frst_getFOVy(struct hg3dclass_struct * thisclass_c, struct radian_struct * result_c)
{
  Ogre::Frustum * thisclass_cpp = static_cast<Ogre::Frustum*> (getHG3DClassPtr(*thisclass_c, "Ogre::Frustum"));
  Radian result_cpp;
  result_cpp = (thisclass_cpp->getFOVy());
  *result_c = *((struct radian_struct*) &result_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_frst_setNearClipDistance(struct hg3dclass_struct * thisclass_c, float nearDist_c)
{
  Ogre::Frustum * thisclass_cpp = static_cast<Ogre::Frustum*> (getHG3DClassPtr(*thisclass_c, "Ogre::Frustum"));
  Real nearDist_cpp = (Real)nearDist_c;
  (thisclass_cpp->setNearClipDistance(nearDist_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_frst_getNearClipDistance(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  Ogre::Frustum * thisclass_cpp = static_cast<Ogre::Frustum*> (getHG3DClassPtr(*thisclass_c, "Ogre::Frustum"));
  Real result_cpp;
  result_cpp = (thisclass_cpp->getNearClipDistance());
  *result_c = (float)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_frst_setFarClipDistance(struct hg3dclass_struct * thisclass_c, float farDist_c)
{
  Ogre::Frustum * thisclass_cpp = static_cast<Ogre::Frustum*> (getHG3DClassPtr(*thisclass_c, "Ogre::Frustum"));
  Real farDist_cpp = (Real)farDist_c;
  (thisclass_cpp->setFarClipDistance(farDist_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_frst_getFarClipDistance(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  Ogre::Frustum * thisclass_cpp = static_cast<Ogre::Frustum*> (getHG3DClassPtr(*thisclass_c, "Ogre::Frustum"));
  Real result_cpp;
  result_cpp = (thisclass_cpp->getFarClipDistance());
  *result_c = (float)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_frst_setAspectRatio(struct hg3dclass_struct * thisclass_c, float ratio_c)
{
  Ogre::Frustum * thisclass_cpp = static_cast<Ogre::Frustum*> (getHG3DClassPtr(*thisclass_c, "Ogre::Frustum"));
  Real ratio_cpp = (Real)ratio_c;
  (thisclass_cpp->setAspectRatio(ratio_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_frst_getAspectRatio(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  Ogre::Frustum * thisclass_cpp = static_cast<Ogre::Frustum*> (getHG3DClassPtr(*thisclass_c, "Ogre::Frustum"));
  Real result_cpp;
  result_cpp = (thisclass_cpp->getAspectRatio());
  *result_c = (float)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_frst_setFrustumOffset(struct hg3dclass_struct * thisclass_c, struct vector2_struct * offset_c)
{
  Ogre::Frustum * thisclass_cpp = static_cast<Ogre::Frustum*> (getHG3DClassPtr(*thisclass_c, "Ogre::Frustum"));
  Vector2 offset_cpp = *((Vector2*) offset_c);
  (thisclass_cpp->setFrustumOffset(offset_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_frst_setFrustumOffset2(struct hg3dclass_struct * thisclass_c, float horizontal_c, float vertical_c)
{
  Ogre::Frustum * thisclass_cpp = static_cast<Ogre::Frustum*> (getHG3DClassPtr(*thisclass_c, "Ogre::Frustum"));
  Real horizontal_cpp = (Real)horizontal_c;
  Real vertical_cpp = (Real)vertical_c;
  (thisclass_cpp->setFrustumOffset(horizontal_cpp, vertical_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_frst_getFrustumOffset(struct hg3dclass_struct * thisclass_c, struct vector2_struct * result_c)
{
  Ogre::Frustum * thisclass_cpp = static_cast<Ogre::Frustum*> (getHG3DClassPtr(*thisclass_c, "Ogre::Frustum"));
  Vector2 result_cpp;
  result_cpp = (thisclass_cpp->getFrustumOffset());
  *result_c = *((struct vector2_struct*) &result_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_frst_setFocalLength(struct hg3dclass_struct * thisclass_c, float focalLength_c)
{
  Ogre::Frustum * thisclass_cpp = static_cast<Ogre::Frustum*> (getHG3DClassPtr(*thisclass_c, "Ogre::Frustum"));
  Real focalLength_cpp = (Real)focalLength_c;
  (thisclass_cpp->setFocalLength(focalLength_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_frst_getFocalLength(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  Ogre::Frustum * thisclass_cpp = static_cast<Ogre::Frustum*> (getHG3DClassPtr(*thisclass_c, "Ogre::Frustum"));
  Real result_cpp;
  result_cpp = (thisclass_cpp->getFocalLength());
  *result_c = (float)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_frst_setFrustumExtents(struct hg3dclass_struct * thisclass_c, float left_c, float right_c, float top_c, float bottom_c)
{
  Ogre::Frustum * thisclass_cpp = static_cast<Ogre::Frustum*> (getHG3DClassPtr(*thisclass_c, "Ogre::Frustum"));
  Real left_cpp = (Real)left_c;
  Real right_cpp = (Real)right_c;
  Real top_cpp = (Real)top_c;
  Real bottom_cpp = (Real)bottom_c;
  (thisclass_cpp->setFrustumExtents(left_cpp, right_cpp, top_cpp, bottom_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_frst_resetFrustumExtents(struct hg3dclass_struct * thisclass_c)
{
  Ogre::Frustum * thisclass_cpp = static_cast<Ogre::Frustum*> (getHG3DClassPtr(*thisclass_c, "Ogre::Frustum"));
  (thisclass_cpp->resetFrustumExtents());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_frst_getFrustumExtents(struct hg3dclass_struct * thisclass_c, float * outleft_c, float * outright_c, float * outtop_c, float * outbottom_c)
{
  Ogre::Frustum * thisclass_cpp = static_cast<Ogre::Frustum*> (getHG3DClassPtr(*thisclass_c, "Ogre::Frustum"));
  Real outleft_cpp;
  Real outright_cpp;
  Real outtop_cpp;
  Real outbottom_cpp;
  (thisclass_cpp->getFrustumExtents(outleft_cpp, outright_cpp, outtop_cpp, outbottom_cpp));
  *outleft_c = (float)outleft_cpp;
  *outright_c = (float)outright_cpp;
  *outtop_c = (float)outtop_cpp;
  *outbottom_c = (float)outbottom_cpp;
};

// Returns whether a custom view matrix is in use. 
extern "C" Ogre_LIB_EXPORT void ogre_frst_isCustomViewMatrixEnabled(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::Frustum * thisclass_cpp = static_cast<Ogre::Frustum*> (getHG3DClassPtr(*thisclass_c, "Ogre::Frustum"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isCustomViewMatrixEnabled());
  *result_c = (long)result_cpp;
};

// Returns whether a custom projection matrix is in use. 
extern "C" Ogre_LIB_EXPORT void ogre_frst_isCustomProjectionMatrixEnabled(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::Frustum * thisclass_cpp = static_cast<Ogre::Frustum*> (getHG3DClassPtr(*thisclass_c, "Ogre::Frustum"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isCustomProjectionMatrixEnabled());
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_frst_isVisible3(struct hg3dclass_struct * thisclass_c, struct vector3_struct * vert_c, enum EnumFrustumPlane * culledBy_c, long * result_c)
{
  Ogre::Frustum * thisclass_cpp = static_cast<Ogre::Frustum*> (getHG3DClassPtr(*thisclass_c, "Ogre::Frustum"));
  Vector3 vert_cpp = *((Vector3*) vert_c);
  enum Ogre::FrustumPlane culledBy_cpp;
  bool result_cpp;
  result_cpp = (thisclass_cpp->isVisible(vert_cpp, &culledBy_cpp));
  *culledBy_c = (enum EnumFrustumPlane) culledBy_cpp;
  *result_c = (long)result_cpp;
};

// Overridden from MovableObject::getTypeFlags
extern "C" Ogre_LIB_EXPORT void ogre_frst_getTypeFlags(struct hg3dclass_struct * thisclass_c, unsigned long * result_c)
{
  Ogre::Frustum * thisclass_cpp = static_cast<Ogre::Frustum*> (getHG3DClassPtr(*thisclass_c, "Ogre::Frustum"));
  uint32 result_cpp;
  result_cpp = (thisclass_cpp->getTypeFlags());
  *result_c = (unsigned long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_frst_getBoundingRadius(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  Ogre::Frustum * thisclass_cpp = static_cast<Ogre::Frustum*> (getHG3DClassPtr(*thisclass_c, "Ogre::Frustum"));
  Real result_cpp;
  result_cpp = (thisclass_cpp->getBoundingRadius());
  *result_c = (float)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_frst_getMovableType(struct hg3dclass_struct * thisclass_c, char * result_c)
{
  Ogre::Frustum * thisclass_cpp = static_cast<Ogre::Frustum*> (getHG3DClassPtr(*thisclass_c, "Ogre::Frustum"));
  Ogre::String result_cpp;
  result_cpp = (thisclass_cpp->getMovableType());
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_frst_getMaterial(struct hg3dclass_struct * thisclass_c, struct sharedptr_struct * result_c)
{
  Ogre::Frustum * thisclass_cpp = static_cast<Ogre::Frustum*> (getHG3DClassPtr(*thisclass_c, "Ogre::Frustum"));
  MaterialPtr result_cpp;
  result_cpp = (thisclass_cpp->getMaterial());
  *result_c = *((struct sharedptr_struct*) &result_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_frst_getSquaredViewDepth(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * cam_c, float * result_c)
{
  Ogre::Frustum * thisclass_cpp = static_cast<Ogre::Frustum*> (getHG3DClassPtr(*thisclass_c, "Ogre::Frustum"));
  const Ogre::Camera * cam_cpp = static_cast<Ogre::Camera*> (getHG3DClassPtr(*cam_c, "Ogre::Camera"));
  Real result_cpp;
  result_cpp = (thisclass_cpp->getSquaredViewDepth(cam_cpp));
  *result_c = (float)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_frst_getWorldSpaceCorners(struct hg3dclass_struct * thisclass_c, struct vector3_struct * result_c)
{
  Ogre::Frustum * thisclass_cpp = static_cast<Ogre::Frustum*> (getHG3DClassPtr(*thisclass_c, "Ogre::Frustum"));
  Vector3 result_cpp;
  result_cpp = *(thisclass_cpp->getWorldSpaceCorners());
  *result_c = *((struct vector3_struct*) &result_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_frst_setProjectionType(struct hg3dclass_struct * thisclass_c, enum EnumProjectionType pt_c)
{
  Ogre::Frustum * thisclass_cpp = static_cast<Ogre::Frustum*> (getHG3DClassPtr(*thisclass_c, "Ogre::Frustum"));
  enum Ogre::ProjectionType pt_cpp = (enum Ogre::ProjectionType)pt_c;
  (thisclass_cpp->setProjectionType(pt_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_frst_getProjectionType(struct hg3dclass_struct * thisclass_c, enum EnumProjectionType * result_c)
{
  Ogre::Frustum * thisclass_cpp = static_cast<Ogre::Frustum*> (getHG3DClassPtr(*thisclass_c, "Ogre::Frustum"));
  enum Ogre::ProjectionType result_cpp;
  result_cpp = (thisclass_cpp->getProjectionType());
  *result_c = (enum EnumProjectionType) result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_frst_setOrthoWindow(struct hg3dclass_struct * thisclass_c, float w_c, float h_c)
{
  Ogre::Frustum * thisclass_cpp = static_cast<Ogre::Frustum*> (getHG3DClassPtr(*thisclass_c, "Ogre::Frustum"));
  Real w_cpp = (Real)w_c;
  Real h_cpp = (Real)h_c;
  (thisclass_cpp->setOrthoWindow(w_cpp, h_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_frst_setOrthoWindowHeight(struct hg3dclass_struct * thisclass_c, float h_c)
{
  Ogre::Frustum * thisclass_cpp = static_cast<Ogre::Frustum*> (getHG3DClassPtr(*thisclass_c, "Ogre::Frustum"));
  Real h_cpp = (Real)h_c;
  (thisclass_cpp->setOrthoWindowHeight(h_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_frst_setOrthoWindowWidth(struct hg3dclass_struct * thisclass_c, float w_c)
{
  Ogre::Frustum * thisclass_cpp = static_cast<Ogre::Frustum*> (getHG3DClassPtr(*thisclass_c, "Ogre::Frustum"));
  Real w_cpp = (Real)w_c;
  (thisclass_cpp->setOrthoWindowWidth(w_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_frst_getOrthoWindowHeight(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  Ogre::Frustum * thisclass_cpp = static_cast<Ogre::Frustum*> (getHG3DClassPtr(*thisclass_c, "Ogre::Frustum"));
  Real result_cpp;
  result_cpp = (thisclass_cpp->getOrthoWindowHeight());
  *result_c = (float)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_frst_getOrthoWindowWidth(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  Ogre::Frustum * thisclass_cpp = static_cast<Ogre::Frustum*> (getHG3DClassPtr(*thisclass_c, "Ogre::Frustum"));
  Real result_cpp;
  result_cpp = (thisclass_cpp->getOrthoWindowWidth());
  *result_c = (float)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_frst_disableReflection(struct hg3dclass_struct * thisclass_c)
{
  Ogre::Frustum * thisclass_cpp = static_cast<Ogre::Frustum*> (getHG3DClassPtr(*thisclass_c, "Ogre::Frustum"));
  (thisclass_cpp->disableReflection());
};

// Returns whether this frustum is being reflected. 
extern "C" Ogre_LIB_EXPORT void ogre_frst_isReflected(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::Frustum * thisclass_cpp = static_cast<Ogre::Frustum*> (getHG3DClassPtr(*thisclass_c, "Ogre::Frustum"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isReflected());
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_frst_disableCustomNearClipPlane(struct hg3dclass_struct * thisclass_c)
{
  Ogre::Frustum * thisclass_cpp = static_cast<Ogre::Frustum*> (getHG3DClassPtr(*thisclass_c, "Ogre::Frustum"));
  (thisclass_cpp->disableCustomNearClipPlane());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_frst_isCustomNearClipPlaneEnabled(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::Frustum * thisclass_cpp = static_cast<Ogre::Frustum*> (getHG3DClassPtr(*thisclass_c, "Ogre::Frustum"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isCustomNearClipPlaneEnabled());
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_frst_getPositionForViewUpdate(struct hg3dclass_struct * thisclass_c, struct vector3_struct * result_c)
{
  Ogre::Frustum * thisclass_cpp = static_cast<Ogre::Frustum*> (getHG3DClassPtr(*thisclass_c, "Ogre::Frustum"));
  Vector3 result_cpp;
  result_cpp = (thisclass_cpp->getPositionForViewUpdate());
  *result_c = *((struct vector3_struct*) &result_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_frst_getOrientationForViewUpdate(struct hg3dclass_struct * thisclass_c, struct quaternion_struct * result_c)
{
  Ogre::Frustum * thisclass_cpp = static_cast<Ogre::Frustum*> (getHG3DClassPtr(*thisclass_c, "Ogre::Frustum"));
  Quaternion result_cpp;
  result_cpp = (thisclass_cpp->getOrientationForViewUpdate());
  *result_c = *((struct quaternion_struct*) &result_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_frst_setOrientationMode(struct hg3dclass_struct * thisclass_c, enum EnumOrientationMode orientationMode_c)
{
  Ogre::Frustum * thisclass_cpp = static_cast<Ogre::Frustum*> (getHG3DClassPtr(*thisclass_c, "Ogre::Frustum"));
  enum Ogre::OrientationMode orientationMode_cpp = (enum Ogre::OrientationMode)orientationMode_c;
  (thisclass_cpp->setOrientationMode(orientationMode_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_frst_getOrientationMode(struct hg3dclass_struct * thisclass_c, enum EnumOrientationMode * result_c)
{
  Ogre::Frustum * thisclass_cpp = static_cast<Ogre::Frustum*> (getHG3DClassPtr(*thisclass_c, "Ogre::Frustum"));
  enum Ogre::OrientationMode result_cpp;
  result_cpp = (thisclass_cpp->getOrientationMode());
  *result_c = (enum EnumOrientationMode) result_cpp;
};

