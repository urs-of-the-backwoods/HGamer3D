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

// ClassLight.cpp

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
	#include "EnumLightType.h"
#include "StructColour.h"
#include "StructVec3.h"
#include "StructRadians.h"
#include "./Ogre.h"
#include "./OgreString.h"
#include "./WindowUtilsHG3D.h"
#include "HG3DUtilities.h"

using namespace Ogre;



// 
extern "C" Ogre_LIB_EXPORT void ogre_lgt_construct(char * name_c, struct hg3dclass_struct * result_c)
{
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  Ogre::Light * result_cpp;
  result_cpp = (new Ogre::Light(name_cpp));
  *result_c = getHG3DClass_Light((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_lgt_destruct(struct hg3dclass_struct * thisclass_c)
{
  Ogre::Light * thisclass_cpp = static_cast<Ogre::Light*> (getHG3DClassPtr(*thisclass_c, "Ogre::Light"));
  (delete thisclass_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_lgt_setType(struct hg3dclass_struct * thisclass_c, enum EnumLightType type_c)
{
  Ogre::Light * thisclass_cpp = static_cast<Ogre::Light*> (getHG3DClassPtr(*thisclass_c, "Ogre::Light"));
  enum Ogre::Light::LightTypes type_cpp = (enum Ogre::Light::LightTypes)type_c;
  (thisclass_cpp->setType(type_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_lgt_getType(struct hg3dclass_struct * thisclass_c, enum EnumLightType * result_c)
{
  Ogre::Light * thisclass_cpp = static_cast<Ogre::Light*> (getHG3DClassPtr(*thisclass_c, "Ogre::Light"));
  enum Ogre::Light::LightTypes result_cpp;
  result_cpp = (thisclass_cpp->getType());
  *result_c = (enum EnumLightType) result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_lgt_setDiffuseColour(struct hg3dclass_struct * thisclass_c, float red_c, float green_c, float blue_c)
{
  Ogre::Light * thisclass_cpp = static_cast<Ogre::Light*> (getHG3DClassPtr(*thisclass_c, "Ogre::Light"));
  Real red_cpp = (Real)red_c;
  Real green_cpp = (Real)green_c;
  Real blue_cpp = (Real)blue_c;
  (thisclass_cpp->setDiffuseColour(red_cpp, green_cpp, blue_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_lgt_setDiffuseColour2(struct hg3dclass_struct * thisclass_c, struct colourvalue_struct * colour_c)
{
  Ogre::Light * thisclass_cpp = static_cast<Ogre::Light*> (getHG3DClassPtr(*thisclass_c, "Ogre::Light"));
  ColourValue colour_cpp = *((ColourValue*) colour_c);
  (thisclass_cpp->setDiffuseColour(colour_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_lgt_getDiffuseColour(struct hg3dclass_struct * thisclass_c, struct colourvalue_struct * result_c)
{
  Ogre::Light * thisclass_cpp = static_cast<Ogre::Light*> (getHG3DClassPtr(*thisclass_c, "Ogre::Light"));
  ColourValue result_cpp;
  result_cpp = (thisclass_cpp->getDiffuseColour());
  *result_c = *((struct colourvalue_struct*) &result_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_lgt_setSpecularColour(struct hg3dclass_struct * thisclass_c, float red_c, float green_c, float blue_c)
{
  Ogre::Light * thisclass_cpp = static_cast<Ogre::Light*> (getHG3DClassPtr(*thisclass_c, "Ogre::Light"));
  Real red_cpp = (Real)red_c;
  Real green_cpp = (Real)green_c;
  Real blue_cpp = (Real)blue_c;
  (thisclass_cpp->setSpecularColour(red_cpp, green_cpp, blue_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_lgt_setSpecularColour2(struct hg3dclass_struct * thisclass_c, struct colourvalue_struct * colour_c)
{
  Ogre::Light * thisclass_cpp = static_cast<Ogre::Light*> (getHG3DClassPtr(*thisclass_c, "Ogre::Light"));
  ColourValue colour_cpp = *((ColourValue*) colour_c);
  (thisclass_cpp->setSpecularColour(colour_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_lgt_getSpecularColour(struct hg3dclass_struct * thisclass_c, struct colourvalue_struct * result_c)
{
  Ogre::Light * thisclass_cpp = static_cast<Ogre::Light*> (getHG3DClassPtr(*thisclass_c, "Ogre::Light"));
  ColourValue result_cpp;
  result_cpp = (thisclass_cpp->getSpecularColour());
  *result_c = *((struct colourvalue_struct*) &result_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_lgt_setAttenuation(struct hg3dclass_struct * thisclass_c, float range_c, float constant_c, float linear_c, float quadratic_c)
{
  Ogre::Light * thisclass_cpp = static_cast<Ogre::Light*> (getHG3DClassPtr(*thisclass_c, "Ogre::Light"));
  Real range_cpp = (Real)range_c;
  Real constant_cpp = (Real)constant_c;
  Real linear_cpp = (Real)linear_c;
  Real quadratic_cpp = (Real)quadratic_c;
  (thisclass_cpp->setAttenuation(range_cpp, constant_cpp, linear_cpp, quadratic_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_lgt_getAttenuationRange(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  Ogre::Light * thisclass_cpp = static_cast<Ogre::Light*> (getHG3DClassPtr(*thisclass_c, "Ogre::Light"));
  Real result_cpp;
  result_cpp = (thisclass_cpp->getAttenuationRange());
  *result_c = (float)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_lgt_getAttenuationConstant(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  Ogre::Light * thisclass_cpp = static_cast<Ogre::Light*> (getHG3DClassPtr(*thisclass_c, "Ogre::Light"));
  Real result_cpp;
  result_cpp = (thisclass_cpp->getAttenuationConstant());
  *result_c = (float)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_lgt_getAttenuationLinear(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  Ogre::Light * thisclass_cpp = static_cast<Ogre::Light*> (getHG3DClassPtr(*thisclass_c, "Ogre::Light"));
  Real result_cpp;
  result_cpp = (thisclass_cpp->getAttenuationLinear());
  *result_c = (float)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_lgt_getAttenuationQuadric(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  Ogre::Light * thisclass_cpp = static_cast<Ogre::Light*> (getHG3DClassPtr(*thisclass_c, "Ogre::Light"));
  Real result_cpp;
  result_cpp = (thisclass_cpp->getAttenuationQuadric());
  *result_c = (float)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_lgt_setPosition(struct hg3dclass_struct * thisclass_c, float x_c, float y_c, float z_c)
{
  Ogre::Light * thisclass_cpp = static_cast<Ogre::Light*> (getHG3DClassPtr(*thisclass_c, "Ogre::Light"));
  Real x_cpp = (Real)x_c;
  Real y_cpp = (Real)y_c;
  Real z_cpp = (Real)z_c;
  (thisclass_cpp->setPosition(x_cpp, y_cpp, z_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_lgt_setPosition2(struct hg3dclass_struct * thisclass_c, struct vector3_struct * vec_c)
{
  Ogre::Light * thisclass_cpp = static_cast<Ogre::Light*> (getHG3DClassPtr(*thisclass_c, "Ogre::Light"));
  Vector3 vec_cpp = *((Vector3*) vec_c);
  (thisclass_cpp->setPosition(vec_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_lgt_getPosition(struct hg3dclass_struct * thisclass_c, struct vector3_struct * result_c)
{
  Ogre::Light * thisclass_cpp = static_cast<Ogre::Light*> (getHG3DClassPtr(*thisclass_c, "Ogre::Light"));
  Vector3 result_cpp;
  result_cpp = (thisclass_cpp->getPosition());
  *result_c = *((struct vector3_struct*) &result_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_lgt_setDirection(struct hg3dclass_struct * thisclass_c, float x_c, float y_c, float z_c)
{
  Ogre::Light * thisclass_cpp = static_cast<Ogre::Light*> (getHG3DClassPtr(*thisclass_c, "Ogre::Light"));
  Real x_cpp = (Real)x_c;
  Real y_cpp = (Real)y_c;
  Real z_cpp = (Real)z_c;
  (thisclass_cpp->setDirection(x_cpp, y_cpp, z_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_lgt_setDirection2(struct hg3dclass_struct * thisclass_c, struct vector3_struct * vec_c)
{
  Ogre::Light * thisclass_cpp = static_cast<Ogre::Light*> (getHG3DClassPtr(*thisclass_c, "Ogre::Light"));
  Vector3 vec_cpp = *((Vector3*) vec_c);
  (thisclass_cpp->setDirection(vec_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_lgt_getDirection(struct hg3dclass_struct * thisclass_c, struct vector3_struct * result_c)
{
  Ogre::Light * thisclass_cpp = static_cast<Ogre::Light*> (getHG3DClassPtr(*thisclass_c, "Ogre::Light"));
  Vector3 result_cpp;
  result_cpp = (thisclass_cpp->getDirection());
  *result_c = *((struct vector3_struct*) &result_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_lgt_setSpotlightRange(struct hg3dclass_struct * thisclass_c, struct radian_struct * innerAngle_c, struct radian_struct * outerAngle_c, float falloff_c)
{
  Ogre::Light * thisclass_cpp = static_cast<Ogre::Light*> (getHG3DClassPtr(*thisclass_c, "Ogre::Light"));
  Radian innerAngle_cpp = *((Radian*) innerAngle_c);
  Radian outerAngle_cpp = *((Radian*) outerAngle_c);
  Real falloff_cpp = (Real)falloff_c;
  (thisclass_cpp->setSpotlightRange(innerAngle_cpp, outerAngle_cpp, falloff_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_lgt_getSpotlightInnerAngle(struct hg3dclass_struct * thisclass_c, struct radian_struct * result_c)
{
  Ogre::Light * thisclass_cpp = static_cast<Ogre::Light*> (getHG3DClassPtr(*thisclass_c, "Ogre::Light"));
  Radian result_cpp;
  result_cpp = (thisclass_cpp->getSpotlightInnerAngle());
  *result_c = *((struct radian_struct*) &result_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_lgt_getSpotlightOuterAngle(struct hg3dclass_struct * thisclass_c, struct radian_struct * result_c)
{
  Ogre::Light * thisclass_cpp = static_cast<Ogre::Light*> (getHG3DClassPtr(*thisclass_c, "Ogre::Light"));
  Radian result_cpp;
  result_cpp = (thisclass_cpp->getSpotlightOuterAngle());
  *result_c = *((struct radian_struct*) &result_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_lgt_getSpotlightFalloff(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  Ogre::Light * thisclass_cpp = static_cast<Ogre::Light*> (getHG3DClassPtr(*thisclass_c, "Ogre::Light"));
  Real result_cpp;
  result_cpp = (thisclass_cpp->getSpotlightFalloff());
  *result_c = (float)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_lgt_setSpotlightInnerAngle(struct hg3dclass_struct * thisclass_c, struct radian_struct * val_c)
{
  Ogre::Light * thisclass_cpp = static_cast<Ogre::Light*> (getHG3DClassPtr(*thisclass_c, "Ogre::Light"));
  Radian val_cpp = *((Radian*) val_c);
  (thisclass_cpp->setSpotlightInnerAngle(val_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_lgt_setSpotlightOuterAngle(struct hg3dclass_struct * thisclass_c, struct radian_struct * val_c)
{
  Ogre::Light * thisclass_cpp = static_cast<Ogre::Light*> (getHG3DClassPtr(*thisclass_c, "Ogre::Light"));
  Radian val_cpp = *((Radian*) val_c);
  (thisclass_cpp->setSpotlightOuterAngle(val_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_lgt_setSpotlightFalloff(struct hg3dclass_struct * thisclass_c, float val_c)
{
  Ogre::Light * thisclass_cpp = static_cast<Ogre::Light*> (getHG3DClassPtr(*thisclass_c, "Ogre::Light"));
  Real val_cpp = (Real)val_c;
  (thisclass_cpp->setSpotlightFalloff(val_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_lgt_setSpotlightNearClipDistance(struct hg3dclass_struct * thisclass_c, float nearClip_c)
{
  Ogre::Light * thisclass_cpp = static_cast<Ogre::Light*> (getHG3DClassPtr(*thisclass_c, "Ogre::Light"));
  Real nearClip_cpp = (Real)nearClip_c;
  (thisclass_cpp->setSpotlightNearClipDistance(nearClip_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_lgt_getSpotlightNearClipDistance(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  Ogre::Light * thisclass_cpp = static_cast<Ogre::Light*> (getHG3DClassPtr(*thisclass_c, "Ogre::Light"));
  Real result_cpp;
  result_cpp = (thisclass_cpp->getSpotlightNearClipDistance());
  *result_c = (float)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_lgt_setPowerScale(struct hg3dclass_struct * thisclass_c, float power_c)
{
  Ogre::Light * thisclass_cpp = static_cast<Ogre::Light*> (getHG3DClassPtr(*thisclass_c, "Ogre::Light"));
  Real power_cpp = (Real)power_c;
  (thisclass_cpp->setPowerScale(power_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_lgt_getPowerScale(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  Ogre::Light * thisclass_cpp = static_cast<Ogre::Light*> (getHG3DClassPtr(*thisclass_c, "Ogre::Light"));
  Real result_cpp;
  result_cpp = (thisclass_cpp->getPowerScale());
  *result_c = (float)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_lgt_getMovableType(struct hg3dclass_struct * thisclass_c, char * result_c)
{
  Ogre::Light * thisclass_cpp = static_cast<Ogre::Light*> (getHG3DClassPtr(*thisclass_c, "Ogre::Light"));
  Ogre::String result_cpp;
  result_cpp = (thisclass_cpp->getMovableType());
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_lgt_getDerivedPosition(struct hg3dclass_struct * thisclass_c, int cameraRelativeIfSet_c, struct vector3_struct * result_c)
{
  Ogre::Light * thisclass_cpp = static_cast<Ogre::Light*> (getHG3DClassPtr(*thisclass_c, "Ogre::Light"));
  bool cameraRelativeIfSet_cpp = (bool)cameraRelativeIfSet_c;
  Vector3 result_cpp;
  result_cpp = (thisclass_cpp->getDerivedPosition(cameraRelativeIfSet_cpp));
  *result_c = *((struct vector3_struct*) &result_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_lgt_getDerivedDirection(struct hg3dclass_struct * thisclass_c, struct vector3_struct * result_c)
{
  Ogre::Light * thisclass_cpp = static_cast<Ogre::Light*> (getHG3DClassPtr(*thisclass_c, "Ogre::Light"));
  Vector3 result_cpp;
  result_cpp = (thisclass_cpp->getDerivedDirection());
  *result_c = *((struct vector3_struct*) &result_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_lgt_setVisible(struct hg3dclass_struct * thisclass_c, int visible_c)
{
  Ogre::Light * thisclass_cpp = static_cast<Ogre::Light*> (getHG3DClassPtr(*thisclass_c, "Ogre::Light"));
  bool visible_cpp = (bool)visible_c;
  (thisclass_cpp->setVisible(visible_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_lgt_getBoundingRadius(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  Ogre::Light * thisclass_cpp = static_cast<Ogre::Light*> (getHG3DClassPtr(*thisclass_c, "Ogre::Light"));
  Real result_cpp;
  result_cpp = (thisclass_cpp->getBoundingRadius());
  *result_c = (float)result_cpp;
};

// Override to return specific type flag. 
extern "C" Ogre_LIB_EXPORT void ogre_lgt_getTypeFlags(struct hg3dclass_struct * thisclass_c, unsigned int * result_c)
{
  Ogre::Light * thisclass_cpp = static_cast<Ogre::Light*> (getHG3DClassPtr(*thisclass_c, "Ogre::Light"));
  uint32 result_cpp;
  result_cpp = (thisclass_cpp->getTypeFlags());
  *result_c = (unsigned int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_lgt_resetCustomShadowCameraSetup(struct hg3dclass_struct * thisclass_c)
{
  Ogre::Light * thisclass_cpp = static_cast<Ogre::Light*> (getHG3DClassPtr(*thisclass_c, "Ogre::Light"));
  (thisclass_cpp->resetCustomShadowCameraSetup());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_lgt_setShadowFarDistance(struct hg3dclass_struct * thisclass_c, float distance_c)
{
  Ogre::Light * thisclass_cpp = static_cast<Ogre::Light*> (getHG3DClassPtr(*thisclass_c, "Ogre::Light"));
  Real distance_cpp = (Real)distance_c;
  (thisclass_cpp->setShadowFarDistance(distance_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_lgt_resetShadowFarDistance(struct hg3dclass_struct * thisclass_c)
{
  Ogre::Light * thisclass_cpp = static_cast<Ogre::Light*> (getHG3DClassPtr(*thisclass_c, "Ogre::Light"));
  (thisclass_cpp->resetShadowFarDistance());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_lgt_getShadowFarDistance(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  Ogre::Light * thisclass_cpp = static_cast<Ogre::Light*> (getHG3DClassPtr(*thisclass_c, "Ogre::Light"));
  Real result_cpp;
  result_cpp = (thisclass_cpp->getShadowFarDistance());
  *result_c = (float)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_lgt_getShadowFarDistanceSquared(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  Ogre::Light * thisclass_cpp = static_cast<Ogre::Light*> (getHG3DClassPtr(*thisclass_c, "Ogre::Light"));
  Real result_cpp;
  result_cpp = (thisclass_cpp->getShadowFarDistanceSquared());
  *result_c = (float)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_lgt_setShadowNearClipDistance(struct hg3dclass_struct * thisclass_c, float nearClip_c)
{
  Ogre::Light * thisclass_cpp = static_cast<Ogre::Light*> (getHG3DClassPtr(*thisclass_c, "Ogre::Light"));
  Real nearClip_cpp = (Real)nearClip_c;
  (thisclass_cpp->setShadowNearClipDistance(nearClip_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_lgt_getShadowNearClipDistance(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  Ogre::Light * thisclass_cpp = static_cast<Ogre::Light*> (getHG3DClassPtr(*thisclass_c, "Ogre::Light"));
  Real result_cpp;
  result_cpp = (thisclass_cpp->getShadowNearClipDistance());
  *result_c = (float)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_lgt_setShadowFarClipDistance(struct hg3dclass_struct * thisclass_c, float farClip_c)
{
  Ogre::Light * thisclass_cpp = static_cast<Ogre::Light*> (getHG3DClassPtr(*thisclass_c, "Ogre::Light"));
  Real farClip_cpp = (Real)farClip_c;
  (thisclass_cpp->setShadowFarClipDistance(farClip_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_lgt_getShadowFarClipDistance(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  Ogre::Light * thisclass_cpp = static_cast<Ogre::Light*> (getHG3DClassPtr(*thisclass_c, "Ogre::Light"));
  Real result_cpp;
  result_cpp = (thisclass_cpp->getShadowFarClipDistance());
  *result_c = (float)result_cpp;
};

