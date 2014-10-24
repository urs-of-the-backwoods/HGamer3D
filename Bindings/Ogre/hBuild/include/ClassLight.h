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

// ClassLight.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassLight
#define _DEFINED_HG3D_ClassLight

#include "ClassPtr.h"
#include "EnumLightType.h"
#include "StructColour.h"
#include "StructVec3.h"
#include "StructRadians.h"


// 
void ogre_lgt_construct(char * name_c, struct hg3dclass_struct * result_c);

// 
void ogre_lgt_destruct(struct hg3dclass_struct * thisclass_c);

// 
void ogre_lgt_setType(struct hg3dclass_struct * thisclass_c, enum EnumLightType type_c);

// 
void ogre_lgt_getType(struct hg3dclass_struct * thisclass_c, enum EnumLightType * result_c);

// 
void ogre_lgt_setDiffuseColour(struct hg3dclass_struct * thisclass_c, float red_c, float green_c, float blue_c);

// 
void ogre_lgt_setDiffuseColour2(struct hg3dclass_struct * thisclass_c, struct colourvalue_struct * colour_c);

// 
void ogre_lgt_getDiffuseColour(struct hg3dclass_struct * thisclass_c, struct colourvalue_struct * result_c);

// 
void ogre_lgt_setSpecularColour(struct hg3dclass_struct * thisclass_c, float red_c, float green_c, float blue_c);

// 
void ogre_lgt_setSpecularColour2(struct hg3dclass_struct * thisclass_c, struct colourvalue_struct * colour_c);

// 
void ogre_lgt_getSpecularColour(struct hg3dclass_struct * thisclass_c, struct colourvalue_struct * result_c);

// 
void ogre_lgt_setAttenuation(struct hg3dclass_struct * thisclass_c, float range_c, float constant_c, float linear_c, float quadratic_c);

// 
void ogre_lgt_getAttenuationRange(struct hg3dclass_struct * thisclass_c, float * result_c);

// 
void ogre_lgt_getAttenuationConstant(struct hg3dclass_struct * thisclass_c, float * result_c);

// 
void ogre_lgt_getAttenuationLinear(struct hg3dclass_struct * thisclass_c, float * result_c);

// 
void ogre_lgt_getAttenuationQuadric(struct hg3dclass_struct * thisclass_c, float * result_c);

// 
void ogre_lgt_setPosition(struct hg3dclass_struct * thisclass_c, float x_c, float y_c, float z_c);

// 
void ogre_lgt_setPosition2(struct hg3dclass_struct * thisclass_c, struct vector3_struct * vec_c);

// 
void ogre_lgt_getPosition(struct hg3dclass_struct * thisclass_c, struct vector3_struct * result_c);

// 
void ogre_lgt_setDirection(struct hg3dclass_struct * thisclass_c, float x_c, float y_c, float z_c);

// 
void ogre_lgt_setDirection2(struct hg3dclass_struct * thisclass_c, struct vector3_struct * vec_c);

// 
void ogre_lgt_getDirection(struct hg3dclass_struct * thisclass_c, struct vector3_struct * result_c);

// 
void ogre_lgt_setSpotlightRange(struct hg3dclass_struct * thisclass_c, struct radian_struct * innerAngle_c, struct radian_struct * outerAngle_c, float falloff_c);

// 
void ogre_lgt_getSpotlightInnerAngle(struct hg3dclass_struct * thisclass_c, struct radian_struct * result_c);

// 
void ogre_lgt_getSpotlightOuterAngle(struct hg3dclass_struct * thisclass_c, struct radian_struct * result_c);

// 
void ogre_lgt_getSpotlightFalloff(struct hg3dclass_struct * thisclass_c, float * result_c);

// 
void ogre_lgt_setSpotlightInnerAngle(struct hg3dclass_struct * thisclass_c, struct radian_struct * val_c);

// 
void ogre_lgt_setSpotlightOuterAngle(struct hg3dclass_struct * thisclass_c, struct radian_struct * val_c);

// 
void ogre_lgt_setSpotlightFalloff(struct hg3dclass_struct * thisclass_c, float val_c);

// 
void ogre_lgt_setSpotlightNearClipDistance(struct hg3dclass_struct * thisclass_c, float nearClip_c);

// 
void ogre_lgt_getSpotlightNearClipDistance(struct hg3dclass_struct * thisclass_c, float * result_c);

// 
void ogre_lgt_setPowerScale(struct hg3dclass_struct * thisclass_c, float power_c);

// 
void ogre_lgt_getPowerScale(struct hg3dclass_struct * thisclass_c, float * result_c);

// 
void ogre_lgt_getMovableType(struct hg3dclass_struct * thisclass_c, char * result_c);

// 
void ogre_lgt_getDerivedPosition(struct hg3dclass_struct * thisclass_c, long cameraRelativeIfSet_c, struct vector3_struct * result_c);

// 
void ogre_lgt_getDerivedDirection(struct hg3dclass_struct * thisclass_c, struct vector3_struct * result_c);

// 
void ogre_lgt_setVisible(struct hg3dclass_struct * thisclass_c, long visible_c);

// 
void ogre_lgt_getBoundingRadius(struct hg3dclass_struct * thisclass_c, float * result_c);

// Override to return specific type flag. 
void ogre_lgt_getTypeFlags(struct hg3dclass_struct * thisclass_c, unsigned long * result_c);

// 
void ogre_lgt_resetCustomShadowCameraSetup(struct hg3dclass_struct * thisclass_c);

// 
void ogre_lgt_setShadowFarDistance(struct hg3dclass_struct * thisclass_c, float distance_c);

// 
void ogre_lgt_resetShadowFarDistance(struct hg3dclass_struct * thisclass_c);

// 
void ogre_lgt_getShadowFarDistance(struct hg3dclass_struct * thisclass_c, float * result_c);

// 
void ogre_lgt_getShadowFarDistanceSquared(struct hg3dclass_struct * thisclass_c, float * result_c);

// 
void ogre_lgt_setShadowNearClipDistance(struct hg3dclass_struct * thisclass_c, float nearClip_c);

// 
void ogre_lgt_getShadowNearClipDistance(struct hg3dclass_struct * thisclass_c, float * result_c);

// 
void ogre_lgt_setShadowFarClipDistance(struct hg3dclass_struct * thisclass_c, float farClip_c);

// 
void ogre_lgt_getShadowFarClipDistance(struct hg3dclass_struct * thisclass_c, float * result_c);

#endif 
