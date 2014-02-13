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

// ClassNode.cpp

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
	#include "StructQuaternion.h"
#include "StructVec3.h"
#include "EnumNodeTransformSpace.h"
#include "StructRadians.h"
#include "./Ogre.h"
#include "./OgreString.h"
#include "./WindowUtilsHG3D.h"
#include "HG3DUtilities.h"

using namespace Ogre;



// 
extern "C" Ogre_LIB_EXPORT void ogre_nd_destruct(struct hg3dclass_struct * thisclass_c)
{
  Ogre::Node * thisclass_cpp = static_cast<Ogre::Node*> (getHG3DClassPtr(*thisclass_c, "Ogre::Node"));
  (delete thisclass_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_nd_getName(struct hg3dclass_struct * thisclass_c, char * result_c)
{
  Ogre::Node * thisclass_cpp = static_cast<Ogre::Node*> (getHG3DClassPtr(*thisclass_c, "Ogre::Node"));
  Ogre::String result_cpp;
  result_cpp = (thisclass_cpp->getName());
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_nd_getParent(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  Ogre::Node * thisclass_cpp = static_cast<Ogre::Node*> (getHG3DClassPtr(*thisclass_c, "Ogre::Node"));
  Ogre::Node * result_cpp;
  result_cpp = (thisclass_cpp->getParent());
  *result_c = getHG3DClass_Node((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_nd_getOrientation(struct hg3dclass_struct * thisclass_c, struct quaternion_struct * result_c)
{
  Ogre::Node * thisclass_cpp = static_cast<Ogre::Node*> (getHG3DClassPtr(*thisclass_c, "Ogre::Node"));
  Quaternion result_cpp;
  result_cpp = (thisclass_cpp->getOrientation());
  *result_c = *((struct quaternion_struct*) &result_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_nd_setOrientation(struct hg3dclass_struct * thisclass_c, struct quaternion_struct * q_c)
{
  Ogre::Node * thisclass_cpp = static_cast<Ogre::Node*> (getHG3DClassPtr(*thisclass_c, "Ogre::Node"));
  Quaternion q_cpp = *((Quaternion*) q_c);
  (thisclass_cpp->setOrientation(q_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_nd_setOrientation2(struct hg3dclass_struct * thisclass_c, float w_c, float x_c, float y_c, float z_c)
{
  Ogre::Node * thisclass_cpp = static_cast<Ogre::Node*> (getHG3DClassPtr(*thisclass_c, "Ogre::Node"));
  Real w_cpp = (Real)w_c;
  Real x_cpp = (Real)x_c;
  Real y_cpp = (Real)y_c;
  Real z_cpp = (Real)z_c;
  (thisclass_cpp->setOrientation(w_cpp, x_cpp, y_cpp, z_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_nd_resetOrientation(struct hg3dclass_struct * thisclass_c)
{
  Ogre::Node * thisclass_cpp = static_cast<Ogre::Node*> (getHG3DClassPtr(*thisclass_c, "Ogre::Node"));
  (thisclass_cpp->resetOrientation());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_nd_setPosition(struct hg3dclass_struct * thisclass_c, struct vector3_struct * pos_c)
{
  Ogre::Node * thisclass_cpp = static_cast<Ogre::Node*> (getHG3DClassPtr(*thisclass_c, "Ogre::Node"));
  Vector3 pos_cpp = *((Vector3*) pos_c);
  (thisclass_cpp->setPosition(pos_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_nd_setPosition2(struct hg3dclass_struct * thisclass_c, float x_c, float y_c, float z_c)
{
  Ogre::Node * thisclass_cpp = static_cast<Ogre::Node*> (getHG3DClassPtr(*thisclass_c, "Ogre::Node"));
  Real x_cpp = (Real)x_c;
  Real y_cpp = (Real)y_c;
  Real z_cpp = (Real)z_c;
  (thisclass_cpp->setPosition(x_cpp, y_cpp, z_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_nd_getPosition(struct hg3dclass_struct * thisclass_c, struct vector3_struct * result_c)
{
  Ogre::Node * thisclass_cpp = static_cast<Ogre::Node*> (getHG3DClassPtr(*thisclass_c, "Ogre::Node"));
  Vector3 result_cpp;
  result_cpp = (thisclass_cpp->getPosition());
  *result_c = *((struct vector3_struct*) &result_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_nd_setScale(struct hg3dclass_struct * thisclass_c, struct vector3_struct * scale_c)
{
  Ogre::Node * thisclass_cpp = static_cast<Ogre::Node*> (getHG3DClassPtr(*thisclass_c, "Ogre::Node"));
  Vector3 scale_cpp = *((Vector3*) scale_c);
  (thisclass_cpp->setScale(scale_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_nd_setScale2(struct hg3dclass_struct * thisclass_c, float x_c, float y_c, float z_c)
{
  Ogre::Node * thisclass_cpp = static_cast<Ogre::Node*> (getHG3DClassPtr(*thisclass_c, "Ogre::Node"));
  Real x_cpp = (Real)x_c;
  Real y_cpp = (Real)y_c;
  Real z_cpp = (Real)z_c;
  (thisclass_cpp->setScale(x_cpp, y_cpp, z_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_nd_getScale(struct hg3dclass_struct * thisclass_c, struct vector3_struct * result_c)
{
  Ogre::Node * thisclass_cpp = static_cast<Ogre::Node*> (getHG3DClassPtr(*thisclass_c, "Ogre::Node"));
  Vector3 result_cpp;
  result_cpp = (thisclass_cpp->getScale());
  *result_c = *((struct vector3_struct*) &result_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_nd_setInheritOrientation(struct hg3dclass_struct * thisclass_c, int inherit_c)
{
  Ogre::Node * thisclass_cpp = static_cast<Ogre::Node*> (getHG3DClassPtr(*thisclass_c, "Ogre::Node"));
  bool inherit_cpp = (bool)inherit_c;
  (thisclass_cpp->setInheritOrientation(inherit_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_nd_getInheritOrientation(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::Node * thisclass_cpp = static_cast<Ogre::Node*> (getHG3DClassPtr(*thisclass_c, "Ogre::Node"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->getInheritOrientation());
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_nd_setInheritScale(struct hg3dclass_struct * thisclass_c, int inherit_c)
{
  Ogre::Node * thisclass_cpp = static_cast<Ogre::Node*> (getHG3DClassPtr(*thisclass_c, "Ogre::Node"));
  bool inherit_cpp = (bool)inherit_c;
  (thisclass_cpp->setInheritScale(inherit_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_nd_getInheritScale(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::Node * thisclass_cpp = static_cast<Ogre::Node*> (getHG3DClassPtr(*thisclass_c, "Ogre::Node"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->getInheritScale());
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_nd_scale(struct hg3dclass_struct * thisclass_c, struct vector3_struct * scale_c)
{
  Ogre::Node * thisclass_cpp = static_cast<Ogre::Node*> (getHG3DClassPtr(*thisclass_c, "Ogre::Node"));
  Vector3 scale_cpp = *((Vector3*) scale_c);
  (thisclass_cpp->scale(scale_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_nd_scale2(struct hg3dclass_struct * thisclass_c, float x_c, float y_c, float z_c)
{
  Ogre::Node * thisclass_cpp = static_cast<Ogre::Node*> (getHG3DClassPtr(*thisclass_c, "Ogre::Node"));
  Real x_cpp = (Real)x_c;
  Real y_cpp = (Real)y_c;
  Real z_cpp = (Real)z_c;
  (thisclass_cpp->scale(x_cpp, y_cpp, z_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_nd_translate(struct hg3dclass_struct * thisclass_c, struct vector3_struct * d_c, enum EnumNodeTransformSpace relativeTo_c)
{
  Ogre::Node * thisclass_cpp = static_cast<Ogre::Node*> (getHG3DClassPtr(*thisclass_c, "Ogre::Node"));
  Vector3 d_cpp = *((Vector3*) d_c);
  enum Ogre::Node::TransformSpace relativeTo_cpp = (enum Ogre::Node::TransformSpace)relativeTo_c;
  (thisclass_cpp->translate(d_cpp, relativeTo_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_nd_translate2(struct hg3dclass_struct * thisclass_c, float x_c, float y_c, float z_c, enum EnumNodeTransformSpace relativeTo_c)
{
  Ogre::Node * thisclass_cpp = static_cast<Ogre::Node*> (getHG3DClassPtr(*thisclass_c, "Ogre::Node"));
  Real x_cpp = (Real)x_c;
  Real y_cpp = (Real)y_c;
  Real z_cpp = (Real)z_c;
  enum Ogre::Node::TransformSpace relativeTo_cpp = (enum Ogre::Node::TransformSpace)relativeTo_c;
  (thisclass_cpp->translate(x_cpp, y_cpp, z_cpp, relativeTo_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_nd_roll(struct hg3dclass_struct * thisclass_c, struct radian_struct * angle_c, enum EnumNodeTransformSpace relativeTo_c)
{
  Ogre::Node * thisclass_cpp = static_cast<Ogre::Node*> (getHG3DClassPtr(*thisclass_c, "Ogre::Node"));
  Radian angle_cpp = *((Radian*) angle_c);
  enum Ogre::Node::TransformSpace relativeTo_cpp = (enum Ogre::Node::TransformSpace)relativeTo_c;
  (thisclass_cpp->roll(angle_cpp, relativeTo_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_nd_pitch(struct hg3dclass_struct * thisclass_c, struct radian_struct * angle_c, enum EnumNodeTransformSpace relativeTo_c)
{
  Ogre::Node * thisclass_cpp = static_cast<Ogre::Node*> (getHG3DClassPtr(*thisclass_c, "Ogre::Node"));
  Radian angle_cpp = *((Radian*) angle_c);
  enum Ogre::Node::TransformSpace relativeTo_cpp = (enum Ogre::Node::TransformSpace)relativeTo_c;
  (thisclass_cpp->pitch(angle_cpp, relativeTo_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_nd_yaw(struct hg3dclass_struct * thisclass_c, struct radian_struct * angle_c, enum EnumNodeTransformSpace relativeTo_c)
{
  Ogre::Node * thisclass_cpp = static_cast<Ogre::Node*> (getHG3DClassPtr(*thisclass_c, "Ogre::Node"));
  Radian angle_cpp = *((Radian*) angle_c);
  enum Ogre::Node::TransformSpace relativeTo_cpp = (enum Ogre::Node::TransformSpace)relativeTo_c;
  (thisclass_cpp->yaw(angle_cpp, relativeTo_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_nd_rotate(struct hg3dclass_struct * thisclass_c, struct vector3_struct * axis_c, struct radian_struct * angle_c, enum EnumNodeTransformSpace relativeTo_c)
{
  Ogre::Node * thisclass_cpp = static_cast<Ogre::Node*> (getHG3DClassPtr(*thisclass_c, "Ogre::Node"));
  Vector3 axis_cpp = *((Vector3*) axis_c);
  Radian angle_cpp = *((Radian*) angle_c);
  enum Ogre::Node::TransformSpace relativeTo_cpp = (enum Ogre::Node::TransformSpace)relativeTo_c;
  (thisclass_cpp->rotate(axis_cpp, angle_cpp, relativeTo_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_nd_rotate2(struct hg3dclass_struct * thisclass_c, struct quaternion_struct * q_c, enum EnumNodeTransformSpace relativeTo_c)
{
  Ogre::Node * thisclass_cpp = static_cast<Ogre::Node*> (getHG3DClassPtr(*thisclass_c, "Ogre::Node"));
  Quaternion q_cpp = *((Quaternion*) q_c);
  enum Ogre::Node::TransformSpace relativeTo_cpp = (enum Ogre::Node::TransformSpace)relativeTo_c;
  (thisclass_cpp->rotate(q_cpp, relativeTo_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_nd_createChild(struct hg3dclass_struct * thisclass_c, struct vector3_struct * translate_c, struct quaternion_struct * rotate_c, struct hg3dclass_struct * result_c)
{
  Ogre::Node * thisclass_cpp = static_cast<Ogre::Node*> (getHG3DClassPtr(*thisclass_c, "Ogre::Node"));
  Vector3 translate_cpp = *((Vector3*) translate_c);
  Quaternion rotate_cpp = *((Quaternion*) rotate_c);
  Ogre::Node * result_cpp;
  result_cpp = (thisclass_cpp->createChild(translate_cpp, rotate_cpp));
  *result_c = getHG3DClass_Node((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_nd_createChild2(struct hg3dclass_struct * thisclass_c, char * name_c, struct vector3_struct * translate_c, struct quaternion_struct * rotate_c, struct hg3dclass_struct * result_c)
{
  Ogre::Node * thisclass_cpp = static_cast<Ogre::Node*> (getHG3DClassPtr(*thisclass_c, "Ogre::Node"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  Vector3 translate_cpp = *((Vector3*) translate_c);
  Quaternion rotate_cpp = *((Quaternion*) rotate_c);
  Ogre::Node * result_cpp;
  result_cpp = (thisclass_cpp->createChild(name_cpp, translate_cpp, rotate_cpp));
  *result_c = getHG3DClass_Node((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_nd_addChild(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * child_c)
{
  Ogre::Node * thisclass_cpp = static_cast<Ogre::Node*> (getHG3DClassPtr(*thisclass_c, "Ogre::Node"));
  Ogre::Node * child_cpp = static_cast<Ogre::Node*> (getHG3DClassPtr(*child_c, "Ogre::Node"));
  (thisclass_cpp->addChild(child_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_nd_numChildren(struct hg3dclass_struct * thisclass_c, unsigned short * result_c)
{
  Ogre::Node * thisclass_cpp = static_cast<Ogre::Node*> (getHG3DClassPtr(*thisclass_c, "Ogre::Node"));
  unsigned short result_cpp;
  result_cpp = (thisclass_cpp->numChildren());
  *result_c = (unsigned short)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_nd_getChild(struct hg3dclass_struct * thisclass_c, unsigned short index_c, struct hg3dclass_struct * result_c)
{
  Ogre::Node * thisclass_cpp = static_cast<Ogre::Node*> (getHG3DClassPtr(*thisclass_c, "Ogre::Node"));
  unsigned short index_cpp = (unsigned short)index_c;
  Ogre::Node * result_cpp;
  result_cpp = (thisclass_cpp->getChild(index_cpp));
  *result_c = getHG3DClass_Node((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_nd_getChild2(struct hg3dclass_struct * thisclass_c, char * name_c, struct hg3dclass_struct * result_c)
{
  Ogre::Node * thisclass_cpp = static_cast<Ogre::Node*> (getHG3DClassPtr(*thisclass_c, "Ogre::Node"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  Ogre::Node * result_cpp;
  result_cpp = (thisclass_cpp->getChild(name_cpp));
  *result_c = getHG3DClass_Node((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_nd_removeChild(struct hg3dclass_struct * thisclass_c, unsigned short index_c, struct hg3dclass_struct * result_c)
{
  Ogre::Node * thisclass_cpp = static_cast<Ogre::Node*> (getHG3DClassPtr(*thisclass_c, "Ogre::Node"));
  unsigned short index_cpp = (unsigned short)index_c;
  Ogre::Node * result_cpp;
  result_cpp = (thisclass_cpp->removeChild(index_cpp));
  *result_c = getHG3DClass_Node((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_nd_removeChild2(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * child_c, struct hg3dclass_struct * result_c)
{
  Ogre::Node * thisclass_cpp = static_cast<Ogre::Node*> (getHG3DClassPtr(*thisclass_c, "Ogre::Node"));
  Ogre::Node * child_cpp = static_cast<Ogre::Node*> (getHG3DClassPtr(*child_c, "Ogre::Node"));
  Ogre::Node * result_cpp;
  result_cpp = (thisclass_cpp->removeChild(child_cpp));
  *result_c = getHG3DClass_Node((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_nd_removeChild3(struct hg3dclass_struct * thisclass_c, char * name_c, struct hg3dclass_struct * result_c)
{
  Ogre::Node * thisclass_cpp = static_cast<Ogre::Node*> (getHG3DClassPtr(*thisclass_c, "Ogre::Node"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  Ogre::Node * result_cpp;
  result_cpp = (thisclass_cpp->removeChild(name_cpp));
  *result_c = getHG3DClass_Node((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_nd_removeAllChildren(struct hg3dclass_struct * thisclass_c)
{
  Ogre::Node * thisclass_cpp = static_cast<Ogre::Node*> (getHG3DClassPtr(*thisclass_c, "Ogre::Node"));
  (thisclass_cpp->removeAllChildren());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_nd_setInitialState(struct hg3dclass_struct * thisclass_c)
{
  Ogre::Node * thisclass_cpp = static_cast<Ogre::Node*> (getHG3DClassPtr(*thisclass_c, "Ogre::Node"));
  (thisclass_cpp->setInitialState());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_nd_resetToInitialState(struct hg3dclass_struct * thisclass_c)
{
  Ogre::Node * thisclass_cpp = static_cast<Ogre::Node*> (getHG3DClassPtr(*thisclass_c, "Ogre::Node"));
  (thisclass_cpp->resetToInitialState());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_nd_getInitialPosition(struct hg3dclass_struct * thisclass_c, struct vector3_struct * result_c)
{
  Ogre::Node * thisclass_cpp = static_cast<Ogre::Node*> (getHG3DClassPtr(*thisclass_c, "Ogre::Node"));
  Vector3 result_cpp;
  result_cpp = (thisclass_cpp->getInitialPosition());
  *result_c = *((struct vector3_struct*) &result_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_nd_convertWorldToLocalPosition(struct hg3dclass_struct * thisclass_c, struct vector3_struct * worldPos_c, struct vector3_struct * result_c)
{
  Ogre::Node * thisclass_cpp = static_cast<Ogre::Node*> (getHG3DClassPtr(*thisclass_c, "Ogre::Node"));
  Vector3 worldPos_cpp = *((Vector3*) worldPos_c);
  Vector3 result_cpp;
  result_cpp = (thisclass_cpp->convertWorldToLocalPosition(worldPos_cpp));
  *result_c = *((struct vector3_struct*) &result_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_nd_convertLocalToWorldPosition(struct hg3dclass_struct * thisclass_c, struct vector3_struct * localPos_c, struct vector3_struct * result_c)
{
  Ogre::Node * thisclass_cpp = static_cast<Ogre::Node*> (getHG3DClassPtr(*thisclass_c, "Ogre::Node"));
  Vector3 localPos_cpp = *((Vector3*) localPos_c);
  Vector3 result_cpp;
  result_cpp = (thisclass_cpp->convertLocalToWorldPosition(localPos_cpp));
  *result_c = *((struct vector3_struct*) &result_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_nd_convertWorldToLocalOrientation(struct hg3dclass_struct * thisclass_c, struct quaternion_struct * worldOrientation_c, struct quaternion_struct * result_c)
{
  Ogre::Node * thisclass_cpp = static_cast<Ogre::Node*> (getHG3DClassPtr(*thisclass_c, "Ogre::Node"));
  Quaternion worldOrientation_cpp = *((Quaternion*) worldOrientation_c);
  Quaternion result_cpp;
  result_cpp = (thisclass_cpp->convertWorldToLocalOrientation(worldOrientation_cpp));
  *result_c = *((struct quaternion_struct*) &result_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_nd_convertLocalToWorldOrientation(struct hg3dclass_struct * thisclass_c, struct quaternion_struct * localOrientation_c, struct quaternion_struct * result_c)
{
  Ogre::Node * thisclass_cpp = static_cast<Ogre::Node*> (getHG3DClassPtr(*thisclass_c, "Ogre::Node"));
  Quaternion localOrientation_cpp = *((Quaternion*) localOrientation_c);
  Quaternion result_cpp;
  result_cpp = (thisclass_cpp->convertLocalToWorldOrientation(localOrientation_cpp));
  *result_c = *((struct quaternion_struct*) &result_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_nd_getInitialOrientation(struct hg3dclass_struct * thisclass_c, struct quaternion_struct * result_c)
{
  Ogre::Node * thisclass_cpp = static_cast<Ogre::Node*> (getHG3DClassPtr(*thisclass_c, "Ogre::Node"));
  Quaternion result_cpp;
  result_cpp = (thisclass_cpp->getInitialOrientation());
  *result_c = *((struct quaternion_struct*) &result_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_nd_getInitialScale(struct hg3dclass_struct * thisclass_c, struct vector3_struct * result_c)
{
  Ogre::Node * thisclass_cpp = static_cast<Ogre::Node*> (getHG3DClassPtr(*thisclass_c, "Ogre::Node"));
  Vector3 result_cpp;
  result_cpp = (thisclass_cpp->getInitialScale());
  *result_c = *((struct vector3_struct*) &result_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_nd_getSquaredViewDepth(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * cam_c, float * result_c)
{
  Ogre::Node * thisclass_cpp = static_cast<Ogre::Node*> (getHG3DClassPtr(*thisclass_c, "Ogre::Node"));
  const Ogre::Camera * cam_cpp = static_cast<Ogre::Camera*> (getHG3DClassPtr(*cam_c, "Ogre::Camera"));
  Real result_cpp;
  result_cpp = (thisclass_cpp->getSquaredViewDepth(cam_cpp));
  *result_c = (float)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_nd_needUpdate(struct hg3dclass_struct * thisclass_c, int forceParentUpdate_c)
{
  Ogre::Node * thisclass_cpp = static_cast<Ogre::Node*> (getHG3DClassPtr(*thisclass_c, "Ogre::Node"));
  bool forceParentUpdate_cpp = (bool)forceParentUpdate_c;
  (thisclass_cpp->needUpdate(forceParentUpdate_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_nd_requestUpdate(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * child_c, int forceParentUpdate_c)
{
  Ogre::Node * thisclass_cpp = static_cast<Ogre::Node*> (getHG3DClassPtr(*thisclass_c, "Ogre::Node"));
  Ogre::Node * child_cpp = static_cast<Ogre::Node*> (getHG3DClassPtr(*child_c, "Ogre::Node"));
  bool forceParentUpdate_cpp = (bool)forceParentUpdate_c;
  (thisclass_cpp->requestUpdate(child_cpp, forceParentUpdate_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_nd_cancelUpdate(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * child_c)
{
  Ogre::Node * thisclass_cpp = static_cast<Ogre::Node*> (getHG3DClassPtr(*thisclass_c, "Ogre::Node"));
  Ogre::Node * child_cpp = static_cast<Ogre::Node*> (getHG3DClassPtr(*child_c, "Ogre::Node"));
  (thisclass_cpp->cancelUpdate(child_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_nd_queueNeedUpdate(struct hg3dclass_struct * n_c)
{
  Ogre::Node * n_cpp = static_cast<Ogre::Node*> (getHG3DClassPtr(*n_c, "Ogre::Node"));
  (Ogre::Node::queueNeedUpdate(n_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_nd_processQueuedUpdates()
{
  (Ogre::Node::processQueuedUpdates());
};

