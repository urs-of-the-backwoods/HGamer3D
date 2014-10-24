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

// ClassBillboard.cpp

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
#include "StructVec3.h"
#include "StructColour.h"
#include "./Ogre.h"
#include "./OgreString.h"
#include "./WindowUtilsHG3D.h"
#include "HG3DUtilities.h"

using namespace Ogre;
using Ogre::uint;



// 
extern "C" Ogre_LIB_EXPORT void ogre_bbd_construct(struct hg3dclass_struct * result_c)
{
  Ogre::Billboard * result_cpp;
  result_cpp = (new Ogre::Billboard());
  *result_c = getHG3DClass_Billboard((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbd_destruct(struct hg3dclass_struct * thisclass_c)
{
  Ogre::Billboard * thisclass_cpp = static_cast<Ogre::Billboard*> (getHG3DClassPtr(*thisclass_c, "Ogre::Billboard"));
  (delete thisclass_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbd_getRotation(struct hg3dclass_struct * thisclass_c, struct radian_struct * result_c)
{
  Ogre::Billboard * thisclass_cpp = static_cast<Ogre::Billboard*> (getHG3DClassPtr(*thisclass_c, "Ogre::Billboard"));
  Radian result_cpp;
  result_cpp = (thisclass_cpp->getRotation());
  *result_c = *((struct radian_struct*) &result_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbd_setRotation(struct hg3dclass_struct * thisclass_c, struct radian_struct * rotation_c)
{
  Ogre::Billboard * thisclass_cpp = static_cast<Ogre::Billboard*> (getHG3DClassPtr(*thisclass_c, "Ogre::Billboard"));
  Radian rotation_cpp = *((Radian*) rotation_c);
  (thisclass_cpp->setRotation(rotation_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbd_setPosition(struct hg3dclass_struct * thisclass_c, struct vector3_struct * position_c)
{
  Ogre::Billboard * thisclass_cpp = static_cast<Ogre::Billboard*> (getHG3DClassPtr(*thisclass_c, "Ogre::Billboard"));
  Vector3 position_cpp = *((Vector3*) position_c);
  (thisclass_cpp->setPosition(position_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbd_setPosition2(struct hg3dclass_struct * thisclass_c, float x_c, float y_c, float z_c)
{
  Ogre::Billboard * thisclass_cpp = static_cast<Ogre::Billboard*> (getHG3DClassPtr(*thisclass_c, "Ogre::Billboard"));
  Real x_cpp = (Real)x_c;
  Real y_cpp = (Real)y_c;
  Real z_cpp = (Real)z_c;
  (thisclass_cpp->setPosition(x_cpp, y_cpp, z_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbd_getPosition(struct hg3dclass_struct * thisclass_c, struct vector3_struct * result_c)
{
  Ogre::Billboard * thisclass_cpp = static_cast<Ogre::Billboard*> (getHG3DClassPtr(*thisclass_c, "Ogre::Billboard"));
  Vector3 result_cpp;
  result_cpp = (thisclass_cpp->getPosition());
  *result_c = *((struct vector3_struct*) &result_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbd_setDimensions(struct hg3dclass_struct * thisclass_c, float width_c, float height_c)
{
  Ogre::Billboard * thisclass_cpp = static_cast<Ogre::Billboard*> (getHG3DClassPtr(*thisclass_c, "Ogre::Billboard"));
  Real width_cpp = (Real)width_c;
  Real height_cpp = (Real)height_c;
  (thisclass_cpp->setDimensions(width_cpp, height_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbd_resetDimensions(struct hg3dclass_struct * thisclass_c)
{
  Ogre::Billboard * thisclass_cpp = static_cast<Ogre::Billboard*> (getHG3DClassPtr(*thisclass_c, "Ogre::Billboard"));
  (thisclass_cpp->resetDimensions());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbd_setColour(struct hg3dclass_struct * thisclass_c, struct colourvalue_struct * colour_c)
{
  Ogre::Billboard * thisclass_cpp = static_cast<Ogre::Billboard*> (getHG3DClassPtr(*thisclass_c, "Ogre::Billboard"));
  ColourValue colour_cpp = *((ColourValue*) colour_c);
  (thisclass_cpp->setColour(colour_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbd_getColour(struct hg3dclass_struct * thisclass_c, struct colourvalue_struct * result_c)
{
  Ogre::Billboard * thisclass_cpp = static_cast<Ogre::Billboard*> (getHG3DClassPtr(*thisclass_c, "Ogre::Billboard"));
  ColourValue result_cpp;
  result_cpp = (thisclass_cpp->getColour());
  *result_c = *((struct colourvalue_struct*) &result_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbd_hasOwnDimensions(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::Billboard * thisclass_cpp = static_cast<Ogre::Billboard*> (getHG3DClassPtr(*thisclass_c, "Ogre::Billboard"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->hasOwnDimensions());
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbd_getOwnWidth(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  Ogre::Billboard * thisclass_cpp = static_cast<Ogre::Billboard*> (getHG3DClassPtr(*thisclass_c, "Ogre::Billboard"));
  Real result_cpp;
  result_cpp = (thisclass_cpp->getOwnWidth());
  *result_c = (float)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbd_getOwnHeight(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  Ogre::Billboard * thisclass_cpp = static_cast<Ogre::Billboard*> (getHG3DClassPtr(*thisclass_c, "Ogre::Billboard"));
  Real result_cpp;
  result_cpp = (thisclass_cpp->getOwnHeight());
  *result_c = (float)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbd_isUseTexcoordRect(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::Billboard * thisclass_cpp = static_cast<Ogre::Billboard*> (getHG3DClassPtr(*thisclass_c, "Ogre::Billboard"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isUseTexcoordRect());
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbd_setTexcoordIndex(struct hg3dclass_struct * thisclass_c, unsigned short texcoordIndex_c)
{
  Ogre::Billboard * thisclass_cpp = static_cast<Ogre::Billboard*> (getHG3DClassPtr(*thisclass_c, "Ogre::Billboard"));
  uint16 texcoordIndex_cpp = (uint16)texcoordIndex_c;
  (thisclass_cpp->setTexcoordIndex(texcoordIndex_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbd_getTexcoordIndex(struct hg3dclass_struct * thisclass_c, unsigned short * result_c)
{
  Ogre::Billboard * thisclass_cpp = static_cast<Ogre::Billboard*> (getHG3DClassPtr(*thisclass_c, "Ogre::Billboard"));
  uint16 result_cpp;
  result_cpp = (thisclass_cpp->getTexcoordIndex());
  *result_c = (unsigned short)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbd_setTexcoordRect2(struct hg3dclass_struct * thisclass_c, float u0_c, float v0_c, float u1_c, float v1_c)
{
  Ogre::Billboard * thisclass_cpp = static_cast<Ogre::Billboard*> (getHG3DClassPtr(*thisclass_c, "Ogre::Billboard"));
  Real u0_cpp = (Real)u0_c;
  Real v0_cpp = (Real)v0_c;
  Real u1_cpp = (Real)u1_c;
  Real v1_cpp = (Real)v1_c;
  (thisclass_cpp->setTexcoordRect(u0_cpp, v0_cpp, u1_cpp, v1_cpp));
};

