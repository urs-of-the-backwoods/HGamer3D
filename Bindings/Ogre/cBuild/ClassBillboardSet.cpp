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

// ClassBillboardSet.cpp

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
#include "StructColour.h"
#include "EnumBillboardOrigin.h"
#include "EnumBillboardRotationType.h"
#include "StructSharedPtr.h"
#include "EnumBillboardType.h"
#include "./Ogre.h"
#include "./OgreString.h"
#include "./WindowUtilsHG3D.h"
#include "HG3DUtilities.h"

using namespace Ogre;
using Ogre::uint;



// 
extern "C" Ogre_LIB_EXPORT void ogre_bbs_construct(char * name_c, unsigned long poolSize_c, long externalDataSource_c, struct hg3dclass_struct * result_c)
{
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  unsigned int poolSize_cpp = (unsigned int)poolSize_c;
  bool externalDataSource_cpp = (bool)externalDataSource_c;
  Ogre::BillboardSet * result_cpp;
  result_cpp = (new Ogre::BillboardSet(name_cpp, poolSize_cpp, externalDataSource_cpp));
  *result_c = getHG3DClass_BillboardSet((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbs_destruct(struct hg3dclass_struct * thisclass_c)
{
  Ogre::BillboardSet * thisclass_cpp = static_cast<Ogre::BillboardSet*> (getHG3DClassPtr(*thisclass_c, "Ogre::BillboardSet"));
  (delete thisclass_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbs_createBillboard(struct hg3dclass_struct * thisclass_c, struct vector3_struct * position_c, struct colourvalue_struct * colour_c, struct hg3dclass_struct * result_c)
{
  Ogre::BillboardSet * thisclass_cpp = static_cast<Ogre::BillboardSet*> (getHG3DClassPtr(*thisclass_c, "Ogre::BillboardSet"));
  Vector3 position_cpp = *((Vector3*) position_c);
  ColourValue colour_cpp = *((ColourValue*) colour_c);
  Ogre::Billboard * result_cpp;
  result_cpp = (thisclass_cpp->createBillboard(position_cpp, colour_cpp));
  *result_c = getHG3DClass_Billboard((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbs_createBillboard2(struct hg3dclass_struct * thisclass_c, float x_c, float y_c, float z_c, struct colourvalue_struct * colour_c, struct hg3dclass_struct * result_c)
{
  Ogre::BillboardSet * thisclass_cpp = static_cast<Ogre::BillboardSet*> (getHG3DClassPtr(*thisclass_c, "Ogre::BillboardSet"));
  Real x_cpp = (Real)x_c;
  Real y_cpp = (Real)y_c;
  Real z_cpp = (Real)z_c;
  ColourValue colour_cpp = *((ColourValue*) colour_c);
  Ogre::Billboard * result_cpp;
  result_cpp = (thisclass_cpp->createBillboard(x_cpp, y_cpp, z_cpp, colour_cpp));
  *result_c = getHG3DClass_Billboard((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbs_getNumBillboards(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::BillboardSet * thisclass_cpp = static_cast<Ogre::BillboardSet*> (getHG3DClassPtr(*thisclass_c, "Ogre::BillboardSet"));
  int result_cpp;
  result_cpp = (thisclass_cpp->getNumBillboards());
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbs_setAutoextend(struct hg3dclass_struct * thisclass_c, long autoextend_c)
{
  Ogre::BillboardSet * thisclass_cpp = static_cast<Ogre::BillboardSet*> (getHG3DClassPtr(*thisclass_c, "Ogre::BillboardSet"));
  bool autoextend_cpp = (bool)autoextend_c;
  (thisclass_cpp->setAutoextend(autoextend_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbs_getAutoextend(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::BillboardSet * thisclass_cpp = static_cast<Ogre::BillboardSet*> (getHG3DClassPtr(*thisclass_c, "Ogre::BillboardSet"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->getAutoextend());
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbs_setSortingEnabled(struct hg3dclass_struct * thisclass_c, long sortenable_c)
{
  Ogre::BillboardSet * thisclass_cpp = static_cast<Ogre::BillboardSet*> (getHG3DClassPtr(*thisclass_c, "Ogre::BillboardSet"));
  bool sortenable_cpp = (bool)sortenable_c;
  (thisclass_cpp->setSortingEnabled(sortenable_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbs_getSortingEnabled(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::BillboardSet * thisclass_cpp = static_cast<Ogre::BillboardSet*> (getHG3DClassPtr(*thisclass_c, "Ogre::BillboardSet"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->getSortingEnabled());
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbs_setPoolSize(struct hg3dclass_struct * thisclass_c, long size_c)
{
  Ogre::BillboardSet * thisclass_cpp = static_cast<Ogre::BillboardSet*> (getHG3DClassPtr(*thisclass_c, "Ogre::BillboardSet"));
  size_t size_cpp = (size_t)size_c;
  (thisclass_cpp->setPoolSize(size_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbs_getPoolSize(struct hg3dclass_struct * thisclass_c, unsigned long * result_c)
{
  Ogre::BillboardSet * thisclass_cpp = static_cast<Ogre::BillboardSet*> (getHG3DClassPtr(*thisclass_c, "Ogre::BillboardSet"));
  unsigned int result_cpp;
  result_cpp = (thisclass_cpp->getPoolSize());
  *result_c = (unsigned long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbs_clear(struct hg3dclass_struct * thisclass_c)
{
  Ogre::BillboardSet * thisclass_cpp = static_cast<Ogre::BillboardSet*> (getHG3DClassPtr(*thisclass_c, "Ogre::BillboardSet"));
  (thisclass_cpp->clear());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbs_getBillboard(struct hg3dclass_struct * thisclass_c, unsigned long index_c, struct hg3dclass_struct * result_c)
{
  Ogre::BillboardSet * thisclass_cpp = static_cast<Ogre::BillboardSet*> (getHG3DClassPtr(*thisclass_c, "Ogre::BillboardSet"));
  unsigned int index_cpp = (unsigned int)index_c;
  Ogre::Billboard * result_cpp;
  result_cpp = (thisclass_cpp->getBillboard(index_cpp));
  *result_c = getHG3DClass_Billboard((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbs_removeBillboard(struct hg3dclass_struct * thisclass_c, unsigned long index_c)
{
  Ogre::BillboardSet * thisclass_cpp = static_cast<Ogre::BillboardSet*> (getHG3DClassPtr(*thisclass_c, "Ogre::BillboardSet"));
  unsigned int index_cpp = (unsigned int)index_c;
  (thisclass_cpp->removeBillboard(index_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbs_removeBillboard2(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * pBill_c)
{
  Ogre::BillboardSet * thisclass_cpp = static_cast<Ogre::BillboardSet*> (getHG3DClassPtr(*thisclass_c, "Ogre::BillboardSet"));
  Ogre::Billboard * pBill_cpp = static_cast<Ogre::Billboard*> (getHG3DClassPtr(*pBill_c, "Ogre::Billboard"));
  (thisclass_cpp->removeBillboard(pBill_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbs_setBillboardOrigin(struct hg3dclass_struct * thisclass_c, enum EnumBillboardOrigin origin_c)
{
  Ogre::BillboardSet * thisclass_cpp = static_cast<Ogre::BillboardSet*> (getHG3DClassPtr(*thisclass_c, "Ogre::BillboardSet"));
  enum Ogre::BillboardOrigin origin_cpp = (enum Ogre::BillboardOrigin)origin_c;
  (thisclass_cpp->setBillboardOrigin(origin_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbs_getBillboardOrigin(struct hg3dclass_struct * thisclass_c, enum EnumBillboardOrigin * result_c)
{
  Ogre::BillboardSet * thisclass_cpp = static_cast<Ogre::BillboardSet*> (getHG3DClassPtr(*thisclass_c, "Ogre::BillboardSet"));
  enum Ogre::BillboardOrigin result_cpp;
  result_cpp = (thisclass_cpp->getBillboardOrigin());
  *result_c = (enum EnumBillboardOrigin) result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbs_setBillboardRotationType(struct hg3dclass_struct * thisclass_c, enum EnumBillboardRotationType rotationType_c)
{
  Ogre::BillboardSet * thisclass_cpp = static_cast<Ogre::BillboardSet*> (getHG3DClassPtr(*thisclass_c, "Ogre::BillboardSet"));
  enum Ogre::BillboardRotationType rotationType_cpp = (enum Ogre::BillboardRotationType)rotationType_c;
  (thisclass_cpp->setBillboardRotationType(rotationType_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbs_getBillboardRotationType(struct hg3dclass_struct * thisclass_c, enum EnumBillboardRotationType * result_c)
{
  Ogre::BillboardSet * thisclass_cpp = static_cast<Ogre::BillboardSet*> (getHG3DClassPtr(*thisclass_c, "Ogre::BillboardSet"));
  enum Ogre::BillboardRotationType result_cpp;
  result_cpp = (thisclass_cpp->getBillboardRotationType());
  *result_c = (enum EnumBillboardRotationType) result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbs_setDefaultDimensions(struct hg3dclass_struct * thisclass_c, float width_c, float height_c)
{
  Ogre::BillboardSet * thisclass_cpp = static_cast<Ogre::BillboardSet*> (getHG3DClassPtr(*thisclass_c, "Ogre::BillboardSet"));
  Real width_cpp = (Real)width_c;
  Real height_cpp = (Real)height_c;
  (thisclass_cpp->setDefaultDimensions(width_cpp, height_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbs_setDefaultWidth(struct hg3dclass_struct * thisclass_c, float width_c)
{
  Ogre::BillboardSet * thisclass_cpp = static_cast<Ogre::BillboardSet*> (getHG3DClassPtr(*thisclass_c, "Ogre::BillboardSet"));
  Real width_cpp = (Real)width_c;
  (thisclass_cpp->setDefaultWidth(width_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbs_getDefaultWidth(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  Ogre::BillboardSet * thisclass_cpp = static_cast<Ogre::BillboardSet*> (getHG3DClassPtr(*thisclass_c, "Ogre::BillboardSet"));
  Real result_cpp;
  result_cpp = (thisclass_cpp->getDefaultWidth());
  *result_c = (float)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbs_setDefaultHeight(struct hg3dclass_struct * thisclass_c, float height_c)
{
  Ogre::BillboardSet * thisclass_cpp = static_cast<Ogre::BillboardSet*> (getHG3DClassPtr(*thisclass_c, "Ogre::BillboardSet"));
  Real height_cpp = (Real)height_c;
  (thisclass_cpp->setDefaultHeight(height_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbs_getDefaultHeight(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  Ogre::BillboardSet * thisclass_cpp = static_cast<Ogre::BillboardSet*> (getHG3DClassPtr(*thisclass_c, "Ogre::BillboardSet"));
  Real result_cpp;
  result_cpp = (thisclass_cpp->getDefaultHeight());
  *result_c = (float)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbs_setMaterialName(struct hg3dclass_struct * thisclass_c, char * name_c, char * groupName_c)
{
  Ogre::BillboardSet * thisclass_cpp = static_cast<Ogre::BillboardSet*> (getHG3DClassPtr(*thisclass_c, "Ogre::BillboardSet"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  Ogre::String groupName_cpp = Ogre::String((const char*) groupName_c);
  (thisclass_cpp->setMaterialName(name_cpp, groupName_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbs_getMaterialName(struct hg3dclass_struct * thisclass_c, char * result_c)
{
  Ogre::BillboardSet * thisclass_cpp = static_cast<Ogre::BillboardSet*> (getHG3DClassPtr(*thisclass_c, "Ogre::BillboardSet"));
  Ogre::String result_cpp;
  result_cpp = (thisclass_cpp->getMaterialName());
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbs_beginBillboards(struct hg3dclass_struct * thisclass_c, long numBillboards_c)
{
  Ogre::BillboardSet * thisclass_cpp = static_cast<Ogre::BillboardSet*> (getHG3DClassPtr(*thisclass_c, "Ogre::BillboardSet"));
  size_t numBillboards_cpp = (size_t)numBillboards_c;
  (thisclass_cpp->beginBillboards(numBillboards_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbs_injectBillboard(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * bb_c)
{
  Ogre::BillboardSet * thisclass_cpp = static_cast<Ogre::BillboardSet*> (getHG3DClassPtr(*thisclass_c, "Ogre::BillboardSet"));
  const Ogre::Billboard * bb_cpp = static_cast<Ogre::Billboard*> (getHG3DClassPtr(*bb_c, "Ogre::Billboard"));
  (thisclass_cpp->injectBillboard(*bb_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbs_endBillboards(struct hg3dclass_struct * thisclass_c)
{
  Ogre::BillboardSet * thisclass_cpp = static_cast<Ogre::BillboardSet*> (getHG3DClassPtr(*thisclass_c, "Ogre::BillboardSet"));
  (thisclass_cpp->endBillboards());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbs_getBoundingRadius(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  Ogre::BillboardSet * thisclass_cpp = static_cast<Ogre::BillboardSet*> (getHG3DClassPtr(*thisclass_c, "Ogre::BillboardSet"));
  Real result_cpp;
  result_cpp = (thisclass_cpp->getBoundingRadius());
  *result_c = (float)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbs_getMaterial(struct hg3dclass_struct * thisclass_c, struct sharedptr_struct * result_c)
{
  Ogre::BillboardSet * thisclass_cpp = static_cast<Ogre::BillboardSet*> (getHG3DClassPtr(*thisclass_c, "Ogre::BillboardSet"));
  MaterialPtr result_cpp;
  result_cpp = (thisclass_cpp->getMaterial());
  *result_c = *((struct sharedptr_struct*) &result_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbs_setMaterial(struct hg3dclass_struct * thisclass_c, struct sharedptr_struct * material_c)
{
  Ogre::BillboardSet * thisclass_cpp = static_cast<Ogre::BillboardSet*> (getHG3DClassPtr(*thisclass_c, "Ogre::BillboardSet"));
  MaterialPtr material_cpp = *((MaterialPtr*) material_c);
  (thisclass_cpp->setMaterial(material_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbs_getCullIndividually(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::BillboardSet * thisclass_cpp = static_cast<Ogre::BillboardSet*> (getHG3DClassPtr(*thisclass_c, "Ogre::BillboardSet"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->getCullIndividually());
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbs_setCullIndividually(struct hg3dclass_struct * thisclass_c, long cullIndividual_c)
{
  Ogre::BillboardSet * thisclass_cpp = static_cast<Ogre::BillboardSet*> (getHG3DClassPtr(*thisclass_c, "Ogre::BillboardSet"));
  bool cullIndividual_cpp = (bool)cullIndividual_c;
  (thisclass_cpp->setCullIndividually(cullIndividual_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbs_setBillboardType(struct hg3dclass_struct * thisclass_c, enum EnumBillboardType bbt_c)
{
  Ogre::BillboardSet * thisclass_cpp = static_cast<Ogre::BillboardSet*> (getHG3DClassPtr(*thisclass_c, "Ogre::BillboardSet"));
  enum Ogre::BillboardType bbt_cpp = (enum Ogre::BillboardType)bbt_c;
  (thisclass_cpp->setBillboardType(bbt_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbs_getBillboardType(struct hg3dclass_struct * thisclass_c, enum EnumBillboardType * result_c)
{
  Ogre::BillboardSet * thisclass_cpp = static_cast<Ogre::BillboardSet*> (getHG3DClassPtr(*thisclass_c, "Ogre::BillboardSet"));
  enum Ogre::BillboardType result_cpp;
  result_cpp = (thisclass_cpp->getBillboardType());
  *result_c = (enum EnumBillboardType) result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbs_setCommonDirection(struct hg3dclass_struct * thisclass_c, struct vector3_struct * vec_c)
{
  Ogre::BillboardSet * thisclass_cpp = static_cast<Ogre::BillboardSet*> (getHG3DClassPtr(*thisclass_c, "Ogre::BillboardSet"));
  Vector3 vec_cpp = *((Vector3*) vec_c);
  (thisclass_cpp->setCommonDirection(vec_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbs_getCommonDirection(struct hg3dclass_struct * thisclass_c, struct vector3_struct * result_c)
{
  Ogre::BillboardSet * thisclass_cpp = static_cast<Ogre::BillboardSet*> (getHG3DClassPtr(*thisclass_c, "Ogre::BillboardSet"));
  Vector3 result_cpp;
  result_cpp = (thisclass_cpp->getCommonDirection());
  *result_c = *((struct vector3_struct*) &result_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbs_setCommonUpVector(struct hg3dclass_struct * thisclass_c, struct vector3_struct * vec_c)
{
  Ogre::BillboardSet * thisclass_cpp = static_cast<Ogre::BillboardSet*> (getHG3DClassPtr(*thisclass_c, "Ogre::BillboardSet"));
  Vector3 vec_cpp = *((Vector3*) vec_c);
  (thisclass_cpp->setCommonUpVector(vec_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbs_getCommonUpVector(struct hg3dclass_struct * thisclass_c, struct vector3_struct * result_c)
{
  Ogre::BillboardSet * thisclass_cpp = static_cast<Ogre::BillboardSet*> (getHG3DClassPtr(*thisclass_c, "Ogre::BillboardSet"));
  Vector3 result_cpp;
  result_cpp = (thisclass_cpp->getCommonUpVector());
  *result_c = *((struct vector3_struct*) &result_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbs_setUseAccurateFacing(struct hg3dclass_struct * thisclass_c, long acc_c)
{
  Ogre::BillboardSet * thisclass_cpp = static_cast<Ogre::BillboardSet*> (getHG3DClassPtr(*thisclass_c, "Ogre::BillboardSet"));
  bool acc_cpp = (bool)acc_c;
  (thisclass_cpp->setUseAccurateFacing(acc_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbs_getUseAccurateFacing(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::BillboardSet * thisclass_cpp = static_cast<Ogre::BillboardSet*> (getHG3DClassPtr(*thisclass_c, "Ogre::BillboardSet"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->getUseAccurateFacing());
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbs_getMovableType(struct hg3dclass_struct * thisclass_c, char * result_c)
{
  Ogre::BillboardSet * thisclass_cpp = static_cast<Ogre::BillboardSet*> (getHG3DClassPtr(*thisclass_c, "Ogre::BillboardSet"));
  Ogre::String result_cpp;
  result_cpp = (thisclass_cpp->getMovableType());
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbs_getSquaredViewDepth(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * cam_c, float * result_c)
{
  Ogre::BillboardSet * thisclass_cpp = static_cast<Ogre::BillboardSet*> (getHG3DClassPtr(*thisclass_c, "Ogre::BillboardSet"));
  const Ogre::Camera * cam_cpp = static_cast<Ogre::Camera*> (getHG3DClassPtr(*cam_c, "Ogre::Camera"));
  Real result_cpp;
  result_cpp = (thisclass_cpp->getSquaredViewDepth(cam_cpp));
  *result_c = (float)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbs_setBillboardsInWorldSpace(struct hg3dclass_struct * thisclass_c, long ws_c)
{
  Ogre::BillboardSet * thisclass_cpp = static_cast<Ogre::BillboardSet*> (getHG3DClassPtr(*thisclass_c, "Ogre::BillboardSet"));
  bool ws_cpp = (bool)ws_c;
  (thisclass_cpp->setBillboardsInWorldSpace(ws_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbs_setPointRenderingEnabled(struct hg3dclass_struct * thisclass_c, long enabled_c)
{
  Ogre::BillboardSet * thisclass_cpp = static_cast<Ogre::BillboardSet*> (getHG3DClassPtr(*thisclass_c, "Ogre::BillboardSet"));
  bool enabled_cpp = (bool)enabled_c;
  (thisclass_cpp->setPointRenderingEnabled(enabled_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbs_isPointRenderingEnabled(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::BillboardSet * thisclass_cpp = static_cast<Ogre::BillboardSet*> (getHG3DClassPtr(*thisclass_c, "Ogre::BillboardSet"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isPointRenderingEnabled());
  *result_c = (long)result_cpp;
};

// Override to return specific type flag. 
extern "C" Ogre_LIB_EXPORT void ogre_bbs_getTypeFlags(struct hg3dclass_struct * thisclass_c, unsigned long * result_c)
{
  Ogre::BillboardSet * thisclass_cpp = static_cast<Ogre::BillboardSet*> (getHG3DClassPtr(*thisclass_c, "Ogre::BillboardSet"));
  uint32 result_cpp;
  result_cpp = (thisclass_cpp->getTypeFlags());
  *result_c = (unsigned long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbs_setAutoUpdate(struct hg3dclass_struct * thisclass_c, long autoUpdate_c)
{
  Ogre::BillboardSet * thisclass_cpp = static_cast<Ogre::BillboardSet*> (getHG3DClassPtr(*thisclass_c, "Ogre::BillboardSet"));
  bool autoUpdate_cpp = (bool)autoUpdate_c;
  (thisclass_cpp->setAutoUpdate(autoUpdate_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbs_getAutoUpdate(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::BillboardSet * thisclass_cpp = static_cast<Ogre::BillboardSet*> (getHG3DClassPtr(*thisclass_c, "Ogre::BillboardSet"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->getAutoUpdate());
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbs_notifyBillboardDataChanged(struct hg3dclass_struct * thisclass_c)
{
  Ogre::BillboardSet * thisclass_cpp = static_cast<Ogre::BillboardSet*> (getHG3DClassPtr(*thisclass_c, "Ogre::BillboardSet"));
  (thisclass_cpp->notifyBillboardDataChanged());
};

