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

// ClassBillboardSet.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassBillboardSet
#define _DEFINED_HG3D_ClassBillboardSet

#include "ClassPtr.h"
#include "ClassBillboard.h"
#include "StructVec3.h"
#include "StructColour.h"
#include "EnumBillboardOrigin.h"
#include "EnumBillboardRotationType.h"
#include "StructSharedPtr.h"
#include "EnumBillboardType.h"
#include "ClassCamera.h"


// 
void ogre_bbs_construct(char * name_c, unsigned int poolSize_c, int externalDataSource_c, struct hg3dclass_struct * result_c);

// 
void ogre_bbs_destruct(struct hg3dclass_struct * thisclass_c);

// 
void ogre_bbs_createBillboard(struct hg3dclass_struct * thisclass_c, struct vector3_struct * position_c, struct colourvalue_struct * colour_c, struct hg3dclass_struct * result_c);

// 
void ogre_bbs_createBillboard2(struct hg3dclass_struct * thisclass_c, float x_c, float y_c, float z_c, struct colourvalue_struct * colour_c, struct hg3dclass_struct * result_c);

// 
void ogre_bbs_getNumBillboards(struct hg3dclass_struct * thisclass_c, int * result_c);

// 
void ogre_bbs_setAutoextend(struct hg3dclass_struct * thisclass_c, int autoextend_c);

// 
void ogre_bbs_getAutoextend(struct hg3dclass_struct * thisclass_c, int * result_c);

// 
void ogre_bbs_setSortingEnabled(struct hg3dclass_struct * thisclass_c, int sortenable_c);

// 
void ogre_bbs_getSortingEnabled(struct hg3dclass_struct * thisclass_c, int * result_c);

// 
void ogre_bbs_setPoolSize(struct hg3dclass_struct * thisclass_c, int size_c);

// 
void ogre_bbs_getPoolSize(struct hg3dclass_struct * thisclass_c, unsigned int * result_c);

// 
void ogre_bbs_clear(struct hg3dclass_struct * thisclass_c);

// 
void ogre_bbs_getBillboard(struct hg3dclass_struct * thisclass_c, unsigned int index_c, struct hg3dclass_struct * result_c);

// 
void ogre_bbs_removeBillboard(struct hg3dclass_struct * thisclass_c, unsigned int index_c);

// 
void ogre_bbs_removeBillboard2(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * pBill_c);

// 
void ogre_bbs_setBillboardOrigin(struct hg3dclass_struct * thisclass_c, enum EnumBillboardOrigin origin_c);

// 
void ogre_bbs_getBillboardOrigin(struct hg3dclass_struct * thisclass_c, enum EnumBillboardOrigin * result_c);

// 
void ogre_bbs_setBillboardRotationType(struct hg3dclass_struct * thisclass_c, enum EnumBillboardRotationType rotationType_c);

// 
void ogre_bbs_getBillboardRotationType(struct hg3dclass_struct * thisclass_c, enum EnumBillboardRotationType * result_c);

// 
void ogre_bbs_setDefaultDimensions(struct hg3dclass_struct * thisclass_c, float width_c, float height_c);

// 
void ogre_bbs_setDefaultWidth(struct hg3dclass_struct * thisclass_c, float width_c);

// 
void ogre_bbs_getDefaultWidth(struct hg3dclass_struct * thisclass_c, float * result_c);

// 
void ogre_bbs_setDefaultHeight(struct hg3dclass_struct * thisclass_c, float height_c);

// 
void ogre_bbs_getDefaultHeight(struct hg3dclass_struct * thisclass_c, float * result_c);

// 
void ogre_bbs_setMaterialName(struct hg3dclass_struct * thisclass_c, char * name_c, char * groupName_c);

// 
void ogre_bbs_getMaterialName(struct hg3dclass_struct * thisclass_c, char * result_c);

// 
void ogre_bbs_beginBillboards(struct hg3dclass_struct * thisclass_c, int numBillboards_c);

// 
void ogre_bbs_injectBillboard(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * bb_c);

// 
void ogre_bbs_endBillboards(struct hg3dclass_struct * thisclass_c);

// 
void ogre_bbs_getBoundingRadius(struct hg3dclass_struct * thisclass_c, float * result_c);

// 
void ogre_bbs_getMaterial(struct hg3dclass_struct * thisclass_c, struct sharedptr_struct * result_c);

// 
void ogre_bbs_setMaterial(struct hg3dclass_struct * thisclass_c, struct sharedptr_struct * material_c);

// 
void ogre_bbs_getCullIndividually(struct hg3dclass_struct * thisclass_c, int * result_c);

// 
void ogre_bbs_setCullIndividually(struct hg3dclass_struct * thisclass_c, int cullIndividual_c);

// 
void ogre_bbs_setBillboardType(struct hg3dclass_struct * thisclass_c, enum EnumBillboardType bbt_c);

// 
void ogre_bbs_getBillboardType(struct hg3dclass_struct * thisclass_c, enum EnumBillboardType * result_c);

// 
void ogre_bbs_setCommonDirection(struct hg3dclass_struct * thisclass_c, struct vector3_struct * vec_c);

// 
void ogre_bbs_getCommonDirection(struct hg3dclass_struct * thisclass_c, struct vector3_struct * result_c);

// 
void ogre_bbs_setCommonUpVector(struct hg3dclass_struct * thisclass_c, struct vector3_struct * vec_c);

// 
void ogre_bbs_getCommonUpVector(struct hg3dclass_struct * thisclass_c, struct vector3_struct * result_c);

// 
void ogre_bbs_setUseAccurateFacing(struct hg3dclass_struct * thisclass_c, int acc_c);

// 
void ogre_bbs_getUseAccurateFacing(struct hg3dclass_struct * thisclass_c, int * result_c);

// 
void ogre_bbs_getMovableType(struct hg3dclass_struct * thisclass_c, char * result_c);

// 
void ogre_bbs_getSquaredViewDepth(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * cam_c, float * result_c);

// 
void ogre_bbs_setBillboardsInWorldSpace(struct hg3dclass_struct * thisclass_c, int ws_c);

// 
void ogre_bbs_setPointRenderingEnabled(struct hg3dclass_struct * thisclass_c, int enabled_c);

// 
void ogre_bbs_isPointRenderingEnabled(struct hg3dclass_struct * thisclass_c, int * result_c);

// Override to return specific type flag. 
void ogre_bbs_getTypeFlags(struct hg3dclass_struct * thisclass_c, unsigned int * result_c);

// 
void ogre_bbs_setAutoUpdate(struct hg3dclass_struct * thisclass_c, int autoUpdate_c);

// 
void ogre_bbs_getAutoUpdate(struct hg3dclass_struct * thisclass_c, int * result_c);

// 
void ogre_bbs_notifyBillboardDataChanged(struct hg3dclass_struct * thisclass_c);

#endif 
