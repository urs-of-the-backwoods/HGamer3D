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

// ClassManualObject.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassManualObject
#define _DEFINED_HG3D_ClassManualObject

#include "ClassPtr.h"
#include "EnumRenderOperationOperationType.h"
#include "StructVec3.h"
#include "StructVec2.h"
#include "StructColour.h"
#include "ClassManualObjectSection.h"
#include "StructSharedPtr.h"


// 
void ogre_mno_construct(char * name_c, struct hg3dclass_struct * result_c);

// 
void ogre_mno_destruct(struct hg3dclass_struct * thisclass_c);

// 
void ogre_mno_clear(struct hg3dclass_struct * thisclass_c);

// 
void ogre_mno_estimateVertexCount(struct hg3dclass_struct * thisclass_c, int vcount_c);

// 
void ogre_mno_estimateIndexCount(struct hg3dclass_struct * thisclass_c, int icount_c);

// 
void ogre_mno_begin(struct hg3dclass_struct * thisclass_c, char * materialName_c, enum EnumRenderOperationOperationType opType_c, char * groupName_c);

// 
void ogre_mno_setDynamic(struct hg3dclass_struct * thisclass_c, int dyn_c);

// 
void ogre_mno_getDynamic(struct hg3dclass_struct * thisclass_c, int * result_c);

// 
void ogre_mno_beginUpdate(struct hg3dclass_struct * thisclass_c, int sectionIndex_c);

// 
void ogre_mno_position(struct hg3dclass_struct * thisclass_c, struct vector3_struct * pos_c);

// 
void ogre_mno_position2(struct hg3dclass_struct * thisclass_c, float x_c, float y_c, float z_c);

// 
void ogre_mno_normal(struct hg3dclass_struct * thisclass_c, struct vector3_struct * norm_c);

// 
void ogre_mno_normal2(struct hg3dclass_struct * thisclass_c, float x_c, float y_c, float z_c);

// 
void ogre_mno_tangent(struct hg3dclass_struct * thisclass_c, struct vector3_struct * tan_c);

// 
void ogre_mno_tangent2(struct hg3dclass_struct * thisclass_c, float x_c, float y_c, float z_c);

// 
void ogre_mno_textureCoord(struct hg3dclass_struct * thisclass_c, float u_c);

// 
void ogre_mno_textureCoord2(struct hg3dclass_struct * thisclass_c, float u_c, float v_c);

// 
void ogre_mno_textureCoord3(struct hg3dclass_struct * thisclass_c, float u_c, float v_c, float w_c);

// 
void ogre_mno_textureCoord4(struct hg3dclass_struct * thisclass_c, float x_c, float y_c, float z_c, float w_c);

// 
void ogre_mno_textureCoord5(struct hg3dclass_struct * thisclass_c, struct vector2_struct * uv_c);

// 
void ogre_mno_textureCoord6(struct hg3dclass_struct * thisclass_c, struct vector3_struct * uvw_c);

// 
void ogre_mno_colour(struct hg3dclass_struct * thisclass_c, struct colourvalue_struct * col_c);

// 
void ogre_mno_colour2(struct hg3dclass_struct * thisclass_c, float r_c, float g_c, float b_c, float a_c);

// 
void ogre_mno_index(struct hg3dclass_struct * thisclass_c, unsigned int idx_c);

// 
void ogre_mno_triangle(struct hg3dclass_struct * thisclass_c, unsigned int i1_c, unsigned int i2_c, unsigned int i3_c);

// 
void ogre_mno_quad(struct hg3dclass_struct * thisclass_c, unsigned int i1_c, unsigned int i2_c, unsigned int i3_c, unsigned int i4_c);

// Get the number of vertices in the section currently being defined (returns 0 if no section is in progress). 
void ogre_mno_getCurrentVertexCount(struct hg3dclass_struct * thisclass_c, int * result_c);

// Get the number of indices in the section currently being defined (returns 0 if no section is in progress). 
void ogre_mno_getCurrentIndexCount(struct hg3dclass_struct * thisclass_c, int * result_c);

// 
void ogre_mno_end(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// 
void ogre_mno_setMaterialName(struct hg3dclass_struct * thisclass_c, int subindex_c, char * name_c, char * group_c);

// 
void ogre_mno_convertToMesh(struct hg3dclass_struct * thisclass_c, char * meshName_c, char * groupName_c, struct sharedptr_struct * result_c);

// 
void ogre_mno_setUseIdentityProjection(struct hg3dclass_struct * thisclass_c, int useIdentityProjection_c);

// 
void ogre_mno_getUseIdentityProjection(struct hg3dclass_struct * thisclass_c, int * result_c);

// 
void ogre_mno_setUseIdentityView(struct hg3dclass_struct * thisclass_c, int useIdentityView_c);

// 
void ogre_mno_getUseIdentityView(struct hg3dclass_struct * thisclass_c, int * result_c);

// 
void ogre_mno_getSection(struct hg3dclass_struct * thisclass_c, unsigned int index_c, struct hg3dclass_struct * result_c);

// 
void ogre_mno_getNumSections(struct hg3dclass_struct * thisclass_c, unsigned int * result_c);

// 
void ogre_mno_setKeepDeclarationOrder(struct hg3dclass_struct * thisclass_c, int keepOrder_c);

// 
void ogre_mno_getKeepDeclarationOrder(struct hg3dclass_struct * thisclass_c, int * result_c);

// . 
void ogre_mno_getMovableType(struct hg3dclass_struct * thisclass_c, char * result_c);

// . 
void ogre_mno_getBoundingRadius(struct hg3dclass_struct * thisclass_c, float * result_c);

// 
void ogre_mno_hasEdgeList(struct hg3dclass_struct * thisclass_c, int * result_c);

#endif 
