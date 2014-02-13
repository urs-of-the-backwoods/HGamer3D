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

// ClassManualObject.cpp

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
	#include "EnumRenderOperationOperationType.h"
#include "StructVec3.h"
#include "StructVec2.h"
#include "StructColour.h"
#include "StructSharedPtr.h"
#include "./Ogre.h"
#include "./OgreString.h"
#include "./WindowUtilsHG3D.h"
#include "HG3DUtilities.h"

using namespace Ogre;



// 
extern "C" Ogre_LIB_EXPORT void ogre_mno_construct(char * name_c, struct hg3dclass_struct * result_c)
{
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  Ogre::ManualObject * result_cpp;
  result_cpp = (new Ogre::ManualObject(name_cpp));
  *result_c = getHG3DClass_ManualObject((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mno_destruct(struct hg3dclass_struct * thisclass_c)
{
  Ogre::ManualObject * thisclass_cpp = static_cast<Ogre::ManualObject*> (getHG3DClassPtr(*thisclass_c, "Ogre::ManualObject"));
  (delete thisclass_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mno_clear(struct hg3dclass_struct * thisclass_c)
{
  Ogre::ManualObject * thisclass_cpp = static_cast<Ogre::ManualObject*> (getHG3DClassPtr(*thisclass_c, "Ogre::ManualObject"));
  (thisclass_cpp->clear());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mno_estimateVertexCount(struct hg3dclass_struct * thisclass_c, int vcount_c)
{
  Ogre::ManualObject * thisclass_cpp = static_cast<Ogre::ManualObject*> (getHG3DClassPtr(*thisclass_c, "Ogre::ManualObject"));
  size_t vcount_cpp = (size_t)vcount_c;
  (thisclass_cpp->estimateVertexCount(vcount_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mno_estimateIndexCount(struct hg3dclass_struct * thisclass_c, int icount_c)
{
  Ogre::ManualObject * thisclass_cpp = static_cast<Ogre::ManualObject*> (getHG3DClassPtr(*thisclass_c, "Ogre::ManualObject"));
  size_t icount_cpp = (size_t)icount_c;
  (thisclass_cpp->estimateIndexCount(icount_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mno_begin(struct hg3dclass_struct * thisclass_c, char * materialName_c, enum EnumRenderOperationOperationType opType_c, char * groupName_c)
{
  Ogre::ManualObject * thisclass_cpp = static_cast<Ogre::ManualObject*> (getHG3DClassPtr(*thisclass_c, "Ogre::ManualObject"));
  Ogre::String materialName_cpp = Ogre::String((const char*) materialName_c);
  enum RenderOperation::OperationType opType_cpp = (enum RenderOperation::OperationType)opType_c;
  Ogre::String groupName_cpp = Ogre::String((const char*) groupName_c);
  (thisclass_cpp->begin(materialName_cpp, opType_cpp, groupName_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mno_setDynamic(struct hg3dclass_struct * thisclass_c, int dyn_c)
{
  Ogre::ManualObject * thisclass_cpp = static_cast<Ogre::ManualObject*> (getHG3DClassPtr(*thisclass_c, "Ogre::ManualObject"));
  bool dyn_cpp = (bool)dyn_c;
  (thisclass_cpp->setDynamic(dyn_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mno_getDynamic(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::ManualObject * thisclass_cpp = static_cast<Ogre::ManualObject*> (getHG3DClassPtr(*thisclass_c, "Ogre::ManualObject"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->getDynamic());
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mno_beginUpdate(struct hg3dclass_struct * thisclass_c, int sectionIndex_c)
{
  Ogre::ManualObject * thisclass_cpp = static_cast<Ogre::ManualObject*> (getHG3DClassPtr(*thisclass_c, "Ogre::ManualObject"));
  size_t sectionIndex_cpp = (size_t)sectionIndex_c;
  (thisclass_cpp->beginUpdate(sectionIndex_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mno_position(struct hg3dclass_struct * thisclass_c, struct vector3_struct * pos_c)
{
  Ogre::ManualObject * thisclass_cpp = static_cast<Ogre::ManualObject*> (getHG3DClassPtr(*thisclass_c, "Ogre::ManualObject"));
  Vector3 pos_cpp = *((Vector3*) pos_c);
  (thisclass_cpp->position(pos_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mno_position2(struct hg3dclass_struct * thisclass_c, float x_c, float y_c, float z_c)
{
  Ogre::ManualObject * thisclass_cpp = static_cast<Ogre::ManualObject*> (getHG3DClassPtr(*thisclass_c, "Ogre::ManualObject"));
  Real x_cpp = (Real)x_c;
  Real y_cpp = (Real)y_c;
  Real z_cpp = (Real)z_c;
  (thisclass_cpp->position(x_cpp, y_cpp, z_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mno_normal(struct hg3dclass_struct * thisclass_c, struct vector3_struct * norm_c)
{
  Ogre::ManualObject * thisclass_cpp = static_cast<Ogre::ManualObject*> (getHG3DClassPtr(*thisclass_c, "Ogre::ManualObject"));
  Vector3 norm_cpp = *((Vector3*) norm_c);
  (thisclass_cpp->normal(norm_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mno_normal2(struct hg3dclass_struct * thisclass_c, float x_c, float y_c, float z_c)
{
  Ogre::ManualObject * thisclass_cpp = static_cast<Ogre::ManualObject*> (getHG3DClassPtr(*thisclass_c, "Ogre::ManualObject"));
  Real x_cpp = (Real)x_c;
  Real y_cpp = (Real)y_c;
  Real z_cpp = (Real)z_c;
  (thisclass_cpp->normal(x_cpp, y_cpp, z_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mno_tangent(struct hg3dclass_struct * thisclass_c, struct vector3_struct * tan_c)
{
  Ogre::ManualObject * thisclass_cpp = static_cast<Ogre::ManualObject*> (getHG3DClassPtr(*thisclass_c, "Ogre::ManualObject"));
  Vector3 tan_cpp = *((Vector3*) tan_c);
  (thisclass_cpp->tangent(tan_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mno_tangent2(struct hg3dclass_struct * thisclass_c, float x_c, float y_c, float z_c)
{
  Ogre::ManualObject * thisclass_cpp = static_cast<Ogre::ManualObject*> (getHG3DClassPtr(*thisclass_c, "Ogre::ManualObject"));
  Real x_cpp = (Real)x_c;
  Real y_cpp = (Real)y_c;
  Real z_cpp = (Real)z_c;
  (thisclass_cpp->tangent(x_cpp, y_cpp, z_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mno_textureCoord(struct hg3dclass_struct * thisclass_c, float u_c)
{
  Ogre::ManualObject * thisclass_cpp = static_cast<Ogre::ManualObject*> (getHG3DClassPtr(*thisclass_c, "Ogre::ManualObject"));
  Real u_cpp = (Real)u_c;
  (thisclass_cpp->textureCoord(u_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mno_textureCoord2(struct hg3dclass_struct * thisclass_c, float u_c, float v_c)
{
  Ogre::ManualObject * thisclass_cpp = static_cast<Ogre::ManualObject*> (getHG3DClassPtr(*thisclass_c, "Ogre::ManualObject"));
  Real u_cpp = (Real)u_c;
  Real v_cpp = (Real)v_c;
  (thisclass_cpp->textureCoord(u_cpp, v_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mno_textureCoord3(struct hg3dclass_struct * thisclass_c, float u_c, float v_c, float w_c)
{
  Ogre::ManualObject * thisclass_cpp = static_cast<Ogre::ManualObject*> (getHG3DClassPtr(*thisclass_c, "Ogre::ManualObject"));
  Real u_cpp = (Real)u_c;
  Real v_cpp = (Real)v_c;
  Real w_cpp = (Real)w_c;
  (thisclass_cpp->textureCoord(u_cpp, v_cpp, w_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mno_textureCoord4(struct hg3dclass_struct * thisclass_c, float x_c, float y_c, float z_c, float w_c)
{
  Ogre::ManualObject * thisclass_cpp = static_cast<Ogre::ManualObject*> (getHG3DClassPtr(*thisclass_c, "Ogre::ManualObject"));
  Real x_cpp = (Real)x_c;
  Real y_cpp = (Real)y_c;
  Real z_cpp = (Real)z_c;
  Real w_cpp = (Real)w_c;
  (thisclass_cpp->textureCoord(x_cpp, y_cpp, z_cpp, w_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mno_textureCoord5(struct hg3dclass_struct * thisclass_c, struct vector2_struct * uv_c)
{
  Ogre::ManualObject * thisclass_cpp = static_cast<Ogre::ManualObject*> (getHG3DClassPtr(*thisclass_c, "Ogre::ManualObject"));
  Vector2 uv_cpp = *((Vector2*) uv_c);
  (thisclass_cpp->textureCoord(uv_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mno_textureCoord6(struct hg3dclass_struct * thisclass_c, struct vector3_struct * uvw_c)
{
  Ogre::ManualObject * thisclass_cpp = static_cast<Ogre::ManualObject*> (getHG3DClassPtr(*thisclass_c, "Ogre::ManualObject"));
  Vector3 uvw_cpp = *((Vector3*) uvw_c);
  (thisclass_cpp->textureCoord(uvw_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mno_colour(struct hg3dclass_struct * thisclass_c, struct colourvalue_struct * col_c)
{
  Ogre::ManualObject * thisclass_cpp = static_cast<Ogre::ManualObject*> (getHG3DClassPtr(*thisclass_c, "Ogre::ManualObject"));
  ColourValue col_cpp = *((ColourValue*) col_c);
  (thisclass_cpp->colour(col_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mno_colour2(struct hg3dclass_struct * thisclass_c, float r_c, float g_c, float b_c, float a_c)
{
  Ogre::ManualObject * thisclass_cpp = static_cast<Ogre::ManualObject*> (getHG3DClassPtr(*thisclass_c, "Ogre::ManualObject"));
  Real r_cpp = (Real)r_c;
  Real g_cpp = (Real)g_c;
  Real b_cpp = (Real)b_c;
  Real a_cpp = (Real)a_c;
  (thisclass_cpp->colour(r_cpp, g_cpp, b_cpp, a_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mno_index(struct hg3dclass_struct * thisclass_c, unsigned int idx_c)
{
  Ogre::ManualObject * thisclass_cpp = static_cast<Ogre::ManualObject*> (getHG3DClassPtr(*thisclass_c, "Ogre::ManualObject"));
  uint32 idx_cpp = (uint32)idx_c;
  (thisclass_cpp->index(idx_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mno_triangle(struct hg3dclass_struct * thisclass_c, unsigned int i1_c, unsigned int i2_c, unsigned int i3_c)
{
  Ogre::ManualObject * thisclass_cpp = static_cast<Ogre::ManualObject*> (getHG3DClassPtr(*thisclass_c, "Ogre::ManualObject"));
  uint32 i1_cpp = (uint32)i1_c;
  uint32 i2_cpp = (uint32)i2_c;
  uint32 i3_cpp = (uint32)i3_c;
  (thisclass_cpp->triangle(i1_cpp, i2_cpp, i3_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mno_quad(struct hg3dclass_struct * thisclass_c, unsigned int i1_c, unsigned int i2_c, unsigned int i3_c, unsigned int i4_c)
{
  Ogre::ManualObject * thisclass_cpp = static_cast<Ogre::ManualObject*> (getHG3DClassPtr(*thisclass_c, "Ogre::ManualObject"));
  uint32 i1_cpp = (uint32)i1_c;
  uint32 i2_cpp = (uint32)i2_c;
  uint32 i3_cpp = (uint32)i3_c;
  uint32 i4_cpp = (uint32)i4_c;
  (thisclass_cpp->quad(i1_cpp, i2_cpp, i3_cpp, i4_cpp));
};

// Get the number of vertices in the section currently being defined (returns 0 if no section is in progress). 
extern "C" Ogre_LIB_EXPORT void ogre_mno_getCurrentVertexCount(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::ManualObject * thisclass_cpp = static_cast<Ogre::ManualObject*> (getHG3DClassPtr(*thisclass_c, "Ogre::ManualObject"));
  size_t result_cpp;
  result_cpp = (thisclass_cpp->getCurrentVertexCount());
  *result_c = (int)result_cpp;
};

// Get the number of indices in the section currently being defined (returns 0 if no section is in progress). 
extern "C" Ogre_LIB_EXPORT void ogre_mno_getCurrentIndexCount(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::ManualObject * thisclass_cpp = static_cast<Ogre::ManualObject*> (getHG3DClassPtr(*thisclass_c, "Ogre::ManualObject"));
  size_t result_cpp;
  result_cpp = (thisclass_cpp->getCurrentIndexCount());
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mno_end(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  Ogre::ManualObject * thisclass_cpp = static_cast<Ogre::ManualObject*> (getHG3DClassPtr(*thisclass_c, "Ogre::ManualObject"));
  Ogre::ManualObject::ManualObjectSection * result_cpp;
  result_cpp = (thisclass_cpp->end());
  *result_c = getHG3DClass_ManualObjectSection((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mno_setMaterialName(struct hg3dclass_struct * thisclass_c, int subindex_c, char * name_c, char * group_c)
{
  Ogre::ManualObject * thisclass_cpp = static_cast<Ogre::ManualObject*> (getHG3DClassPtr(*thisclass_c, "Ogre::ManualObject"));
  size_t subindex_cpp = (size_t)subindex_c;
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  Ogre::String group_cpp = Ogre::String((const char*) group_c);
  (thisclass_cpp->setMaterialName(subindex_cpp, name_cpp, group_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mno_convertToMesh(struct hg3dclass_struct * thisclass_c, char * meshName_c, char * groupName_c, struct sharedptr_struct * result_c)
{
  Ogre::ManualObject * thisclass_cpp = static_cast<Ogre::ManualObject*> (getHG3DClassPtr(*thisclass_c, "Ogre::ManualObject"));
  Ogre::String meshName_cpp = Ogre::String((const char*) meshName_c);
  Ogre::String groupName_cpp = Ogre::String((const char*) groupName_c);
  MeshPtr result_cpp;
  result_cpp = (thisclass_cpp->convertToMesh(meshName_cpp, groupName_cpp));
  *result_c = *((struct sharedptr_struct*) &result_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mno_setUseIdentityProjection(struct hg3dclass_struct * thisclass_c, int useIdentityProjection_c)
{
  Ogre::ManualObject * thisclass_cpp = static_cast<Ogre::ManualObject*> (getHG3DClassPtr(*thisclass_c, "Ogre::ManualObject"));
  bool useIdentityProjection_cpp = (bool)useIdentityProjection_c;
  (thisclass_cpp->setUseIdentityProjection(useIdentityProjection_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mno_getUseIdentityProjection(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::ManualObject * thisclass_cpp = static_cast<Ogre::ManualObject*> (getHG3DClassPtr(*thisclass_c, "Ogre::ManualObject"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->getUseIdentityProjection());
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mno_setUseIdentityView(struct hg3dclass_struct * thisclass_c, int useIdentityView_c)
{
  Ogre::ManualObject * thisclass_cpp = static_cast<Ogre::ManualObject*> (getHG3DClassPtr(*thisclass_c, "Ogre::ManualObject"));
  bool useIdentityView_cpp = (bool)useIdentityView_c;
  (thisclass_cpp->setUseIdentityView(useIdentityView_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mno_getUseIdentityView(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::ManualObject * thisclass_cpp = static_cast<Ogre::ManualObject*> (getHG3DClassPtr(*thisclass_c, "Ogre::ManualObject"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->getUseIdentityView());
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mno_getSection(struct hg3dclass_struct * thisclass_c, unsigned int index_c, struct hg3dclass_struct * result_c)
{
  Ogre::ManualObject * thisclass_cpp = static_cast<Ogre::ManualObject*> (getHG3DClassPtr(*thisclass_c, "Ogre::ManualObject"));
  unsigned int index_cpp = (unsigned int)index_c;
  Ogre::ManualObject::ManualObjectSection * result_cpp;
  result_cpp = (thisclass_cpp->getSection(index_cpp));
  *result_c = getHG3DClass_ManualObjectSection((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mno_getNumSections(struct hg3dclass_struct * thisclass_c, unsigned int * result_c)
{
  Ogre::ManualObject * thisclass_cpp = static_cast<Ogre::ManualObject*> (getHG3DClassPtr(*thisclass_c, "Ogre::ManualObject"));
  unsigned int result_cpp;
  result_cpp = (thisclass_cpp->getNumSections());
  *result_c = (unsigned int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mno_setKeepDeclarationOrder(struct hg3dclass_struct * thisclass_c, int keepOrder_c)
{
  Ogre::ManualObject * thisclass_cpp = static_cast<Ogre::ManualObject*> (getHG3DClassPtr(*thisclass_c, "Ogre::ManualObject"));
  bool keepOrder_cpp = (bool)keepOrder_c;
  (thisclass_cpp->setKeepDeclarationOrder(keepOrder_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mno_getKeepDeclarationOrder(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::ManualObject * thisclass_cpp = static_cast<Ogre::ManualObject*> (getHG3DClassPtr(*thisclass_c, "Ogre::ManualObject"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->getKeepDeclarationOrder());
  *result_c = (int)result_cpp;
};

// . 
extern "C" Ogre_LIB_EXPORT void ogre_mno_getMovableType(struct hg3dclass_struct * thisclass_c, char * result_c)
{
  Ogre::ManualObject * thisclass_cpp = static_cast<Ogre::ManualObject*> (getHG3DClassPtr(*thisclass_c, "Ogre::ManualObject"));
  Ogre::String result_cpp;
  result_cpp = (thisclass_cpp->getMovableType());
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

// . 
extern "C" Ogre_LIB_EXPORT void ogre_mno_getBoundingRadius(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  Ogre::ManualObject * thisclass_cpp = static_cast<Ogre::ManualObject*> (getHG3DClassPtr(*thisclass_c, "Ogre::ManualObject"));
  Real result_cpp;
  result_cpp = (thisclass_cpp->getBoundingRadius());
  *result_c = (float)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mno_hasEdgeList(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::ManualObject * thisclass_cpp = static_cast<Ogre::ManualObject*> (getHG3DClassPtr(*thisclass_c, "Ogre::ManualObject"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->hasEdgeList());
  *result_c = (int)result_cpp;
};

