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

// ClassMesh.cpp

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
	#include "StructSharedPtr.h"
#include "EnumVertexAnimationType.h"
#include "./Ogre.h"
#include "./OgreString.h"
#include "./WindowUtilsHG3D.h"
#include "HG3DUtilities.h"

using namespace Ogre;
using Ogre::uint;



// 
extern "C" Ogre_LIB_EXPORT void ogre_msh_destruct(struct hg3dclass_struct * thisclass_c)
{
  Ogre::Mesh * thisclass_cpp = static_cast<Ogre::Mesh*> (getHG3DClassPtr(*thisclass_c, "Ogre::Mesh"));
  (delete thisclass_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_msh_unnameSubMesh(struct hg3dclass_struct * thisclass_c, char * name_c)
{
  Ogre::Mesh * thisclass_cpp = static_cast<Ogre::Mesh*> (getHG3DClassPtr(*thisclass_c, "Ogre::Mesh"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  (thisclass_cpp->unnameSubMesh(name_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_msh_getNumSubMeshes(struct hg3dclass_struct * thisclass_c, unsigned short * result_c)
{
  Ogre::Mesh * thisclass_cpp = static_cast<Ogre::Mesh*> (getHG3DClassPtr(*thisclass_c, "Ogre::Mesh"));
  unsigned short result_cpp;
  result_cpp = (thisclass_cpp->getNumSubMeshes());
  *result_c = (unsigned short)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_msh_destroySubMesh(struct hg3dclass_struct * thisclass_c, unsigned short index_c)
{
  Ogre::Mesh * thisclass_cpp = static_cast<Ogre::Mesh*> (getHG3DClassPtr(*thisclass_c, "Ogre::Mesh"));
  unsigned short index_cpp = (unsigned short)index_c;
  (thisclass_cpp->destroySubMesh(index_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_msh_destroySubMesh2(struct hg3dclass_struct * thisclass_c, char * name_c)
{
  Ogre::Mesh * thisclass_cpp = static_cast<Ogre::Mesh*> (getHG3DClassPtr(*thisclass_c, "Ogre::Mesh"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  (thisclass_cpp->destroySubMesh(name_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_msh_clone(struct hg3dclass_struct * thisclass_c, char * newName_c, char * newGroup_c, struct sharedptr_struct * result_c)
{
  Ogre::Mesh * thisclass_cpp = static_cast<Ogre::Mesh*> (getHG3DClassPtr(*thisclass_c, "Ogre::Mesh"));
  Ogre::String newName_cpp = Ogre::String((const char*) newName_c);
  Ogre::String newGroup_cpp = Ogre::String((const char*) newGroup_c);
  MeshPtr result_cpp;
  result_cpp = (thisclass_cpp->clone(newName_cpp, newGroup_cpp));
  *result_c = *((struct sharedptr_struct*) &result_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_msh_getBoundingSphereRadius(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  Ogre::Mesh * thisclass_cpp = static_cast<Ogre::Mesh*> (getHG3DClassPtr(*thisclass_c, "Ogre::Mesh"));
  Real result_cpp;
  result_cpp = (thisclass_cpp->getBoundingSphereRadius());
  *result_c = (float)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_msh_setSkeletonName(struct hg3dclass_struct * thisclass_c, char * skelName_c)
{
  Ogre::Mesh * thisclass_cpp = static_cast<Ogre::Mesh*> (getHG3DClassPtr(*thisclass_c, "Ogre::Mesh"));
  Ogre::String skelName_cpp = Ogre::String((const char*) skelName_c);
  (thisclass_cpp->setSkeletonName(skelName_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_msh_hasSkeleton(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::Mesh * thisclass_cpp = static_cast<Ogre::Mesh*> (getHG3DClassPtr(*thisclass_c, "Ogre::Mesh"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->hasSkeleton());
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_msh_hasVertexAnimation(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::Mesh * thisclass_cpp = static_cast<Ogre::Mesh*> (getHG3DClassPtr(*thisclass_c, "Ogre::Mesh"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->hasVertexAnimation());
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_msh_getSkeleton(struct hg3dclass_struct * thisclass_c, struct sharedptr_struct * result_c)
{
  Ogre::Mesh * thisclass_cpp = static_cast<Ogre::Mesh*> (getHG3DClassPtr(*thisclass_c, "Ogre::Mesh"));
  SkeletonPtr result_cpp;
  result_cpp = (thisclass_cpp->getSkeleton());
  *result_c = *((struct sharedptr_struct*) &result_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_msh_getSkeletonName(struct hg3dclass_struct * thisclass_c, char * result_c)
{
  Ogre::Mesh * thisclass_cpp = static_cast<Ogre::Mesh*> (getHG3DClassPtr(*thisclass_c, "Ogre::Mesh"));
  Ogre::String result_cpp;
  result_cpp = (thisclass_cpp->getSkeletonName());
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_msh_clearBoneAssignments(struct hg3dclass_struct * thisclass_c)
{
  Ogre::Mesh * thisclass_cpp = static_cast<Ogre::Mesh*> (getHG3DClassPtr(*thisclass_c, "Ogre::Mesh"));
  (thisclass_cpp->clearBoneAssignments());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_msh_createManualLodLevel(struct hg3dclass_struct * thisclass_c, float value_c, char * meshName_c, char * groupName_c)
{
  Ogre::Mesh * thisclass_cpp = static_cast<Ogre::Mesh*> (getHG3DClassPtr(*thisclass_c, "Ogre::Mesh"));
  Real value_cpp = (Real)value_c;
  Ogre::String meshName_cpp = Ogre::String((const char*) meshName_c);
  Ogre::String groupName_cpp = Ogre::String((const char*) groupName_c);
  (thisclass_cpp->createManualLodLevel(value_cpp, meshName_cpp, groupName_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_msh_isLodManual(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::Mesh * thisclass_cpp = static_cast<Ogre::Mesh*> (getHG3DClassPtr(*thisclass_c, "Ogre::Mesh"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isLodManual());
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_msh_removeLodLevels(struct hg3dclass_struct * thisclass_c)
{
  Ogre::Mesh * thisclass_cpp = static_cast<Ogre::Mesh*> (getHG3DClassPtr(*thisclass_c, "Ogre::Mesh"));
  (thisclass_cpp->removeLodLevels());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_msh_isVertexBufferShadowed(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::Mesh * thisclass_cpp = static_cast<Ogre::Mesh*> (getHG3DClassPtr(*thisclass_c, "Ogre::Mesh"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isVertexBufferShadowed());
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_msh_isIndexBufferShadowed(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::Mesh * thisclass_cpp = static_cast<Ogre::Mesh*> (getHG3DClassPtr(*thisclass_c, "Ogre::Mesh"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isIndexBufferShadowed());
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_msh_buildEdgeList(struct hg3dclass_struct * thisclass_c)
{
  Ogre::Mesh * thisclass_cpp = static_cast<Ogre::Mesh*> (getHG3DClassPtr(*thisclass_c, "Ogre::Mesh"));
  (thisclass_cpp->buildEdgeList());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_msh_freeEdgeList(struct hg3dclass_struct * thisclass_c)
{
  Ogre::Mesh * thisclass_cpp = static_cast<Ogre::Mesh*> (getHG3DClassPtr(*thisclass_c, "Ogre::Mesh"));
  (thisclass_cpp->freeEdgeList());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_msh_prepareForShadowVolume(struct hg3dclass_struct * thisclass_c)
{
  Ogre::Mesh * thisclass_cpp = static_cast<Ogre::Mesh*> (getHG3DClassPtr(*thisclass_c, "Ogre::Mesh"));
  (thisclass_cpp->prepareForShadowVolume());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_msh_isPreparedForShadowVolumes(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::Mesh * thisclass_cpp = static_cast<Ogre::Mesh*> (getHG3DClassPtr(*thisclass_c, "Ogre::Mesh"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isPreparedForShadowVolumes());
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_msh_isEdgeListBuilt(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::Mesh * thisclass_cpp = static_cast<Ogre::Mesh*> (getHG3DClassPtr(*thisclass_c, "Ogre::Mesh"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isEdgeListBuilt());
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_msh_setAutoBuildEdgeLists(struct hg3dclass_struct * thisclass_c, long autobuild_c)
{
  Ogre::Mesh * thisclass_cpp = static_cast<Ogre::Mesh*> (getHG3DClassPtr(*thisclass_c, "Ogre::Mesh"));
  bool autobuild_cpp = (bool)autobuild_c;
  (thisclass_cpp->setAutoBuildEdgeLists(autobuild_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_msh_getAutoBuildEdgeLists(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::Mesh * thisclass_cpp = static_cast<Ogre::Mesh*> (getHG3DClassPtr(*thisclass_c, "Ogre::Mesh"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->getAutoBuildEdgeLists());
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_msh_getSharedVertexDataAnimationType(struct hg3dclass_struct * thisclass_c, enum EnumVertexAnimationType * result_c)
{
  Ogre::Mesh * thisclass_cpp = static_cast<Ogre::Mesh*> (getHG3DClassPtr(*thisclass_c, "Ogre::Mesh"));
  enum Ogre::VertexAnimationType result_cpp;
  result_cpp = (thisclass_cpp->getSharedVertexDataAnimationType());
  *result_c = (enum EnumVertexAnimationType) result_cpp;
};

// Returns whether animation on shared vertex data includes normals. 
extern "C" Ogre_LIB_EXPORT void ogre_msh_getSharedVertexDataAnimationIncludesNormals(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::Mesh * thisclass_cpp = static_cast<Ogre::Mesh*> (getHG3DClassPtr(*thisclass_c, "Ogre::Mesh"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->getSharedVertexDataAnimationIncludesNormals());
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_msh_createAnimation(struct hg3dclass_struct * thisclass_c, char * name_c, float length_c, struct hg3dclass_struct * result_c)
{
  Ogre::Mesh * thisclass_cpp = static_cast<Ogre::Mesh*> (getHG3DClassPtr(*thisclass_c, "Ogre::Mesh"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  Real length_cpp = (Real)length_c;
  Ogre::Animation * result_cpp;
  result_cpp = (thisclass_cpp->createAnimation(name_cpp, length_cpp));
  *result_c = getHG3DClass_Animation((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_msh_getAnimation(struct hg3dclass_struct * thisclass_c, char * name_c, struct hg3dclass_struct * result_c)
{
  Ogre::Mesh * thisclass_cpp = static_cast<Ogre::Mesh*> (getHG3DClassPtr(*thisclass_c, "Ogre::Mesh"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  Ogre::Animation * result_cpp;
  result_cpp = (thisclass_cpp->getAnimation(name_cpp));
  *result_c = getHG3DClass_Animation((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_msh_hasAnimation(struct hg3dclass_struct * thisclass_c, char * name_c, long * result_c)
{
  Ogre::Mesh * thisclass_cpp = static_cast<Ogre::Mesh*> (getHG3DClassPtr(*thisclass_c, "Ogre::Mesh"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  bool result_cpp;
  result_cpp = (thisclass_cpp->hasAnimation(name_cpp));
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_msh_removeAnimation(struct hg3dclass_struct * thisclass_c, char * name_c)
{
  Ogre::Mesh * thisclass_cpp = static_cast<Ogre::Mesh*> (getHG3DClassPtr(*thisclass_c, "Ogre::Mesh"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  (thisclass_cpp->removeAnimation(name_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_msh_getNumAnimations(struct hg3dclass_struct * thisclass_c, unsigned short * result_c)
{
  Ogre::Mesh * thisclass_cpp = static_cast<Ogre::Mesh*> (getHG3DClassPtr(*thisclass_c, "Ogre::Mesh"));
  unsigned short result_cpp;
  result_cpp = (thisclass_cpp->getNumAnimations());
  *result_c = (unsigned short)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_msh_getAnimation2(struct hg3dclass_struct * thisclass_c, unsigned short index_c, struct hg3dclass_struct * result_c)
{
  Ogre::Mesh * thisclass_cpp = static_cast<Ogre::Mesh*> (getHG3DClassPtr(*thisclass_c, "Ogre::Mesh"));
  unsigned short index_cpp = (unsigned short)index_c;
  Ogre::Animation * result_cpp;
  result_cpp = (thisclass_cpp->getAnimation(index_cpp));
  *result_c = getHG3DClass_Animation((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_msh_removeAllAnimations(struct hg3dclass_struct * thisclass_c)
{
  Ogre::Mesh * thisclass_cpp = static_cast<Ogre::Mesh*> (getHG3DClassPtr(*thisclass_c, "Ogre::Mesh"));
  (thisclass_cpp->removeAllAnimations());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_msh_updateMaterialForAllSubMeshes(struct hg3dclass_struct * thisclass_c)
{
  Ogre::Mesh * thisclass_cpp = static_cast<Ogre::Mesh*> (getHG3DClassPtr(*thisclass_c, "Ogre::Mesh"));
  (thisclass_cpp->updateMaterialForAllSubMeshes());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_msh_getPoseCount(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::Mesh * thisclass_cpp = static_cast<Ogre::Mesh*> (getHG3DClassPtr(*thisclass_c, "Ogre::Mesh"));
  size_t result_cpp;
  result_cpp = (thisclass_cpp->getPoseCount());
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_msh_removePose2(struct hg3dclass_struct * thisclass_c, char * name_c)
{
  Ogre::Mesh * thisclass_cpp = static_cast<Ogre::Mesh*> (getHG3DClassPtr(*thisclass_c, "Ogre::Mesh"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  (thisclass_cpp->removePose(name_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_msh_removeAllPoses(struct hg3dclass_struct * thisclass_c)
{
  Ogre::Mesh * thisclass_cpp = static_cast<Ogre::Mesh*> (getHG3DClassPtr(*thisclass_c, "Ogre::Mesh"));
  (thisclass_cpp->removeAllPoses());
};

