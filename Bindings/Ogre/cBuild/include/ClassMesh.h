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

// ClassMesh.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassMesh
#define _DEFINED_HG3D_ClassMesh

#include "ClassPtr.h"
#include "StructSharedPtr.h"
#include "EnumVertexAnimationType.h"
#include "ClassAnimation.h"


// 
void ogre_msh_destruct(struct hg3dclass_struct * thisclass_c);

// 
void ogre_msh_unnameSubMesh(struct hg3dclass_struct * thisclass_c, char * name_c);

// 
void ogre_msh_getNumSubMeshes(struct hg3dclass_struct * thisclass_c, unsigned short * result_c);

// 
void ogre_msh_destroySubMesh(struct hg3dclass_struct * thisclass_c, unsigned short index_c);

// 
void ogre_msh_destroySubMesh2(struct hg3dclass_struct * thisclass_c, char * name_c);

// 
void ogre_msh_clone(struct hg3dclass_struct * thisclass_c, char * newName_c, char * newGroup_c, struct sharedptr_struct * result_c);

// 
void ogre_msh_getBoundingSphereRadius(struct hg3dclass_struct * thisclass_c, float * result_c);

// 
void ogre_msh_setSkeletonName(struct hg3dclass_struct * thisclass_c, char * skelName_c);

// 
void ogre_msh_hasSkeleton(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_msh_hasVertexAnimation(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_msh_getSkeleton(struct hg3dclass_struct * thisclass_c, struct sharedptr_struct * result_c);

// 
void ogre_msh_getSkeletonName(struct hg3dclass_struct * thisclass_c, char * result_c);

// 
void ogre_msh_clearBoneAssignments(struct hg3dclass_struct * thisclass_c);

// 
void ogre_msh_createManualLodLevel(struct hg3dclass_struct * thisclass_c, float value_c, char * meshName_c, char * groupName_c);

// 
void ogre_msh_isLodManual(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_msh_removeLodLevels(struct hg3dclass_struct * thisclass_c);

// 
void ogre_msh_isVertexBufferShadowed(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_msh_isIndexBufferShadowed(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_msh_buildEdgeList(struct hg3dclass_struct * thisclass_c);

// 
void ogre_msh_freeEdgeList(struct hg3dclass_struct * thisclass_c);

// 
void ogre_msh_prepareForShadowVolume(struct hg3dclass_struct * thisclass_c);

// 
void ogre_msh_isPreparedForShadowVolumes(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_msh_isEdgeListBuilt(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_msh_setAutoBuildEdgeLists(struct hg3dclass_struct * thisclass_c, long autobuild_c);

// 
void ogre_msh_getAutoBuildEdgeLists(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_msh_getSharedVertexDataAnimationType(struct hg3dclass_struct * thisclass_c, enum EnumVertexAnimationType * result_c);

// Returns whether animation on shared vertex data includes normals. 
void ogre_msh_getSharedVertexDataAnimationIncludesNormals(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_msh_createAnimation(struct hg3dclass_struct * thisclass_c, char * name_c, float length_c, struct hg3dclass_struct * result_c);

// 
void ogre_msh_getAnimation(struct hg3dclass_struct * thisclass_c, char * name_c, struct hg3dclass_struct * result_c);

// 
void ogre_msh_hasAnimation(struct hg3dclass_struct * thisclass_c, char * name_c, long * result_c);

// 
void ogre_msh_removeAnimation(struct hg3dclass_struct * thisclass_c, char * name_c);

// 
void ogre_msh_getNumAnimations(struct hg3dclass_struct * thisclass_c, unsigned short * result_c);

// 
void ogre_msh_getAnimation2(struct hg3dclass_struct * thisclass_c, unsigned short index_c, struct hg3dclass_struct * result_c);

// 
void ogre_msh_removeAllAnimations(struct hg3dclass_struct * thisclass_c);

// 
void ogre_msh_updateMaterialForAllSubMeshes(struct hg3dclass_struct * thisclass_c);

// 
void ogre_msh_getPoseCount(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_msh_removePose2(struct hg3dclass_struct * thisclass_c, char * name_c);

// 
void ogre_msh_removeAllPoses(struct hg3dclass_struct * thisclass_c);

#endif 
