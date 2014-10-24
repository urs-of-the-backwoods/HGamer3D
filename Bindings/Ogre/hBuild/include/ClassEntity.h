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

// ClassEntity.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassEntity
#define _DEFINED_HG3D_ClassEntity

#include "ClassPtr.h"
#include "StructSharedPtr.h"
#include "ClassAnimationState.h"
#include "ClassAnimationStateSet.h"
#include "ClassMovableObject.h"
#include "EnumEntityVertexDataBindChoice.h"
#include "ClassResource.h"


// 
void ogre_ent_destruct(struct hg3dclass_struct * thisclass_c);

// 
void ogre_ent_getMesh(struct hg3dclass_struct * thisclass_c, struct sharedptr_struct * result_c);

// 
void ogre_ent_getNumSubEntities(struct hg3dclass_struct * thisclass_c, unsigned long * result_c);

// 
void ogre_ent_clone(struct hg3dclass_struct * thisclass_c, char * newName_c, struct hg3dclass_struct * result_c);

// 
void ogre_ent_setMaterialName(struct hg3dclass_struct * thisclass_c, char * name_c, char * groupName_c);

// 
void ogre_ent_setMaterial(struct hg3dclass_struct * thisclass_c, struct sharedptr_struct * material_c);

// 
void ogre_ent_getMovableType(struct hg3dclass_struct * thisclass_c, char * result_c);

// 
void ogre_ent_getAnimationState(struct hg3dclass_struct * thisclass_c, char * name_c, struct hg3dclass_struct * result_c);

// 
void ogre_ent_hasAnimationState(struct hg3dclass_struct * thisclass_c, char * name_c, long * result_c);

// 
void ogre_ent_getAllAnimationStates(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// 
void ogre_ent_setDisplaySkeleton(struct hg3dclass_struct * thisclass_c, long display_c);

// 
void ogre_ent_getDisplaySkeleton(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_ent_getManualLodLevel(struct hg3dclass_struct * thisclass_c, long index_c, struct hg3dclass_struct * result_c);

// 
void ogre_ent_getNumManualLodLevels(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_ent_setPolygonModeOverrideable(struct hg3dclass_struct * thisclass_c, long PolygonModeOverrideable_c);

// 
void ogre_ent_detachObjectFromBone(struct hg3dclass_struct * thisclass_c, char * movableName_c, struct hg3dclass_struct * result_c);

// 
void ogre_ent_detachObjectFromBone2(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * obj_c);

// Detach all MovableObjects previously attached using attachObjectToBone. 
void ogre_ent_detachAllObjectsFromBone(struct hg3dclass_struct * thisclass_c);

// 
void ogre_ent_getBoundingRadius(struct hg3dclass_struct * thisclass_c, float * result_c);

// 
void ogre_ent_hasEdgeList(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_ent_hasSkeleton(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_ent_isHardwareAnimationEnabled(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_ent_getSoftwareAnimationRequests(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_ent_getSoftwareAnimationNormalsRequests(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_ent_addSoftwareAnimationRequest(struct hg3dclass_struct * thisclass_c, long normalsAlso_c);

// 
void ogre_ent_removeSoftwareAnimationRequest(struct hg3dclass_struct * thisclass_c, long normalsAlso_c);

// 
void ogre_ent_shareSkeletonInstanceWith(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * entity_c);

// 
void ogre_ent_hasVertexAnimation(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_ent_stopSharingSkeletonInstance(struct hg3dclass_struct * thisclass_c);

// 
void ogre_ent_sharesSkeletonInstance(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_ent_refreshAvailableAnimationState(struct hg3dclass_struct * thisclass_c);

// Choose which vertex data to bind to the renderer. 
void ogre_ent_chooseVertexDataForBinding(struct hg3dclass_struct * thisclass_c, long hasVertexAnim_c, enum EnumEntityVertexDataBindChoice * result_c);

// 
void ogre_ent_isInitialised(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_ent_backgroundLoadingComplete(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * res_c);

// 
void ogre_ent_setSkipAnimationStateUpdate(struct hg3dclass_struct * thisclass_c, long skip_c);

// 
void ogre_ent_getSkipAnimationStateUpdate(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_ent_setAlwaysUpdateMainSkeleton(struct hg3dclass_struct * thisclass_c, long update_c);

// 
void ogre_ent_getAlwaysUpdateMainSkeleton(struct hg3dclass_struct * thisclass_c, long * result_c);

#endif 
