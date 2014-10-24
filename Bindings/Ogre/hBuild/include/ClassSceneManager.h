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

// ClassSceneManager.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassSceneManager
#define _DEFINED_HG3D_ClassSceneManager

#include "ClassPtr.h"
#include "ClassCamera.h"
#include "ClassLight.h"
#include "ClassSceneNode.h"
#include "ClassEntity.h"
#include "StructSharedPtr.h"
#include "EnumSceneManagerPrefabType.h"
#include "ClassManualObject.h"
#include "ClassBillboardChain.h"
#include "StructColour.h"
#include "StructQuaternion.h"
#include "ClassBillboardSet.h"
#include "ClassAnimation.h"
#include "ClassAnimationState.h"
#include "EnumSceneManagerSpecialCaseRenderQueueMode.h"
#include "EnumLightType.h"
#include "ClassMovableObject.h"
#include "ClassRenderSystem.h"
#include "ClassViewport.h"


// 
void ogre_scmgr_destruct(struct hg3dclass_struct * thisclass_c);

// 
void ogre_scmgr_getName(struct hg3dclass_struct * thisclass_c, char * result_c);

// 
void ogre_scmgr_getTypeName(struct hg3dclass_struct * thisclass_c, char * result_c);

// 
void ogre_scmgr_createCamera(struct hg3dclass_struct * thisclass_c, char * name_c, struct hg3dclass_struct * result_c);

// 
void ogre_scmgr_getCamera(struct hg3dclass_struct * thisclass_c, char * name_c, struct hg3dclass_struct * result_c);

// 
void ogre_scmgr_hasCamera(struct hg3dclass_struct * thisclass_c, char * name_c, long * result_c);

// 
void ogre_scmgr_destroyCamera(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * cam_c);

// 
void ogre_scmgr_destroyCamera2(struct hg3dclass_struct * thisclass_c, char * name_c);

// 
void ogre_scmgr_destroyAllCameras(struct hg3dclass_struct * thisclass_c);

// 
void ogre_scmgr_createLight(struct hg3dclass_struct * thisclass_c, char * name_c, struct hg3dclass_struct * result_c);

// 
void ogre_scmgr_createLight2(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// 
void ogre_scmgr_getLight(struct hg3dclass_struct * thisclass_c, char * name_c, struct hg3dclass_struct * result_c);

// 
void ogre_scmgr_hasLight(struct hg3dclass_struct * thisclass_c, char * name_c, long * result_c);

// 
void ogre_scmgr_destroyLight(struct hg3dclass_struct * thisclass_c, char * name_c);

// 
void ogre_scmgr_destroyLight2(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * light_c);

// 
void ogre_scmgr_destroyAllLights(struct hg3dclass_struct * thisclass_c);

// 
void ogre_scmgr_createSceneNode(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// 
void ogre_scmgr_createSceneNode2(struct hg3dclass_struct * thisclass_c, char * name_c, struct hg3dclass_struct * result_c);

// 
void ogre_scmgr_destroySceneNode(struct hg3dclass_struct * thisclass_c, char * name_c);

// 
void ogre_scmgr_destroySceneNode2(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * sn_c);

// 
void ogre_scmgr_getRootSceneNode(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// 
void ogre_scmgr_getSceneNode(struct hg3dclass_struct * thisclass_c, char * name_c, struct hg3dclass_struct * result_c);

// 
void ogre_scmgr_hasSceneNode(struct hg3dclass_struct * thisclass_c, char * name_c, long * result_c);

// 
void ogre_scmgr_createEntity(struct hg3dclass_struct * thisclass_c, char * entityName_c, char * meshName_c, char * groupName_c, struct hg3dclass_struct * result_c);

// 
void ogre_scmgr_createEntity2(struct hg3dclass_struct * thisclass_c, char * entityName_c, struct sharedptr_struct * pMesh_c, struct hg3dclass_struct * result_c);

// 
void ogre_scmgr_createEntity3(struct hg3dclass_struct * thisclass_c, char * meshName_c, struct hg3dclass_struct * result_c);

// 
void ogre_scmgr_createEntity4(struct hg3dclass_struct * thisclass_c, struct sharedptr_struct * pMesh_c, struct hg3dclass_struct * result_c);

// 
void ogre_scmgr_createEntity5(struct hg3dclass_struct * thisclass_c, char * entityName_c, enum EnumSceneManagerPrefabType ptype_c, struct hg3dclass_struct * result_c);

// 
void ogre_scmgr_createEntity6(struct hg3dclass_struct * thisclass_c, enum EnumSceneManagerPrefabType ptype_c, struct hg3dclass_struct * result_c);

// 
void ogre_scmgr_getEntity(struct hg3dclass_struct * thisclass_c, char * name_c, struct hg3dclass_struct * result_c);

// 
void ogre_scmgr_hasEntity(struct hg3dclass_struct * thisclass_c, char * name_c, long * result_c);

// 
void ogre_scmgr_destroyEntity(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * ent_c);

// 
void ogre_scmgr_destroyEntity2(struct hg3dclass_struct * thisclass_c, char * name_c);

// 
void ogre_scmgr_destroyAllEntities(struct hg3dclass_struct * thisclass_c);

// 
void ogre_scmgr_createManualObject(struct hg3dclass_struct * thisclass_c, char * name_c, struct hg3dclass_struct * result_c);

// 
void ogre_scmgr_createManualObject2(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// 
void ogre_scmgr_getManualObject(struct hg3dclass_struct * thisclass_c, char * name_c, struct hg3dclass_struct * result_c);

// 
void ogre_scmgr_hasManualObject(struct hg3dclass_struct * thisclass_c, char * name_c, long * result_c);

// 
void ogre_scmgr_destroyManualObject(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * obj_c);

// 
void ogre_scmgr_destroyManualObject2(struct hg3dclass_struct * thisclass_c, char * name_c);

// 
void ogre_scmgr_destroyAllManualObjects(struct hg3dclass_struct * thisclass_c);

// 
void ogre_scmgr_createBillboardChain(struct hg3dclass_struct * thisclass_c, char * name_c, struct hg3dclass_struct * result_c);

// 
void ogre_scmgr_createBillboardChain2(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// 
void ogre_scmgr_getBillboardChain(struct hg3dclass_struct * thisclass_c, char * name_c, struct hg3dclass_struct * result_c);

// 
void ogre_scmgr_hasBillboardChain(struct hg3dclass_struct * thisclass_c, char * name_c, long * result_c);

// 
void ogre_scmgr_destroyBillboardChain(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * obj_c);

// 
void ogre_scmgr_destroyBillboardChain2(struct hg3dclass_struct * thisclass_c, char * name_c);

// 
void ogre_scmgr_destroyAllBillboardChains(struct hg3dclass_struct * thisclass_c);

// 
void ogre_scmgr_hasRibbonTrail(struct hg3dclass_struct * thisclass_c, char * name_c, long * result_c);

// 
void ogre_scmgr_destroyRibbonTrail2(struct hg3dclass_struct * thisclass_c, char * name_c);

// 
void ogre_scmgr_destroyAllRibbonTrails(struct hg3dclass_struct * thisclass_c);

// 
void ogre_scmgr_hasParticleSystem(struct hg3dclass_struct * thisclass_c, char * name_c, long * result_c);

// 
void ogre_scmgr_destroyParticleSystem2(struct hg3dclass_struct * thisclass_c, char * name_c);

// 
void ogre_scmgr_destroyAllParticleSystems(struct hg3dclass_struct * thisclass_c);

// 
void ogre_scmgr_clearScene(struct hg3dclass_struct * thisclass_c);

// 
void ogre_scmgr_setAmbientLight(struct hg3dclass_struct * thisclass_c, struct colourvalue_struct * colour_c);

// 
void ogre_scmgr_getAmbientLight(struct hg3dclass_struct * thisclass_c, struct colourvalue_struct * result_c);

// 
void ogre_scmgr_prepareWorldGeometry(struct hg3dclass_struct * thisclass_c, char * filename_c);

// 
void ogre_scmgr_setWorldGeometry(struct hg3dclass_struct * thisclass_c, char * filename_c);

// 
void ogre_scmgr_estimateWorldGeometry(struct hg3dclass_struct * thisclass_c, char * filename_c, long * result_c);

// 
void ogre_scmgr_hasOption(struct hg3dclass_struct * thisclass_c, char * strKey_c, long * result_c);

// 
void ogre_scmgr_setSkyPlaneEnabled(struct hg3dclass_struct * thisclass_c, long enable_c);

// 
void ogre_scmgr_isSkyPlaneEnabled(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_scmgr_getSkyPlaneNode(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// 
void ogre_scmgr_setSkyBox(struct hg3dclass_struct * thisclass_c, long enable_c, char * materialName_c, float distance_c, long drawFirst_c, struct quaternion_struct * orientation_c, char * groupName_c);

// 
void ogre_scmgr_setSkyBoxEnabled(struct hg3dclass_struct * thisclass_c, long enable_c);

// 
void ogre_scmgr_isSkyBoxEnabled(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_scmgr_getSkyBoxNode(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// 
void ogre_scmgr_setSkyDome(struct hg3dclass_struct * thisclass_c, long enable_c, char * materialName_c, float curvature_c, float tiling_c, float distance_c, long drawFirst_c, struct quaternion_struct * orientation_c, long xsegments_c, long ysegments_c, long ysegments_keep_c, char * groupName_c);

// 
void ogre_scmgr_setSkyDomeEnabled(struct hg3dclass_struct * thisclass_c, long enable_c);

// 
void ogre_scmgr_isSkyDomeEnabled(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_scmgr_getSkyDomeNode(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// 
void ogre_scmgr_getFogColour(struct hg3dclass_struct * thisclass_c, struct colourvalue_struct * result_c);

// 
void ogre_scmgr_getFogStart(struct hg3dclass_struct * thisclass_c, float * result_c);

// 
void ogre_scmgr_getFogEnd(struct hg3dclass_struct * thisclass_c, float * result_c);

// 
void ogre_scmgr_getFogDensity(struct hg3dclass_struct * thisclass_c, float * result_c);

// 
void ogre_scmgr_createBillboardSet(struct hg3dclass_struct * thisclass_c, char * name_c, unsigned long poolSize_c, struct hg3dclass_struct * result_c);

// 
void ogre_scmgr_createBillboardSet2(struct hg3dclass_struct * thisclass_c, unsigned long poolSize_c, struct hg3dclass_struct * result_c);

// 
void ogre_scmgr_getBillboardSet(struct hg3dclass_struct * thisclass_c, char * name_c, struct hg3dclass_struct * result_c);

// 
void ogre_scmgr_hasBillboardSet(struct hg3dclass_struct * thisclass_c, char * name_c, long * result_c);

// 
void ogre_scmgr_destroyBillboardSet(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * set_c);

// 
void ogre_scmgr_destroyBillboardSet2(struct hg3dclass_struct * thisclass_c, char * name_c);

// 
void ogre_scmgr_destroyAllBillboardSets(struct hg3dclass_struct * thisclass_c);

// 
void ogre_scmgr_setDisplaySceneNodes(struct hg3dclass_struct * thisclass_c, long display_c);

// 
void ogre_scmgr_getDisplaySceneNodes(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_scmgr_createAnimation(struct hg3dclass_struct * thisclass_c, char * name_c, float length_c, struct hg3dclass_struct * result_c);

// 
void ogre_scmgr_getAnimation(struct hg3dclass_struct * thisclass_c, char * name_c, struct hg3dclass_struct * result_c);

// 
void ogre_scmgr_hasAnimation(struct hg3dclass_struct * thisclass_c, char * name_c, long * result_c);

// 
void ogre_scmgr_destroyAnimation(struct hg3dclass_struct * thisclass_c, char * name_c);

// 
void ogre_scmgr_destroyAllAnimations(struct hg3dclass_struct * thisclass_c);

// 
void ogre_scmgr_createAnimationState(struct hg3dclass_struct * thisclass_c, char * animName_c, struct hg3dclass_struct * result_c);

// 
void ogre_scmgr_getAnimationState(struct hg3dclass_struct * thisclass_c, char * animName_c, struct hg3dclass_struct * result_c);

// 
void ogre_scmgr_hasAnimationState(struct hg3dclass_struct * thisclass_c, char * name_c, long * result_c);

// 
void ogre_scmgr_destroyAnimationState(struct hg3dclass_struct * thisclass_c, char * name_c);

// 
void ogre_scmgr_destroyAllAnimationStates(struct hg3dclass_struct * thisclass_c);

// 
void ogre_scmgr_clearSpecialCaseRenderQueues(struct hg3dclass_struct * thisclass_c);

// 
void ogre_scmgr_setSpecialCaseRenderQueueMode(struct hg3dclass_struct * thisclass_c, enum EnumSceneManagerSpecialCaseRenderQueueMode mode_c);

// 
void ogre_scmgr_getSpecialCaseRenderQueueMode(struct hg3dclass_struct * thisclass_c, enum EnumSceneManagerSpecialCaseRenderQueueMode * result_c);

// 
void ogre_scmgr_showBoundingBoxes(struct hg3dclass_struct * thisclass_c, long bShow_c);

// 
void ogre_scmgr_getShowBoundingBoxes(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_scmgr_setShowDebugShadows(struct hg3dclass_struct * thisclass_c, long debug_c);

// 
void ogre_scmgr_getShowDebugShadows(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_scmgr_setShadowColour(struct hg3dclass_struct * thisclass_c, struct colourvalue_struct * colour_c);

// 
void ogre_scmgr_getShadowColour(struct hg3dclass_struct * thisclass_c, struct colourvalue_struct * result_c);

// 
void ogre_scmgr_setShadowDirectionalLightExtrusionDistance(struct hg3dclass_struct * thisclass_c, float dist_c);

// 
void ogre_scmgr_getShadowDirectionalLightExtrusionDistance(struct hg3dclass_struct * thisclass_c, float * result_c);

// 
void ogre_scmgr_setShadowFarDistance(struct hg3dclass_struct * thisclass_c, float distance_c);

// 
void ogre_scmgr_getShadowFarDistance(struct hg3dclass_struct * thisclass_c, float * result_c);

// 
void ogre_scmgr_getShadowFarDistanceSquared(struct hg3dclass_struct * thisclass_c, float * result_c);

// 
void ogre_scmgr_setShadowIndexBufferSize(struct hg3dclass_struct * thisclass_c, long size_c);

// Get the size of the shadow index buffer. 
void ogre_scmgr_getShadowIndexBufferSize(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_scmgr_setShadowTextureSize(struct hg3dclass_struct * thisclass_c, unsigned short size_c);

// 
void ogre_scmgr_setShadowTextureFSAA(struct hg3dclass_struct * thisclass_c, unsigned short fsaa_c);

// 
void ogre_scmgr_setShadowTextureCount(struct hg3dclass_struct * thisclass_c, long count_c);

// Get the number of the textures allocated for texture based shadows. 
void ogre_scmgr_getShadowTextureCount(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_scmgr_setShadowTextureCountPerLightType(struct hg3dclass_struct * thisclass_c, enum EnumLightType type_c, long count_c);

// Get the number of shadow textures is assigned for the given light type. 
void ogre_scmgr_getShadowTextureCountPerLightType(struct hg3dclass_struct * thisclass_c, enum EnumLightType type_c, long * result_c);

// 
void ogre_scmgr_getShadowTexture(struct hg3dclass_struct * thisclass_c, long shadowIndex_c, struct sharedptr_struct * result_c);

// 
void ogre_scmgr_setShadowDirLightTextureOffset(struct hg3dclass_struct * thisclass_c, float offset_c);

// 
void ogre_scmgr_getShadowDirLightTextureOffset(struct hg3dclass_struct * thisclass_c, float * result_c);

// 
void ogre_scmgr_setShadowTextureFadeStart(struct hg3dclass_struct * thisclass_c, float fadeStart_c);

// 
void ogre_scmgr_setShadowTextureFadeEnd(struct hg3dclass_struct * thisclass_c, float fadeEnd_c);

// 
void ogre_scmgr_setShadowTextureSelfShadow(struct hg3dclass_struct * thisclass_c, long selfShadow_c);

// Gets whether or not texture shadows attempt to self-shadow. 
void ogre_scmgr_getShadowTextureSelfShadow(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_scmgr_setShadowTextureCasterMaterial(struct hg3dclass_struct * thisclass_c, char * name_c);

// 
void ogre_scmgr_setShadowTextureReceiverMaterial(struct hg3dclass_struct * thisclass_c, char * name_c);

// 
void ogre_scmgr_setShadowCasterRenderBackFaces(struct hg3dclass_struct * thisclass_c, long bf_c);

// 
void ogre_scmgr_getShadowCasterRenderBackFaces(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_scmgr_setShadowUseInfiniteFarPlane(struct hg3dclass_struct * thisclass_c, long enable_c);

// 
void ogre_scmgr_isShadowTechniqueStencilBased(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_scmgr_isShadowTechniqueTextureBased(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_scmgr_isShadowTechniqueModulative(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_scmgr_isShadowTechniqueAdditive(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_scmgr_isShadowTechniqueIntegrated(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_scmgr_isShadowTechniqueInUse(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_scmgr_setShadowUseLightClipPlanes(struct hg3dclass_struct * thisclass_c, long enabled_c);

// 
void ogre_scmgr_getShadowUseLightClipPlanes(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_scmgr_setLateMaterialResolving(struct hg3dclass_struct * thisclass_c, long isLate_c);

// 
void ogre_scmgr_isLateMaterialResolving(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_scmgr_hasStaticGeometry(struct hg3dclass_struct * thisclass_c, char * name_c, long * result_c);

// 
void ogre_scmgr_destroyStaticGeometry2(struct hg3dclass_struct * thisclass_c, char * name_c);

// 
void ogre_scmgr_destroyAllStaticGeometry(struct hg3dclass_struct * thisclass_c);

// 
void ogre_scmgr_destroyInstancedGeometry2(struct hg3dclass_struct * thisclass_c, char * name_c);

// 
void ogre_scmgr_destroyAllInstancedGeometry(struct hg3dclass_struct * thisclass_c);

// 
void ogre_scmgr_hasInstanceManager(struct hg3dclass_struct * thisclass_c, char * managerName_c, long * result_c);

// 
void ogre_scmgr_destroyInstanceManager(struct hg3dclass_struct * thisclass_c, char * name_c);

// 
void ogre_scmgr_destroyAllInstanceManagers(struct hg3dclass_struct * thisclass_c);

// 
void ogre_scmgr_destroyMovableObject(struct hg3dclass_struct * thisclass_c, char * name_c, char * typeName_c);

// 
void ogre_scmgr_destroyMovableObject2(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * m_c);

// 
void ogre_scmgr_destroyAllMovableObjectsByType(struct hg3dclass_struct * thisclass_c, char * typeName_c);

// 
void ogre_scmgr_destroyAllMovableObjects(struct hg3dclass_struct * thisclass_c);

// 
void ogre_scmgr_getMovableObject(struct hg3dclass_struct * thisclass_c, char * name_c, char * typeName_c, struct hg3dclass_struct * result_c);

// 
void ogre_scmgr_hasMovableObject(struct hg3dclass_struct * thisclass_c, char * name_c, char * typeName_c, long * result_c);

// 
void ogre_scmgr_injectMovableObject(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * m_c);

// 
void ogre_scmgr_extractMovableObject(struct hg3dclass_struct * thisclass_c, char * name_c, char * typeName_c);

// 
void ogre_scmgr_extractMovableObject2(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * m_c);

// 
void ogre_scmgr_extractAllMovableObjectsByType(struct hg3dclass_struct * thisclass_c, char * typeName_c);

// 
void ogre_scmgr_setVisibilityMask(struct hg3dclass_struct * thisclass_c, unsigned long vmask_c);

// 
void ogre_scmgr_getVisibilityMask(struct hg3dclass_struct * thisclass_c, unsigned long * result_c);

// 
void ogre_scmgr_setFindVisibleObjects(struct hg3dclass_struct * thisclass_c, long find_c);

// 
void ogre_scmgr_getFindVisibleObjects(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_scmgr_setNormaliseNormalsOnScale(struct hg3dclass_struct * thisclass_c, long n_c);

// 
void ogre_scmgr_getNormaliseNormalsOnScale(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_scmgr_setFlipCullingOnNegativeScale(struct hg3dclass_struct * thisclass_c, long n_c);

// 
void ogre_scmgr_getFlipCullingOnNegativeScale(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_scmgr_getDestinationRenderSystem(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// 
void ogre_scmgr_getCurrentViewport(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// 
void ogre_scmgr_setCameraRelativeRendering(struct hg3dclass_struct * thisclass_c, long rel_c);

// 
void ogre_scmgr_getCameraRelativeRendering(struct hg3dclass_struct * thisclass_c, long * result_c);

#endif 
