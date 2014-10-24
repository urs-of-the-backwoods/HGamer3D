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

// ClassRoot.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassRoot
#define _DEFINED_HG3D_ClassRoot

#include "ClassPtr.h"
#include "ClassRenderSystem.h"
#include "ClassRenderWindow.h"
#include "ClassSceneManagerFactory.h"
#include "ClassSceneManager.h"
#include "ClassTextureManager.h"
#include "ClassMeshManager.h"
#include "StructColour.h"
#include "ClassRenderTarget.h"
#include "ClassMovableObjectFactory.h"


// 
void ogre_rt_construct(char * pluginFileName_c, char * configFileName_c, char * logFileName_c, struct hg3dclass_struct * result_c);

// 
void ogre_rt_destruct(struct hg3dclass_struct * thisclass_c);

// 
void ogre_rt_saveConfig(struct hg3dclass_struct * thisclass_c);

// 
void ogre_rt_restoreConfig(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_rt_showConfigDialog(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_rt_addRenderSystem(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * newRend_c);

// 
void ogre_rt_getRenderSystemByName(struct hg3dclass_struct * thisclass_c, char * name_c, struct hg3dclass_struct * result_c);

// 
void ogre_rt_setRenderSystem(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * system_c);

// 
void ogre_rt_getRenderSystem(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// 
void ogre_rt_initialise(struct hg3dclass_struct * thisclass_c, long autoCreateWindow_c, char * windowTitle_c, char * customCapabilitiesConfig_c, struct hg3dclass_struct * result_c);

// 
void ogre_rt_isInitialised(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_rt_getRemoveRenderQueueStructuresOnClear(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_rt_setRemoveRenderQueueStructuresOnClear(struct hg3dclass_struct * thisclass_c, long r_c);

// 
void ogre_rt_addSceneManagerFactory(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * fact_c);

// 
void ogre_rt_removeSceneManagerFactory(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * fact_c);

// 
void ogre_rt_createSceneManager(struct hg3dclass_struct * thisclass_c, char * typeName_c, char * instanceName_c, struct hg3dclass_struct * result_c);

// 
void ogre_rt_destroySceneManager(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * sm_c);

// 
void ogre_rt_getSceneManager(struct hg3dclass_struct * thisclass_c, char * instanceName_c, struct hg3dclass_struct * result_c);

// 
void ogre_rt_hasSceneManager(struct hg3dclass_struct * thisclass_c, char * instanceName_c, long * result_c);

// 
void ogre_rt_getTextureManager(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// 
void ogre_rt_getMeshManager(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// 
void ogre_rt_getErrorDescription(struct hg3dclass_struct * thisclass_c, long errorNumber_c, char * result_c);

// 
void ogre_rt_queueEndRendering(struct hg3dclass_struct * thisclass_c);

// 
void ogre_rt_startRendering(struct hg3dclass_struct * thisclass_c);

// 
void ogre_rt_renderOneFrame(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_rt_renderOneFrame2(struct hg3dclass_struct * thisclass_c, float timeSinceLastFrame_c, long * result_c);

// 
void ogre_rt_shutdown(struct hg3dclass_struct * thisclass_c);

// 
void ogre_rt_addResourceLocation(struct hg3dclass_struct * thisclass_c, char * name_c, char * locType_c, char * groupName_c, long recursive_c);

// 
void ogre_rt_removeResourceLocation(struct hg3dclass_struct * thisclass_c, char * name_c, char * groupName_c);

// 
void ogre_rt_convertColourValue(struct hg3dclass_struct * thisclass_c, struct colourvalue_struct * colour_c, unsigned long * pDest_c);

// 
void ogre_rt_getAutoCreatedWindow(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// 
void ogre_rt_detachRenderTarget(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * pWin_c, struct hg3dclass_struct * result_c);

// 
void ogre_rt_detachRenderTarget2(struct hg3dclass_struct * thisclass_c, char * name_c, struct hg3dclass_struct * result_c);

// 
void ogre_rt_destroyRenderTarget(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * target_c);

// 
void ogre_rt_destroyRenderTarget2(struct hg3dclass_struct * thisclass_c, char * name_c);

// 
void ogre_rt_getRenderTarget(struct hg3dclass_struct * thisclass_c, char * name_c, struct hg3dclass_struct * result_c);

// 
void ogre_rt_loadPlugin(struct hg3dclass_struct * thisclass_c, char * pluginName_c);

// 
void ogre_rt_unloadPlugin(struct hg3dclass_struct * thisclass_c, char * pluginName_c);

// 
void ogre_rt_getNextFrameNumber(struct hg3dclass_struct * thisclass_c, unsigned long * result_c);

// 
void ogre_rt_destroyRenderQueueInvocationSequence(struct hg3dclass_struct * thisclass_c, char * name_c);

// 
void ogre_rt_destroyAllRenderQueueInvocationSequences(struct hg3dclass_struct * thisclass_c);

// 
void ogre_rt_clearEventTimes(struct hg3dclass_struct * thisclass_c);

// 
void ogre_rt_setFrameSmoothingPeriod(struct hg3dclass_struct * thisclass_c, float period_c);

// 
void ogre_rt_getFrameSmoothingPeriod(struct hg3dclass_struct * thisclass_c, float * result_c);

// 
void ogre_rt_addMovableObjectFactory(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * fact_c, long overrideExisting_c);

// 
void ogre_rt_removeMovableObjectFactory(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * fact_c);

// Checks whether a factory is registered for a given MovableObject
void ogre_rt_hasMovableObjectFactory(struct hg3dclass_struct * thisclass_c, char * typeName_c, long * result_c);

// Get a MovableObjectFactory
void ogre_rt_getMovableObjectFactory(struct hg3dclass_struct * thisclass_c, char * typeName_c, struct hg3dclass_struct * result_c);

// 
void ogre_rt_getDisplayMonitorCount(struct hg3dclass_struct * thisclass_c, unsigned long * result_c);

// 
void ogre_rt_setBlendIndicesGpuRedundant(struct hg3dclass_struct * thisclass_c, long redundant_c);

// 
void ogre_rt_isBlendIndicesGpuRedundant(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_rt_setBlendWeightsGpuRedundant(struct hg3dclass_struct * thisclass_c, long redundant_c);

// 
void ogre_rt_isBlendWeightsGpuRedundant(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_rt_setDefaultMinPixelSize(struct hg3dclass_struct * thisclass_c, float pixelSize_c);

// 
void ogre_rt_getDefaultMinPixelSize(struct hg3dclass_struct * thisclass_c, float * result_c);

// 
void ogre_rt_getSingleton(struct hg3dclass_struct * result_c);

// 
void ogre_rt_getSingletonPtr(struct hg3dclass_struct * result_c);

#endif 
