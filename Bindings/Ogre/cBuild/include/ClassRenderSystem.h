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

// ClassRenderSystem.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassRenderSystem
#define _DEFINED_HG3D_ClassRenderSystem

#include "ClassPtr.h"
#include "ClassMultiRenderTarget.h"
#include "ClassRenderTarget.h"
#include "StructColour.h"
#include "EnumGpuProgramType.h"


// 
void ogre_rds_destruct(struct hg3dclass_struct * thisclass_c);

// 
void ogre_rds_getName(struct hg3dclass_struct * thisclass_c, char * result_c);

// 
void ogre_rds_setConfigOption(struct hg3dclass_struct * thisclass_c, char * name_c, char * value_c);

// 
void ogre_rds_validateConfigOptions(struct hg3dclass_struct * thisclass_c, char * result_c);

// 
void ogre_rds_reinitialise(struct hg3dclass_struct * thisclass_c);

// 
void ogre_rds_shutdown(struct hg3dclass_struct * thisclass_c);

// 
void ogre_rds_setAmbientLight(struct hg3dclass_struct * thisclass_c, float r_c, float g_c, float b_c);

// 
void ogre_rds_setLightingEnabled(struct hg3dclass_struct * thisclass_c, long enabled_c);

// 
void ogre_rds_setWBufferEnabled(struct hg3dclass_struct * thisclass_c, long enabled_c);

// 
void ogre_rds_getWBufferEnabled(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_rds_createMultiRenderTarget(struct hg3dclass_struct * thisclass_c, char * name_c, struct hg3dclass_struct * result_c);

// 
void ogre_rds_destroyRenderWindow(struct hg3dclass_struct * thisclass_c, char * name_c);

// 
void ogre_rds_destroyRenderTexture(struct hg3dclass_struct * thisclass_c, char * name_c);

// 
void ogre_rds_destroyRenderTarget(struct hg3dclass_struct * thisclass_c, char * name_c);

// 
void ogre_rds_attachRenderTarget(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * target_c);

// 
void ogre_rds_getRenderTarget(struct hg3dclass_struct * thisclass_c, char * name_c, struct hg3dclass_struct * result_c);

// 
void ogre_rds_detachRenderTarget(struct hg3dclass_struct * thisclass_c, char * name_c, struct hg3dclass_struct * result_c);

// 
void ogre_rds_getErrorDescription(struct hg3dclass_struct * thisclass_c, long errorNumber_c, char * result_c);

// 
void ogre_rds_setWaitForVerticalBlank(struct hg3dclass_struct * thisclass_c, long enabled_c);

// 
void ogre_rds_getWaitForVerticalBlank(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_rds_getGlobalNumberOfInstances(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_rds_setGlobalNumberOfInstances(struct hg3dclass_struct * thisclass_c, const long val_c);

// 
void ogre_rds_setDepthBufferFor(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * renderTarget_c);

// 
void ogre_rds_areFixedFunctionLightsInViewSpace(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_rds_convertColourValue(struct hg3dclass_struct * thisclass_c, struct colourvalue_struct * colour_c, unsigned long * pDest_c);

// 
void ogre_rds_setStencilCheckEnabled(struct hg3dclass_struct * thisclass_c, long enabled_c);

// 
void ogre_rds_setNormaliseNormals(struct hg3dclass_struct * thisclass_c, long normalise_c);

// 
void ogre_rds_bindGpuProgramPassIterationParameters(struct hg3dclass_struct * thisclass_c, enum EnumGpuProgramType gptype_c);

// 
void ogre_rds_unbindGpuProgram(struct hg3dclass_struct * thisclass_c, enum EnumGpuProgramType gptype_c);

// 
void ogre_rds_isGpuProgramBound(struct hg3dclass_struct * thisclass_c, enum EnumGpuProgramType gptype_c, long * result_c);

// 
void ogre_rds_addClipPlane2(struct hg3dclass_struct * thisclass_c, float A_c, float B_c, float C_c, float D_c);

// 
void ogre_rds_resetClipPlanes(struct hg3dclass_struct * thisclass_c);

// 
void ogre_rds_setInvertVertexWinding(struct hg3dclass_struct * thisclass_c, long invert_c);

// 
void ogre_rds_getInvertVertexWinding(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_rds_setScissorTest(struct hg3dclass_struct * thisclass_c, long enabled_c, long left_c, long top_c, long right_c, long bottom_c);

// 
void ogre_rds_clearFrameBuffer(struct hg3dclass_struct * thisclass_c, unsigned long buffers_c, struct colourvalue_struct * colour_c, float depth_c, unsigned short stencil_c);

// 
void ogre_rds_getHorizontalTexelOffset(struct hg3dclass_struct * thisclass_c, float * result_c);

// 
void ogre_rds_getVerticalTexelOffset(struct hg3dclass_struct * thisclass_c, float * result_c);

// 
void ogre_rds_getMinimumDepthInputValue(struct hg3dclass_struct * thisclass_c, float * result_c);

// 
void ogre_rds_getMaximumDepthInputValue(struct hg3dclass_struct * thisclass_c, float * result_c);

// 
void ogre_rds_setCurrentPassIterationCount(struct hg3dclass_struct * thisclass_c, const long count_c);

// 
void ogre_rds_setDeriveDepthBias(struct hg3dclass_struct * thisclass_c, long derive_c, float baseValue_c, float multiplier_c, float slopeScale_c);

// 
void ogre_rds_preExtraThreadsStarted(struct hg3dclass_struct * thisclass_c);

// 
void ogre_rds_postExtraThreadsStarted(struct hg3dclass_struct * thisclass_c);

// 
void ogre_rds_registerThread(struct hg3dclass_struct * thisclass_c);

// 
void ogre_rds_unregisterThread(struct hg3dclass_struct * thisclass_c);

// 
void ogre_rds_getDisplayMonitorCount(struct hg3dclass_struct * thisclass_c, unsigned long * result_c);

// 
void ogre_rds_beginProfileEvent(struct hg3dclass_struct * thisclass_c, char * eventName_c);

// 
void ogre_rds_endProfileEvent(struct hg3dclass_struct * thisclass_c);

// 
void ogre_rds_markProfileEvent(struct hg3dclass_struct * thisclass_c, char * event_c);

#endif 
