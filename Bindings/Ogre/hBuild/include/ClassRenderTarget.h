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

// ClassRenderTarget.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassRenderTarget
#define _DEFINED_HG3D_ClassRenderTarget

#include "ClassPtr.h"
#include "ClassViewport.h"
#include "ClassCamera.h"


// 
void ogre_rtgt_destruct(struct hg3dclass_struct * thisclass_c);

// Retrieve target's name. 
void ogre_rtgt_getName(struct hg3dclass_struct * thisclass_c, char * result_c);

// Retrieve information about the render target. 
void ogre_rtgt_getMetrics(struct hg3dclass_struct * thisclass_c, unsigned long * width_c, unsigned long * height_c, unsigned long * colourDepth_c);

// 
void ogre_rtgt_getWidth(struct hg3dclass_struct * thisclass_c, unsigned long * result_c);

// 
void ogre_rtgt_getHeight(struct hg3dclass_struct * thisclass_c, unsigned long * result_c);

// 
void ogre_rtgt_getColourDepth(struct hg3dclass_struct * thisclass_c, unsigned long * result_c);

// 
void ogre_rtgt_setDepthBufferPool(struct hg3dclass_struct * thisclass_c, unsigned short poolId_c);

// 
void ogre_rtgt_getDepthBufferPool(struct hg3dclass_struct * thisclass_c, unsigned short * result_c);

// 
void ogre_rtgt_detachDepthBuffer(struct hg3dclass_struct * thisclass_c);

// 
void ogre_rtgt_update(struct hg3dclass_struct * thisclass_c, long swapBuffers_c);

// 
void ogre_rtgt_swapBuffers(struct hg3dclass_struct * thisclass_c, long waitForVSync_c);

// 
void ogre_rtgt_addViewport(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * cam_c, long ZOrder_c, float left_c, float top_c, float width_c, float height_c, struct hg3dclass_struct * result_c);

// 
void ogre_rtgt_getNumViewports(struct hg3dclass_struct * thisclass_c, unsigned short * result_c);

// 
void ogre_rtgt_getViewport(struct hg3dclass_struct * thisclass_c, unsigned short index_c, struct hg3dclass_struct * result_c);

// 
void ogre_rtgt_getViewportByZOrder(struct hg3dclass_struct * thisclass_c, long ZOrder_c, struct hg3dclass_struct * result_c);

// 
void ogre_rtgt_hasViewportWithZOrder(struct hg3dclass_struct * thisclass_c, long ZOrder_c, long * result_c);

// 
void ogre_rtgt_removeViewport(struct hg3dclass_struct * thisclass_c, long ZOrder_c);

// 
void ogre_rtgt_removeAllViewports(struct hg3dclass_struct * thisclass_c);

// 
void ogre_rtgt_getStatistics(struct hg3dclass_struct * thisclass_c, float * lastFPS_c, float * avgFPS_c, float * bestFPS_c, float * worstFPS_c);

// 
void ogre_rtgt_getLastFPS(struct hg3dclass_struct * thisclass_c, float * result_c);

// 
void ogre_rtgt_getAverageFPS(struct hg3dclass_struct * thisclass_c, float * result_c);

// 
void ogre_rtgt_getBestFPS(struct hg3dclass_struct * thisclass_c, float * result_c);

// 
void ogre_rtgt_getWorstFPS(struct hg3dclass_struct * thisclass_c, float * result_c);

// 
void ogre_rtgt_getBestFrameTime(struct hg3dclass_struct * thisclass_c, float * result_c);

// 
void ogre_rtgt_getWorstFrameTime(struct hg3dclass_struct * thisclass_c, float * result_c);

// 
void ogre_rtgt_resetStatistics(struct hg3dclass_struct * thisclass_c);

// 
void ogre_rtgt_removeAllListeners(struct hg3dclass_struct * thisclass_c);

// 
void ogre_rtgt_isActive(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_rtgt_setActive(struct hg3dclass_struct * thisclass_c, long state_c);

// 
void ogre_rtgt_setAutoUpdated(struct hg3dclass_struct * thisclass_c, long autoupdate_c);

// 
void ogre_rtgt_isAutoUpdated(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_rtgt_writeContentsToFile(struct hg3dclass_struct * thisclass_c, char * filename_c);

// 
void ogre_rtgt_writeContentsToTimestampedFile(struct hg3dclass_struct * thisclass_c, char * filenamePrefix_c, char * filenameSuffix_c, char * result_c);

// 
void ogre_rtgt_requiresTextureFlipping(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_rtgt_getTriangleCount(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_rtgt_getBatchCount(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_rtgt_isPrimary(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_rtgt_isHardwareGammaEnabled(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_rtgt_getFSAA(struct hg3dclass_struct * thisclass_c, unsigned long * result_c);

// 
void ogre_rtgt_getFSAAHint(struct hg3dclass_struct * thisclass_c, char * result_c);

#endif 
