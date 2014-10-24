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

// ClassRenderTarget.cpp

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
	#include "./Ogre.h"
#include "./OgreString.h"
#include "./WindowUtilsHG3D.h"
#include "HG3DUtilities.h"

using namespace Ogre;
using Ogre::uint;



// 
extern "C" Ogre_LIB_EXPORT void ogre_rtgt_destruct(struct hg3dclass_struct * thisclass_c)
{
  Ogre::RenderTarget * thisclass_cpp = static_cast<Ogre::RenderTarget*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderTarget"));
  (delete thisclass_cpp);
};

// Retrieve target's name. 
extern "C" Ogre_LIB_EXPORT void ogre_rtgt_getName(struct hg3dclass_struct * thisclass_c, char * result_c)
{
  Ogre::RenderTarget * thisclass_cpp = static_cast<Ogre::RenderTarget*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderTarget"));
  Ogre::String result_cpp;
  result_cpp = (thisclass_cpp->getName());
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

// Retrieve information about the render target. 
extern "C" Ogre_LIB_EXPORT void ogre_rtgt_getMetrics(struct hg3dclass_struct * thisclass_c, unsigned long * width_c, unsigned long * height_c, unsigned long * colourDepth_c)
{
  Ogre::RenderTarget * thisclass_cpp = static_cast<Ogre::RenderTarget*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderTarget"));
  unsigned int width_cpp;
  unsigned int height_cpp;
  unsigned int colourDepth_cpp;
  (thisclass_cpp->getMetrics(width_cpp, height_cpp, colourDepth_cpp));
  *width_c = (unsigned long)width_cpp;
  *height_c = (unsigned long)height_cpp;
  *colourDepth_c = (unsigned long)colourDepth_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rtgt_getWidth(struct hg3dclass_struct * thisclass_c, unsigned long * result_c)
{
  Ogre::RenderTarget * thisclass_cpp = static_cast<Ogre::RenderTarget*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderTarget"));
  unsigned int result_cpp;
  result_cpp = (thisclass_cpp->getWidth());
  *result_c = (unsigned long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rtgt_getHeight(struct hg3dclass_struct * thisclass_c, unsigned long * result_c)
{
  Ogre::RenderTarget * thisclass_cpp = static_cast<Ogre::RenderTarget*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderTarget"));
  unsigned int result_cpp;
  result_cpp = (thisclass_cpp->getHeight());
  *result_c = (unsigned long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rtgt_getColourDepth(struct hg3dclass_struct * thisclass_c, unsigned long * result_c)
{
  Ogre::RenderTarget * thisclass_cpp = static_cast<Ogre::RenderTarget*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderTarget"));
  unsigned int result_cpp;
  result_cpp = (thisclass_cpp->getColourDepth());
  *result_c = (unsigned long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rtgt_setDepthBufferPool(struct hg3dclass_struct * thisclass_c, unsigned short poolId_c)
{
  Ogre::RenderTarget * thisclass_cpp = static_cast<Ogre::RenderTarget*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderTarget"));
  uint16 poolId_cpp = (uint16)poolId_c;
  (thisclass_cpp->setDepthBufferPool(poolId_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rtgt_getDepthBufferPool(struct hg3dclass_struct * thisclass_c, unsigned short * result_c)
{
  Ogre::RenderTarget * thisclass_cpp = static_cast<Ogre::RenderTarget*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderTarget"));
  uint16 result_cpp;
  result_cpp = (thisclass_cpp->getDepthBufferPool());
  *result_c = (unsigned short)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rtgt_detachDepthBuffer(struct hg3dclass_struct * thisclass_c)
{
  Ogre::RenderTarget * thisclass_cpp = static_cast<Ogre::RenderTarget*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderTarget"));
  (thisclass_cpp->detachDepthBuffer());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rtgt_update(struct hg3dclass_struct * thisclass_c, long swapBuffers_c)
{
  Ogre::RenderTarget * thisclass_cpp = static_cast<Ogre::RenderTarget*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderTarget"));
  bool swapBuffers_cpp = (bool)swapBuffers_c;
  (thisclass_cpp->update(swapBuffers_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rtgt_swapBuffers(struct hg3dclass_struct * thisclass_c, long waitForVSync_c)
{
  Ogre::RenderTarget * thisclass_cpp = static_cast<Ogre::RenderTarget*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderTarget"));
  bool waitForVSync_cpp = (bool)waitForVSync_c;
  (thisclass_cpp->swapBuffers(waitForVSync_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rtgt_addViewport(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * cam_c, long ZOrder_c, float left_c, float top_c, float width_c, float height_c, struct hg3dclass_struct * result_c)
{
  Ogre::RenderTarget * thisclass_cpp = static_cast<Ogre::RenderTarget*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderTarget"));
  Ogre::Camera * cam_cpp = static_cast<Ogre::Camera*> (getHG3DClassPtr(*cam_c, "Ogre::Camera"));
  int ZOrder_cpp = (int)ZOrder_c;
  float left_cpp = (float)left_c;
  float top_cpp = (float)top_c;
  float width_cpp = (float)width_c;
  float height_cpp = (float)height_c;
  Ogre::Viewport * result_cpp;
  result_cpp = (thisclass_cpp->addViewport(cam_cpp, ZOrder_cpp, left_cpp, top_cpp, width_cpp, height_cpp));
  *result_c = getHG3DClass_Viewport((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rtgt_getNumViewports(struct hg3dclass_struct * thisclass_c, unsigned short * result_c)
{
  Ogre::RenderTarget * thisclass_cpp = static_cast<Ogre::RenderTarget*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderTarget"));
  unsigned short result_cpp;
  result_cpp = (thisclass_cpp->getNumViewports());
  *result_c = (unsigned short)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rtgt_getViewport(struct hg3dclass_struct * thisclass_c, unsigned short index_c, struct hg3dclass_struct * result_c)
{
  Ogre::RenderTarget * thisclass_cpp = static_cast<Ogre::RenderTarget*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderTarget"));
  unsigned short index_cpp = (unsigned short)index_c;
  Ogre::Viewport * result_cpp;
  result_cpp = (thisclass_cpp->getViewport(index_cpp));
  *result_c = getHG3DClass_Viewport((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rtgt_getViewportByZOrder(struct hg3dclass_struct * thisclass_c, long ZOrder_c, struct hg3dclass_struct * result_c)
{
  Ogre::RenderTarget * thisclass_cpp = static_cast<Ogre::RenderTarget*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderTarget"));
  int ZOrder_cpp = (int)ZOrder_c;
  Ogre::Viewport * result_cpp;
  result_cpp = (thisclass_cpp->getViewportByZOrder(ZOrder_cpp));
  *result_c = getHG3DClass_Viewport((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rtgt_hasViewportWithZOrder(struct hg3dclass_struct * thisclass_c, long ZOrder_c, long * result_c)
{
  Ogre::RenderTarget * thisclass_cpp = static_cast<Ogre::RenderTarget*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderTarget"));
  int ZOrder_cpp = (int)ZOrder_c;
  bool result_cpp;
  result_cpp = (thisclass_cpp->hasViewportWithZOrder(ZOrder_cpp));
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rtgt_removeViewport(struct hg3dclass_struct * thisclass_c, long ZOrder_c)
{
  Ogre::RenderTarget * thisclass_cpp = static_cast<Ogre::RenderTarget*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderTarget"));
  int ZOrder_cpp = (int)ZOrder_c;
  (thisclass_cpp->removeViewport(ZOrder_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rtgt_removeAllViewports(struct hg3dclass_struct * thisclass_c)
{
  Ogre::RenderTarget * thisclass_cpp = static_cast<Ogre::RenderTarget*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderTarget"));
  (thisclass_cpp->removeAllViewports());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rtgt_getStatistics(struct hg3dclass_struct * thisclass_c, float * lastFPS_c, float * avgFPS_c, float * bestFPS_c, float * worstFPS_c)
{
  Ogre::RenderTarget * thisclass_cpp = static_cast<Ogre::RenderTarget*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderTarget"));
  float lastFPS_cpp;
  float avgFPS_cpp;
  float bestFPS_cpp;
  float worstFPS_cpp;
  (thisclass_cpp->getStatistics(lastFPS_cpp, avgFPS_cpp, bestFPS_cpp, worstFPS_cpp));
  *lastFPS_c = (float)lastFPS_cpp;
  *avgFPS_c = (float)avgFPS_cpp;
  *bestFPS_c = (float)bestFPS_cpp;
  *worstFPS_c = (float)worstFPS_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rtgt_getLastFPS(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  Ogre::RenderTarget * thisclass_cpp = static_cast<Ogre::RenderTarget*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderTarget"));
  float result_cpp;
  result_cpp = (thisclass_cpp->getLastFPS());
  *result_c = (float)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rtgt_getAverageFPS(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  Ogre::RenderTarget * thisclass_cpp = static_cast<Ogre::RenderTarget*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderTarget"));
  float result_cpp;
  result_cpp = (thisclass_cpp->getAverageFPS());
  *result_c = (float)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rtgt_getBestFPS(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  Ogre::RenderTarget * thisclass_cpp = static_cast<Ogre::RenderTarget*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderTarget"));
  float result_cpp;
  result_cpp = (thisclass_cpp->getBestFPS());
  *result_c = (float)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rtgt_getWorstFPS(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  Ogre::RenderTarget * thisclass_cpp = static_cast<Ogre::RenderTarget*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderTarget"));
  float result_cpp;
  result_cpp = (thisclass_cpp->getWorstFPS());
  *result_c = (float)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rtgt_getBestFrameTime(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  Ogre::RenderTarget * thisclass_cpp = static_cast<Ogre::RenderTarget*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderTarget"));
  float result_cpp;
  result_cpp = (thisclass_cpp->getBestFrameTime());
  *result_c = (float)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rtgt_getWorstFrameTime(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  Ogre::RenderTarget * thisclass_cpp = static_cast<Ogre::RenderTarget*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderTarget"));
  float result_cpp;
  result_cpp = (thisclass_cpp->getWorstFrameTime());
  *result_c = (float)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rtgt_resetStatistics(struct hg3dclass_struct * thisclass_c)
{
  Ogre::RenderTarget * thisclass_cpp = static_cast<Ogre::RenderTarget*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderTarget"));
  (thisclass_cpp->resetStatistics());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rtgt_removeAllListeners(struct hg3dclass_struct * thisclass_c)
{
  Ogre::RenderTarget * thisclass_cpp = static_cast<Ogre::RenderTarget*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderTarget"));
  (thisclass_cpp->removeAllListeners());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rtgt_isActive(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::RenderTarget * thisclass_cpp = static_cast<Ogre::RenderTarget*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderTarget"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isActive());
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rtgt_setActive(struct hg3dclass_struct * thisclass_c, long state_c)
{
  Ogre::RenderTarget * thisclass_cpp = static_cast<Ogre::RenderTarget*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderTarget"));
  bool state_cpp = (bool)state_c;
  (thisclass_cpp->setActive(state_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rtgt_setAutoUpdated(struct hg3dclass_struct * thisclass_c, long autoupdate_c)
{
  Ogre::RenderTarget * thisclass_cpp = static_cast<Ogre::RenderTarget*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderTarget"));
  bool autoupdate_cpp = (bool)autoupdate_c;
  (thisclass_cpp->setAutoUpdated(autoupdate_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rtgt_isAutoUpdated(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::RenderTarget * thisclass_cpp = static_cast<Ogre::RenderTarget*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderTarget"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isAutoUpdated());
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rtgt_writeContentsToFile(struct hg3dclass_struct * thisclass_c, char * filename_c)
{
  Ogre::RenderTarget * thisclass_cpp = static_cast<Ogre::RenderTarget*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderTarget"));
  Ogre::String filename_cpp = Ogre::String((const char*) filename_c);
  (thisclass_cpp->writeContentsToFile(filename_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rtgt_writeContentsToTimestampedFile(struct hg3dclass_struct * thisclass_c, char * filenamePrefix_c, char * filenameSuffix_c, char * result_c)
{
  Ogre::RenderTarget * thisclass_cpp = static_cast<Ogre::RenderTarget*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderTarget"));
  Ogre::String filenamePrefix_cpp = Ogre::String((const char*) filenamePrefix_c);
  Ogre::String filenameSuffix_cpp = Ogre::String((const char*) filenameSuffix_c);
  Ogre::String result_cpp;
  result_cpp = (thisclass_cpp->writeContentsToTimestampedFile(filenamePrefix_cpp, filenameSuffix_cpp));
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rtgt_requiresTextureFlipping(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::RenderTarget * thisclass_cpp = static_cast<Ogre::RenderTarget*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderTarget"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->requiresTextureFlipping());
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rtgt_getTriangleCount(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::RenderTarget * thisclass_cpp = static_cast<Ogre::RenderTarget*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderTarget"));
  size_t result_cpp;
  result_cpp = (thisclass_cpp->getTriangleCount());
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rtgt_getBatchCount(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::RenderTarget * thisclass_cpp = static_cast<Ogre::RenderTarget*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderTarget"));
  size_t result_cpp;
  result_cpp = (thisclass_cpp->getBatchCount());
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rtgt_isPrimary(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::RenderTarget * thisclass_cpp = static_cast<Ogre::RenderTarget*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderTarget"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isPrimary());
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rtgt_isHardwareGammaEnabled(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::RenderTarget * thisclass_cpp = static_cast<Ogre::RenderTarget*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderTarget"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isHardwareGammaEnabled());
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rtgt_getFSAA(struct hg3dclass_struct * thisclass_c, unsigned long * result_c)
{
  Ogre::RenderTarget * thisclass_cpp = static_cast<Ogre::RenderTarget*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderTarget"));
  uint result_cpp;
  result_cpp = (thisclass_cpp->getFSAA());
  *result_c = (unsigned long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rtgt_getFSAAHint(struct hg3dclass_struct * thisclass_c, char * result_c)
{
  Ogre::RenderTarget * thisclass_cpp = static_cast<Ogre::RenderTarget*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderTarget"));
  Ogre::String result_cpp;
  result_cpp = (thisclass_cpp->getFSAAHint());
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

