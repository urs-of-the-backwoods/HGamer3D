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

// ClassRenderSystem.cpp

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
	#include "StructColour.h"
#include "EnumGpuProgramType.h"
#include "./Ogre.h"
#include "./OgreString.h"
#include "./WindowUtilsHG3D.h"
#include "HG3DUtilities.h"

using namespace Ogre;
using Ogre::uint;



// 
extern "C" Ogre_LIB_EXPORT void ogre_rds_destruct(struct hg3dclass_struct * thisclass_c)
{
  Ogre::RenderSystem * thisclass_cpp = static_cast<Ogre::RenderSystem*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderSystem"));
  (delete thisclass_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rds_getName(struct hg3dclass_struct * thisclass_c, char * result_c)
{
  Ogre::RenderSystem * thisclass_cpp = static_cast<Ogre::RenderSystem*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderSystem"));
  Ogre::String result_cpp;
  result_cpp = (thisclass_cpp->getName());
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rds_setConfigOption(struct hg3dclass_struct * thisclass_c, char * name_c, char * value_c)
{
  Ogre::RenderSystem * thisclass_cpp = static_cast<Ogre::RenderSystem*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderSystem"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  Ogre::String value_cpp = Ogre::String((const char*) value_c);
  (thisclass_cpp->setConfigOption(name_cpp, value_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rds_validateConfigOptions(struct hg3dclass_struct * thisclass_c, char * result_c)
{
  Ogre::RenderSystem * thisclass_cpp = static_cast<Ogre::RenderSystem*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderSystem"));
  Ogre::String result_cpp;
  result_cpp = (thisclass_cpp->validateConfigOptions());
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rds_reinitialise(struct hg3dclass_struct * thisclass_c)
{
  Ogre::RenderSystem * thisclass_cpp = static_cast<Ogre::RenderSystem*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderSystem"));
  (thisclass_cpp->reinitialise());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rds_shutdown(struct hg3dclass_struct * thisclass_c)
{
  Ogre::RenderSystem * thisclass_cpp = static_cast<Ogre::RenderSystem*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderSystem"));
  (thisclass_cpp->shutdown());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rds_setAmbientLight(struct hg3dclass_struct * thisclass_c, float r_c, float g_c, float b_c)
{
  Ogre::RenderSystem * thisclass_cpp = static_cast<Ogre::RenderSystem*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderSystem"));
  float r_cpp = (float)r_c;
  float g_cpp = (float)g_c;
  float b_cpp = (float)b_c;
  (thisclass_cpp->setAmbientLight(r_cpp, g_cpp, b_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rds_setLightingEnabled(struct hg3dclass_struct * thisclass_c, int enabled_c)
{
  Ogre::RenderSystem * thisclass_cpp = static_cast<Ogre::RenderSystem*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderSystem"));
  bool enabled_cpp = (bool)enabled_c;
  (thisclass_cpp->setLightingEnabled(enabled_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rds_setWBufferEnabled(struct hg3dclass_struct * thisclass_c, int enabled_c)
{
  Ogre::RenderSystem * thisclass_cpp = static_cast<Ogre::RenderSystem*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderSystem"));
  bool enabled_cpp = (bool)enabled_c;
  (thisclass_cpp->setWBufferEnabled(enabled_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rds_getWBufferEnabled(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::RenderSystem * thisclass_cpp = static_cast<Ogre::RenderSystem*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderSystem"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->getWBufferEnabled());
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rds_createMultiRenderTarget(struct hg3dclass_struct * thisclass_c, char * name_c, struct hg3dclass_struct * result_c)
{
  Ogre::RenderSystem * thisclass_cpp = static_cast<Ogre::RenderSystem*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderSystem"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  Ogre::MultiRenderTarget * result_cpp;
  result_cpp = (thisclass_cpp->createMultiRenderTarget(name_cpp));
  *result_c = getHG3DClass_MultiRenderTarget((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rds_destroyRenderWindow(struct hg3dclass_struct * thisclass_c, char * name_c)
{
  Ogre::RenderSystem * thisclass_cpp = static_cast<Ogre::RenderSystem*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderSystem"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  (thisclass_cpp->destroyRenderWindow(name_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rds_destroyRenderTexture(struct hg3dclass_struct * thisclass_c, char * name_c)
{
  Ogre::RenderSystem * thisclass_cpp = static_cast<Ogre::RenderSystem*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderSystem"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  (thisclass_cpp->destroyRenderTexture(name_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rds_destroyRenderTarget(struct hg3dclass_struct * thisclass_c, char * name_c)
{
  Ogre::RenderSystem * thisclass_cpp = static_cast<Ogre::RenderSystem*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderSystem"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  (thisclass_cpp->destroyRenderTarget(name_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rds_attachRenderTarget(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * target_c)
{
  Ogre::RenderSystem * thisclass_cpp = static_cast<Ogre::RenderSystem*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderSystem"));
  Ogre::RenderTarget * target_cpp = static_cast<Ogre::RenderTarget*> (getHG3DClassPtr(*target_c, "Ogre::RenderTarget"));
  (thisclass_cpp->attachRenderTarget(*target_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rds_getRenderTarget(struct hg3dclass_struct * thisclass_c, char * name_c, struct hg3dclass_struct * result_c)
{
  Ogre::RenderSystem * thisclass_cpp = static_cast<Ogre::RenderSystem*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderSystem"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  Ogre::RenderTarget * result_cpp;
  result_cpp = (thisclass_cpp->getRenderTarget(name_cpp));
  *result_c = getHG3DClass_RenderTarget((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rds_detachRenderTarget(struct hg3dclass_struct * thisclass_c, char * name_c, struct hg3dclass_struct * result_c)
{
  Ogre::RenderSystem * thisclass_cpp = static_cast<Ogre::RenderSystem*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderSystem"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  Ogre::RenderTarget * result_cpp;
  result_cpp = (thisclass_cpp->detachRenderTarget(name_cpp));
  *result_c = getHG3DClass_RenderTarget((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rds_getErrorDescription(struct hg3dclass_struct * thisclass_c, int errorNumber_c, char * result_c)
{
  Ogre::RenderSystem * thisclass_cpp = static_cast<Ogre::RenderSystem*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderSystem"));
  long errorNumber_cpp = (long)errorNumber_c;
  Ogre::String result_cpp;
  result_cpp = (thisclass_cpp->getErrorDescription(errorNumber_cpp));
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rds_setWaitForVerticalBlank(struct hg3dclass_struct * thisclass_c, int enabled_c)
{
  Ogre::RenderSystem * thisclass_cpp = static_cast<Ogre::RenderSystem*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderSystem"));
  bool enabled_cpp = (bool)enabled_c;
  (thisclass_cpp->setWaitForVerticalBlank(enabled_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rds_getWaitForVerticalBlank(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::RenderSystem * thisclass_cpp = static_cast<Ogre::RenderSystem*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderSystem"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->getWaitForVerticalBlank());
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rds_getGlobalNumberOfInstances(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::RenderSystem * thisclass_cpp = static_cast<Ogre::RenderSystem*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderSystem"));
  size_t result_cpp;
  result_cpp = (thisclass_cpp->getGlobalNumberOfInstances());
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rds_setGlobalNumberOfInstances(struct hg3dclass_struct * thisclass_c, const int val_c)
{
  Ogre::RenderSystem * thisclass_cpp = static_cast<Ogre::RenderSystem*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderSystem"));
  const size_t val_cpp = (size_t)val_c;
  (thisclass_cpp->setGlobalNumberOfInstances(val_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rds_setDepthBufferFor(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * renderTarget_c)
{
  Ogre::RenderSystem * thisclass_cpp = static_cast<Ogre::RenderSystem*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderSystem"));
  Ogre::RenderTarget * renderTarget_cpp = static_cast<Ogre::RenderTarget*> (getHG3DClassPtr(*renderTarget_c, "Ogre::RenderTarget"));
  (thisclass_cpp->setDepthBufferFor(renderTarget_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rds_areFixedFunctionLightsInViewSpace(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::RenderSystem * thisclass_cpp = static_cast<Ogre::RenderSystem*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderSystem"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->areFixedFunctionLightsInViewSpace());
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rds_convertColourValue(struct hg3dclass_struct * thisclass_c, struct colourvalue_struct * colour_c, unsigned int * pDest_c)
{
  Ogre::RenderSystem * thisclass_cpp = static_cast<Ogre::RenderSystem*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderSystem"));
  ColourValue colour_cpp = *((ColourValue*) colour_c);
  uint32 pDest_cpp;
  (thisclass_cpp->convertColourValue(colour_cpp, &pDest_cpp));
  *pDest_c = (unsigned int)pDest_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rds_setStencilCheckEnabled(struct hg3dclass_struct * thisclass_c, int enabled_c)
{
  Ogre::RenderSystem * thisclass_cpp = static_cast<Ogre::RenderSystem*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderSystem"));
  bool enabled_cpp = (bool)enabled_c;
  (thisclass_cpp->setStencilCheckEnabled(enabled_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rds_setNormaliseNormals(struct hg3dclass_struct * thisclass_c, int normalise_c)
{
  Ogre::RenderSystem * thisclass_cpp = static_cast<Ogre::RenderSystem*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderSystem"));
  bool normalise_cpp = (bool)normalise_c;
  (thisclass_cpp->setNormaliseNormals(normalise_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rds_bindGpuProgramPassIterationParameters(struct hg3dclass_struct * thisclass_c, enum EnumGpuProgramType gptype_c)
{
  Ogre::RenderSystem * thisclass_cpp = static_cast<Ogre::RenderSystem*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderSystem"));
  enum Ogre::GpuProgramType gptype_cpp = (enum Ogre::GpuProgramType)gptype_c;
  (thisclass_cpp->bindGpuProgramPassIterationParameters(gptype_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rds_unbindGpuProgram(struct hg3dclass_struct * thisclass_c, enum EnumGpuProgramType gptype_c)
{
  Ogre::RenderSystem * thisclass_cpp = static_cast<Ogre::RenderSystem*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderSystem"));
  enum Ogre::GpuProgramType gptype_cpp = (enum Ogre::GpuProgramType)gptype_c;
  (thisclass_cpp->unbindGpuProgram(gptype_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rds_isGpuProgramBound(struct hg3dclass_struct * thisclass_c, enum EnumGpuProgramType gptype_c, int * result_c)
{
  Ogre::RenderSystem * thisclass_cpp = static_cast<Ogre::RenderSystem*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderSystem"));
  enum Ogre::GpuProgramType gptype_cpp = (enum Ogre::GpuProgramType)gptype_c;
  bool result_cpp;
  result_cpp = (thisclass_cpp->isGpuProgramBound(gptype_cpp));
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rds_addClipPlane2(struct hg3dclass_struct * thisclass_c, float A_c, float B_c, float C_c, float D_c)
{
  Ogre::RenderSystem * thisclass_cpp = static_cast<Ogre::RenderSystem*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderSystem"));
  Real A_cpp = (Real)A_c;
  Real B_cpp = (Real)B_c;
  Real C_cpp = (Real)C_c;
  Real D_cpp = (Real)D_c;
  (thisclass_cpp->addClipPlane(A_cpp, B_cpp, C_cpp, D_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rds_resetClipPlanes(struct hg3dclass_struct * thisclass_c)
{
  Ogre::RenderSystem * thisclass_cpp = static_cast<Ogre::RenderSystem*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderSystem"));
  (thisclass_cpp->resetClipPlanes());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rds_setInvertVertexWinding(struct hg3dclass_struct * thisclass_c, int invert_c)
{
  Ogre::RenderSystem * thisclass_cpp = static_cast<Ogre::RenderSystem*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderSystem"));
  bool invert_cpp = (bool)invert_c;
  (thisclass_cpp->setInvertVertexWinding(invert_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rds_getInvertVertexWinding(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::RenderSystem * thisclass_cpp = static_cast<Ogre::RenderSystem*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderSystem"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->getInvertVertexWinding());
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rds_setScissorTest(struct hg3dclass_struct * thisclass_c, int enabled_c, int left_c, int top_c, int right_c, int bottom_c)
{
  Ogre::RenderSystem * thisclass_cpp = static_cast<Ogre::RenderSystem*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderSystem"));
  bool enabled_cpp = (bool)enabled_c;
  size_t left_cpp = (size_t)left_c;
  size_t top_cpp = (size_t)top_c;
  size_t right_cpp = (size_t)right_c;
  size_t bottom_cpp = (size_t)bottom_c;
  (thisclass_cpp->setScissorTest(enabled_cpp, left_cpp, top_cpp, right_cpp, bottom_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rds_clearFrameBuffer(struct hg3dclass_struct * thisclass_c, unsigned int buffers_c, struct colourvalue_struct * colour_c, float depth_c, unsigned short stencil_c)
{
  Ogre::RenderSystem * thisclass_cpp = static_cast<Ogre::RenderSystem*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderSystem"));
  unsigned int buffers_cpp = (unsigned int)buffers_c;
  ColourValue colour_cpp = *((ColourValue*) colour_c);
  Real depth_cpp = (Real)depth_c;
  unsigned short stencil_cpp = (unsigned short)stencil_c;
  (thisclass_cpp->clearFrameBuffer(buffers_cpp, colour_cpp, depth_cpp, stencil_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rds_getHorizontalTexelOffset(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  Ogre::RenderSystem * thisclass_cpp = static_cast<Ogre::RenderSystem*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderSystem"));
  Real result_cpp;
  result_cpp = (thisclass_cpp->getHorizontalTexelOffset());
  *result_c = (float)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rds_getVerticalTexelOffset(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  Ogre::RenderSystem * thisclass_cpp = static_cast<Ogre::RenderSystem*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderSystem"));
  Real result_cpp;
  result_cpp = (thisclass_cpp->getVerticalTexelOffset());
  *result_c = (float)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rds_getMinimumDepthInputValue(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  Ogre::RenderSystem * thisclass_cpp = static_cast<Ogre::RenderSystem*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderSystem"));
  Real result_cpp;
  result_cpp = (thisclass_cpp->getMinimumDepthInputValue());
  *result_c = (float)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rds_getMaximumDepthInputValue(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  Ogre::RenderSystem * thisclass_cpp = static_cast<Ogre::RenderSystem*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderSystem"));
  Real result_cpp;
  result_cpp = (thisclass_cpp->getMaximumDepthInputValue());
  *result_c = (float)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rds_setCurrentPassIterationCount(struct hg3dclass_struct * thisclass_c, const int count_c)
{
  Ogre::RenderSystem * thisclass_cpp = static_cast<Ogre::RenderSystem*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderSystem"));
  const size_t count_cpp = (size_t)count_c;
  (thisclass_cpp->setCurrentPassIterationCount(count_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rds_setDeriveDepthBias(struct hg3dclass_struct * thisclass_c, int derive_c, float baseValue_c, float multiplier_c, float slopeScale_c)
{
  Ogre::RenderSystem * thisclass_cpp = static_cast<Ogre::RenderSystem*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderSystem"));
  bool derive_cpp = (bool)derive_c;
  float baseValue_cpp = (float)baseValue_c;
  float multiplier_cpp = (float)multiplier_c;
  float slopeScale_cpp = (float)slopeScale_c;
  (thisclass_cpp->setDeriveDepthBias(derive_cpp, baseValue_cpp, multiplier_cpp, slopeScale_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rds_preExtraThreadsStarted(struct hg3dclass_struct * thisclass_c)
{
  Ogre::RenderSystem * thisclass_cpp = static_cast<Ogre::RenderSystem*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderSystem"));
  (thisclass_cpp->preExtraThreadsStarted());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rds_postExtraThreadsStarted(struct hg3dclass_struct * thisclass_c)
{
  Ogre::RenderSystem * thisclass_cpp = static_cast<Ogre::RenderSystem*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderSystem"));
  (thisclass_cpp->postExtraThreadsStarted());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rds_registerThread(struct hg3dclass_struct * thisclass_c)
{
  Ogre::RenderSystem * thisclass_cpp = static_cast<Ogre::RenderSystem*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderSystem"));
  (thisclass_cpp->registerThread());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rds_unregisterThread(struct hg3dclass_struct * thisclass_c)
{
  Ogre::RenderSystem * thisclass_cpp = static_cast<Ogre::RenderSystem*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderSystem"));
  (thisclass_cpp->unregisterThread());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rds_getDisplayMonitorCount(struct hg3dclass_struct * thisclass_c, unsigned int * result_c)
{
  Ogre::RenderSystem * thisclass_cpp = static_cast<Ogre::RenderSystem*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderSystem"));
  unsigned int result_cpp;
  result_cpp = (thisclass_cpp->getDisplayMonitorCount());
  *result_c = (unsigned int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rds_beginProfileEvent(struct hg3dclass_struct * thisclass_c, char * eventName_c)
{
  Ogre::RenderSystem * thisclass_cpp = static_cast<Ogre::RenderSystem*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderSystem"));
  Ogre::String eventName_cpp = Ogre::String((const char*) eventName_c);
  (thisclass_cpp->beginProfileEvent(eventName_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rds_endProfileEvent(struct hg3dclass_struct * thisclass_c)
{
  Ogre::RenderSystem * thisclass_cpp = static_cast<Ogre::RenderSystem*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderSystem"));
  (thisclass_cpp->endProfileEvent());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rds_markProfileEvent(struct hg3dclass_struct * thisclass_c, char * event_c)
{
  Ogre::RenderSystem * thisclass_cpp = static_cast<Ogre::RenderSystem*> (getHG3DClassPtr(*thisclass_c, "Ogre::RenderSystem"));
  Ogre::String event_cpp = Ogre::String((const char*) event_c);
  (thisclass_cpp->markProfileEvent(event_cpp));
};

