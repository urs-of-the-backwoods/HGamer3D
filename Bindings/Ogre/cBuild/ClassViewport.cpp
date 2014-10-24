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

// ClassViewport.cpp

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
#include "EnumOrientationMode.h"
#include "StructVec2.h"
#include "./Ogre.h"
#include "./OgreString.h"
#include "./WindowUtilsHG3D.h"
#include "HG3DUtilities.h"

using namespace Ogre;
using Ogre::uint;



// 
extern "C" Ogre_LIB_EXPORT void ogre_vprt_construct(struct hg3dclass_struct * camera_c, struct hg3dclass_struct * target_c, float left_c, float top_c, float width_c, float height_c, long ZOrder_c, struct hg3dclass_struct * result_c)
{
  Ogre::Camera * camera_cpp = static_cast<Ogre::Camera*> (getHG3DClassPtr(*camera_c, "Ogre::Camera"));
  Ogre::RenderTarget * target_cpp = static_cast<Ogre::RenderTarget*> (getHG3DClassPtr(*target_c, "Ogre::RenderTarget"));
  Real left_cpp = (Real)left_c;
  Real top_cpp = (Real)top_c;
  Real width_cpp = (Real)width_c;
  Real height_cpp = (Real)height_c;
  int ZOrder_cpp = (int)ZOrder_c;
  Ogre::Viewport * result_cpp;
  result_cpp = (new Ogre::Viewport(camera_cpp, target_cpp, left_cpp, top_cpp, width_cpp, height_cpp, ZOrder_cpp));
  *result_c = getHG3DClass_Viewport((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_vprt_destruct(struct hg3dclass_struct * thisclass_c)
{
  Ogre::Viewport * thisclass_cpp = static_cast<Ogre::Viewport*> (getHG3DClassPtr(*thisclass_c, "Ogre::Viewport"));
  (delete thisclass_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_vprt_update(struct hg3dclass_struct * thisclass_c)
{
  Ogre::Viewport * thisclass_cpp = static_cast<Ogre::Viewport*> (getHG3DClassPtr(*thisclass_c, "Ogre::Viewport"));
  (thisclass_cpp->update());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_vprt_clear(struct hg3dclass_struct * thisclass_c, unsigned long buffers_c, struct colourvalue_struct * colour_c, float depth_c, unsigned short stencil_c)
{
  Ogre::Viewport * thisclass_cpp = static_cast<Ogre::Viewport*> (getHG3DClassPtr(*thisclass_c, "Ogre::Viewport"));
  unsigned int buffers_cpp = (unsigned int)buffers_c;
  ColourValue colour_cpp = *((ColourValue*) colour_c);
  Real depth_cpp = (Real)depth_c;
  unsigned short stencil_cpp = (unsigned short)stencil_c;
  (thisclass_cpp->clear(buffers_cpp, colour_cpp, depth_cpp, stencil_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_vprt_getTarget(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  Ogre::Viewport * thisclass_cpp = static_cast<Ogre::Viewport*> (getHG3DClassPtr(*thisclass_c, "Ogre::Viewport"));
  Ogre::RenderTarget * result_cpp;
  result_cpp = (thisclass_cpp->getTarget());
  *result_c = getHG3DClass_RenderTarget((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_vprt_getCamera(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  Ogre::Viewport * thisclass_cpp = static_cast<Ogre::Viewport*> (getHG3DClassPtr(*thisclass_c, "Ogre::Viewport"));
  Ogre::Camera * result_cpp;
  result_cpp = (thisclass_cpp->getCamera());
  *result_c = getHG3DClass_Camera((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_vprt_setCamera(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * cam_c)
{
  Ogre::Viewport * thisclass_cpp = static_cast<Ogre::Viewport*> (getHG3DClassPtr(*thisclass_c, "Ogre::Viewport"));
  Ogre::Camera * cam_cpp = static_cast<Ogre::Camera*> (getHG3DClassPtr(*cam_c, "Ogre::Camera"));
  (thisclass_cpp->setCamera(cam_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_vprt_getZOrder(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::Viewport * thisclass_cpp = static_cast<Ogre::Viewport*> (getHG3DClassPtr(*thisclass_c, "Ogre::Viewport"));
  int result_cpp;
  result_cpp = (thisclass_cpp->getZOrder());
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_vprt_getLeft(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  Ogre::Viewport * thisclass_cpp = static_cast<Ogre::Viewport*> (getHG3DClassPtr(*thisclass_c, "Ogre::Viewport"));
  Real result_cpp;
  result_cpp = (thisclass_cpp->getLeft());
  *result_c = (float)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_vprt_getTop(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  Ogre::Viewport * thisclass_cpp = static_cast<Ogre::Viewport*> (getHG3DClassPtr(*thisclass_c, "Ogre::Viewport"));
  Real result_cpp;
  result_cpp = (thisclass_cpp->getTop());
  *result_c = (float)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_vprt_getWidth(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  Ogre::Viewport * thisclass_cpp = static_cast<Ogre::Viewport*> (getHG3DClassPtr(*thisclass_c, "Ogre::Viewport"));
  Real result_cpp;
  result_cpp = (thisclass_cpp->getWidth());
  *result_c = (float)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_vprt_getHeight(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  Ogre::Viewport * thisclass_cpp = static_cast<Ogre::Viewport*> (getHG3DClassPtr(*thisclass_c, "Ogre::Viewport"));
  Real result_cpp;
  result_cpp = (thisclass_cpp->getHeight());
  *result_c = (float)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_vprt_getActualLeft(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::Viewport * thisclass_cpp = static_cast<Ogre::Viewport*> (getHG3DClassPtr(*thisclass_c, "Ogre::Viewport"));
  int result_cpp;
  result_cpp = (thisclass_cpp->getActualLeft());
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_vprt_getActualTop(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::Viewport * thisclass_cpp = static_cast<Ogre::Viewport*> (getHG3DClassPtr(*thisclass_c, "Ogre::Viewport"));
  int result_cpp;
  result_cpp = (thisclass_cpp->getActualTop());
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_vprt_getActualWidth(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::Viewport * thisclass_cpp = static_cast<Ogre::Viewport*> (getHG3DClassPtr(*thisclass_c, "Ogre::Viewport"));
  int result_cpp;
  result_cpp = (thisclass_cpp->getActualWidth());
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_vprt_getActualHeight(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::Viewport * thisclass_cpp = static_cast<Ogre::Viewport*> (getHG3DClassPtr(*thisclass_c, "Ogre::Viewport"));
  int result_cpp;
  result_cpp = (thisclass_cpp->getActualHeight());
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_vprt_setDimensions(struct hg3dclass_struct * thisclass_c, float left_c, float top_c, float width_c, float height_c)
{
  Ogre::Viewport * thisclass_cpp = static_cast<Ogre::Viewport*> (getHG3DClassPtr(*thisclass_c, "Ogre::Viewport"));
  Real left_cpp = (Real)left_c;
  Real top_cpp = (Real)top_c;
  Real width_cpp = (Real)width_c;
  Real height_cpp = (Real)height_c;
  (thisclass_cpp->setDimensions(left_cpp, top_cpp, width_cpp, height_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_vprt_setOrientationMode(struct hg3dclass_struct * thisclass_c, enum EnumOrientationMode orientationMode_c, long setDefault_c)
{
  Ogre::Viewport * thisclass_cpp = static_cast<Ogre::Viewport*> (getHG3DClassPtr(*thisclass_c, "Ogre::Viewport"));
  enum Ogre::OrientationMode orientationMode_cpp = (enum Ogre::OrientationMode)orientationMode_c;
  bool setDefault_cpp = (bool)setDefault_c;
  (thisclass_cpp->setOrientationMode(orientationMode_cpp, setDefault_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_vprt_getOrientationMode(struct hg3dclass_struct * thisclass_c, enum EnumOrientationMode * result_c)
{
  Ogre::Viewport * thisclass_cpp = static_cast<Ogre::Viewport*> (getHG3DClassPtr(*thisclass_c, "Ogre::Viewport"));
  enum Ogre::OrientationMode result_cpp;
  result_cpp = (thisclass_cpp->getOrientationMode());
  *result_c = (enum EnumOrientationMode) result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_vprt_setBackgroundColour(struct hg3dclass_struct * thisclass_c, struct colourvalue_struct * colour_c)
{
  Ogre::Viewport * thisclass_cpp = static_cast<Ogre::Viewport*> (getHG3DClassPtr(*thisclass_c, "Ogre::Viewport"));
  ColourValue colour_cpp = *((ColourValue*) colour_c);
  (thisclass_cpp->setBackgroundColour(colour_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_vprt_getBackgroundColour(struct hg3dclass_struct * thisclass_c, struct colourvalue_struct * result_c)
{
  Ogre::Viewport * thisclass_cpp = static_cast<Ogre::Viewport*> (getHG3DClassPtr(*thisclass_c, "Ogre::Viewport"));
  ColourValue result_cpp;
  result_cpp = (thisclass_cpp->getBackgroundColour());
  *result_c = *((struct colourvalue_struct*) &result_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_vprt_setDepthClear(struct hg3dclass_struct * thisclass_c, float depth_c)
{
  Ogre::Viewport * thisclass_cpp = static_cast<Ogre::Viewport*> (getHG3DClassPtr(*thisclass_c, "Ogre::Viewport"));
  Real depth_cpp = (Real)depth_c;
  (thisclass_cpp->setDepthClear(depth_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_vprt_getDepthClear(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  Ogre::Viewport * thisclass_cpp = static_cast<Ogre::Viewport*> (getHG3DClassPtr(*thisclass_c, "Ogre::Viewport"));
  Real result_cpp;
  result_cpp = (thisclass_cpp->getDepthClear());
  *result_c = (float)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_vprt_setClearEveryFrame(struct hg3dclass_struct * thisclass_c, long clear_c, unsigned long buffers_c)
{
  Ogre::Viewport * thisclass_cpp = static_cast<Ogre::Viewport*> (getHG3DClassPtr(*thisclass_c, "Ogre::Viewport"));
  bool clear_cpp = (bool)clear_c;
  unsigned int buffers_cpp = (unsigned int)buffers_c;
  (thisclass_cpp->setClearEveryFrame(clear_cpp, buffers_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_vprt_getClearEveryFrame(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::Viewport * thisclass_cpp = static_cast<Ogre::Viewport*> (getHG3DClassPtr(*thisclass_c, "Ogre::Viewport"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->getClearEveryFrame());
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_vprt_getClearBuffers(struct hg3dclass_struct * thisclass_c, unsigned long * result_c)
{
  Ogre::Viewport * thisclass_cpp = static_cast<Ogre::Viewport*> (getHG3DClassPtr(*thisclass_c, "Ogre::Viewport"));
  unsigned int result_cpp;
  result_cpp = (thisclass_cpp->getClearBuffers());
  *result_c = (unsigned long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_vprt_setAutoUpdated(struct hg3dclass_struct * thisclass_c, long autoupdate_c)
{
  Ogre::Viewport * thisclass_cpp = static_cast<Ogre::Viewport*> (getHG3DClassPtr(*thisclass_c, "Ogre::Viewport"));
  bool autoupdate_cpp = (bool)autoupdate_c;
  (thisclass_cpp->setAutoUpdated(autoupdate_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_vprt_isAutoUpdated(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::Viewport * thisclass_cpp = static_cast<Ogre::Viewport*> (getHG3DClassPtr(*thisclass_c, "Ogre::Viewport"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isAutoUpdated());
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_vprt_setMaterialScheme(struct hg3dclass_struct * thisclass_c, char * schemeName_c)
{
  Ogre::Viewport * thisclass_cpp = static_cast<Ogre::Viewport*> (getHG3DClassPtr(*thisclass_c, "Ogre::Viewport"));
  Ogre::String schemeName_cpp = Ogre::String((const char*) schemeName_c);
  (thisclass_cpp->setMaterialScheme(schemeName_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_vprt_getMaterialScheme(struct hg3dclass_struct * thisclass_c, char * result_c)
{
  Ogre::Viewport * thisclass_cpp = static_cast<Ogre::Viewport*> (getHG3DClassPtr(*thisclass_c, "Ogre::Viewport"));
  Ogre::String result_cpp;
  result_cpp = (thisclass_cpp->getMaterialScheme());
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_vprt_getActualDimensions(struct hg3dclass_struct * thisclass_c, long * left_c, long * top_c, long * width_c, long * height_c)
{
  Ogre::Viewport * thisclass_cpp = static_cast<Ogre::Viewport*> (getHG3DClassPtr(*thisclass_c, "Ogre::Viewport"));
  int left_cpp;
  int top_cpp;
  int width_cpp;
  int height_cpp;
  (thisclass_cpp->getActualDimensions(left_cpp, top_cpp, width_cpp, height_cpp));
  *left_c = (long)left_cpp;
  *top_c = (long)top_cpp;
  *width_c = (long)width_cpp;
  *height_c = (long)height_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_vprt_setOverlaysEnabled(struct hg3dclass_struct * thisclass_c, long enabled_c)
{
  Ogre::Viewport * thisclass_cpp = static_cast<Ogre::Viewport*> (getHG3DClassPtr(*thisclass_c, "Ogre::Viewport"));
  bool enabled_cpp = (bool)enabled_c;
  (thisclass_cpp->setOverlaysEnabled(enabled_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_vprt_getOverlaysEnabled(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::Viewport * thisclass_cpp = static_cast<Ogre::Viewport*> (getHG3DClassPtr(*thisclass_c, "Ogre::Viewport"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->getOverlaysEnabled());
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_vprt_setSkiesEnabled(struct hg3dclass_struct * thisclass_c, long enabled_c)
{
  Ogre::Viewport * thisclass_cpp = static_cast<Ogre::Viewport*> (getHG3DClassPtr(*thisclass_c, "Ogre::Viewport"));
  bool enabled_cpp = (bool)enabled_c;
  (thisclass_cpp->setSkiesEnabled(enabled_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_vprt_getSkiesEnabled(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::Viewport * thisclass_cpp = static_cast<Ogre::Viewport*> (getHG3DClassPtr(*thisclass_c, "Ogre::Viewport"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->getSkiesEnabled());
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_vprt_setShadowsEnabled(struct hg3dclass_struct * thisclass_c, long enabled_c)
{
  Ogre::Viewport * thisclass_cpp = static_cast<Ogre::Viewport*> (getHG3DClassPtr(*thisclass_c, "Ogre::Viewport"));
  bool enabled_cpp = (bool)enabled_c;
  (thisclass_cpp->setShadowsEnabled(enabled_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_vprt_getShadowsEnabled(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::Viewport * thisclass_cpp = static_cast<Ogre::Viewport*> (getHG3DClassPtr(*thisclass_c, "Ogre::Viewport"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->getShadowsEnabled());
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_vprt_setVisibilityMask(struct hg3dclass_struct * thisclass_c, unsigned long mask_c)
{
  Ogre::Viewport * thisclass_cpp = static_cast<Ogre::Viewport*> (getHG3DClassPtr(*thisclass_c, "Ogre::Viewport"));
  uint32 mask_cpp = (uint32)mask_c;
  (thisclass_cpp->setVisibilityMask(mask_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_vprt_getVisibilityMask(struct hg3dclass_struct * thisclass_c, unsigned long * result_c)
{
  Ogre::Viewport * thisclass_cpp = static_cast<Ogre::Viewport*> (getHG3DClassPtr(*thisclass_c, "Ogre::Viewport"));
  uint result_cpp;
  result_cpp = (thisclass_cpp->getVisibilityMask());
  *result_c = (unsigned long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_vprt_setRenderQueueInvocationSequenceName(struct hg3dclass_struct * thisclass_c, char * sequenceName_c)
{
  Ogre::Viewport * thisclass_cpp = static_cast<Ogre::Viewport*> (getHG3DClassPtr(*thisclass_c, "Ogre::Viewport"));
  Ogre::String sequenceName_cpp = Ogre::String((const char*) sequenceName_c);
  (thisclass_cpp->setRenderQueueInvocationSequenceName(sequenceName_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_vprt_getRenderQueueInvocationSequenceName(struct hg3dclass_struct * thisclass_c, char * result_c)
{
  Ogre::Viewport * thisclass_cpp = static_cast<Ogre::Viewport*> (getHG3DClassPtr(*thisclass_c, "Ogre::Viewport"));
  Ogre::String result_cpp;
  result_cpp = (thisclass_cpp->getRenderQueueInvocationSequenceName());
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_vprt_pointOrientedToScreen(struct hg3dclass_struct * thisclass_c, struct vector2_struct * v_c, long orientationMode_c, struct vector2_struct * outv_c)
{
  Ogre::Viewport * thisclass_cpp = static_cast<Ogre::Viewport*> (getHG3DClassPtr(*thisclass_c, "Ogre::Viewport"));
  Vector2 v_cpp = *((Vector2*) v_c);
  int orientationMode_cpp = (int)orientationMode_c;
  Vector2 outv_cpp;
  (thisclass_cpp->pointOrientedToScreen(v_cpp, orientationMode_cpp, outv_cpp));
  *outv_c = *((struct vector2_struct*) &outv_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_vprt_pointOrientedToScreen2(struct hg3dclass_struct * thisclass_c, float orientedX_c, float orientedY_c, long orientationMode_c, float * screenX_c, float * screenY_c)
{
  Ogre::Viewport * thisclass_cpp = static_cast<Ogre::Viewport*> (getHG3DClassPtr(*thisclass_c, "Ogre::Viewport"));
  Real orientedX_cpp = (Real)orientedX_c;
  Real orientedY_cpp = (Real)orientedY_c;
  int orientationMode_cpp = (int)orientationMode_c;
  Real screenX_cpp;
  Real screenY_cpp;
  (thisclass_cpp->pointOrientedToScreen(orientedX_cpp, orientedY_cpp, orientationMode_cpp, screenX_cpp, screenY_cpp));
  *screenX_c = (float)screenX_cpp;
  *screenY_c = (float)screenY_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_vprt_setDefaultOrientationMode(enum EnumOrientationMode orientationMode_c)
{
  enum Ogre::OrientationMode orientationMode_cpp = (enum Ogre::OrientationMode)orientationMode_c;
  (Ogre::Viewport::setDefaultOrientationMode(orientationMode_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_vprt_getDefaultOrientationMode(enum EnumOrientationMode * result_c)
{
  enum Ogre::OrientationMode result_cpp;
  result_cpp = (Ogre::Viewport::getDefaultOrientationMode());
  *result_c = (enum EnumOrientationMode) result_cpp;
};

