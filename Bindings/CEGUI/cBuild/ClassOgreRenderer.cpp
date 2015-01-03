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

// ClassOgreRenderer.cpp

// 

#include <wchar.h>
#include <string>
#include <iostream>

#include <iostream>
	#include <typeinfo>
	#include <stdio.h>
	#include <cstring>
	#include <exception>
	#include "CEGUIDllDefines.h"
	#include "ClassPtr.h"
	#include "EnumBlendMode.h"
#include "./CEGUI.h"
#include "./CEGUIString.h"
#include "RendererModules/Ogre/CEGUIOgreRenderer.h"
#include "./WindowManagerHG3D.h"
#include "./SystemHG3D.h"
#include "HG3DCommandHandler.h"
#include "HG3DEventController.h"
#include "HG3DEventModule.h"
#include "HG3DEventStaticFunctions.h"
#include "HG3DListboxStaticFunctions.h"
#include "HG3DWindowStaticFunctions.h"

using namespace CEGUI;



// Convenience function that creates all the OgreCEGUI
extern "C" CEGUI_LIB_EXPORT void cegui_ogrrndr_bootstrapSystem(struct hg3dclass_struct * result_c)
{
  CEGUI::OgreRenderer * result_cpp;
  result_cpp = &(CEGUI::OgreRenderer::bootstrapSystem());
  *result_c = getHG3DClass_OgreRenderer((void *) result_cpp);
;
};

// Convenience function to cleanup the CEGUI
extern "C" CEGUI_LIB_EXPORT void cegui_ogrrndr_destroySystem()
{
  (CEGUI::OgreRenderer::destroySystem());
};

// Create an OgreRendererOgre
extern "C" CEGUI_LIB_EXPORT void cegui_ogrrndr_create(struct hg3dclass_struct * result_c)
{
  CEGUI::OgreRenderer * result_cpp;
  result_cpp = &(CEGUI::OgreRenderer::create());
  *result_c = getHG3DClass_OgreRenderer((void *) result_cpp);
;
};

// destory an OgreRenderer
extern "C" CEGUI_LIB_EXPORT void cegui_ogrrndr_destroy(struct hg3dclass_struct * renderer_c)
{
  CEGUI::OgreRenderer * renderer_cpp = static_cast<CEGUI::OgreRenderer*> (getHG3DClassPtr(*renderer_c, "CEGUI::OgreRenderer"));
  (CEGUI::OgreRenderer::destroy(*renderer_cpp));
};

// function to create a CEGUI::OgreResourceProvider
extern "C" CEGUI_LIB_EXPORT void cegui_ogrrndr_createOgreResourceProvider(struct hg3dclass_struct * result_c)
{
  CEGUI::OgreResourceProvider * result_cpp;
  result_cpp = &(CEGUI::OgreRenderer::createOgreResourceProvider());
  *result_c = getHG3DClass_OgreResourceProvider((void *) result_cpp);
;
};

// function to destroy a CEGUI::OgreResourceProvider
extern "C" CEGUI_LIB_EXPORT void cegui_ogrrndr_destroyOgreResourceProvider(struct hg3dclass_struct * rp_c)
{
  CEGUI::OgreResourceProvider * rp_cpp = static_cast<CEGUI::OgreResourceProvider*> (getHG3DClassPtr(*rp_c, "CEGUI::OgreResourceProvider"));
  (CEGUI::OgreRenderer::destroyOgreResourceProvider(*rp_cpp));
};

// set whether CEGUI
extern "C" CEGUI_LIB_EXPORT void cegui_ogrrndr_setRenderingEnabled(struct hg3dclass_struct * thisclass_c, const int enabled_c)
{
  CEGUI::OgreRenderer * thisclass_cpp = static_cast<CEGUI::OgreRenderer*> (getHG3DClassPtr(*thisclass_c, "CEGUI::OgreRenderer"));
  const bool enabled_cpp = (bool)enabled_c;
  (thisclass_cpp->setRenderingEnabled(enabled_cpp));
};

// return whether CEGUI
extern "C" CEGUI_LIB_EXPORT void cegui_ogrrndr_isRenderingEnabled(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::OgreRenderer * thisclass_cpp = static_cast<CEGUI::OgreRenderer*> (getHG3DClassPtr(*thisclass_c, "CEGUI::OgreRenderer"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isRenderingEnabled());
  *result_c = (int)result_cpp;
};

// set the render states for the specified BlendMode. 
extern "C" CEGUI_LIB_EXPORT void cegui_ogrrndr_setupRenderingBlendMode(struct hg3dclass_struct * thisclass_c, const enum EnumBlendMode mode_c, const int force_c)
{
  CEGUI::OgreRenderer * thisclass_cpp = static_cast<CEGUI::OgreRenderer*> (getHG3DClassPtr(*thisclass_c, "CEGUI::OgreRenderer"));
  enum CEGUI::BlendMode mode_cpp = (enum CEGUI::BlendMode)mode_c;
  const bool force_cpp = (bool)force_c;
  (thisclass_cpp->setupRenderingBlendMode(mode_cpp, force_cpp));
};

// Controls whether rendering done by CEGUI
extern "C" CEGUI_LIB_EXPORT void cegui_ogrrndr_setFrameControlExecutionEnabled(struct hg3dclass_struct * thisclass_c, const int enabled_c)
{
  CEGUI::OgreRenderer * thisclass_cpp = static_cast<CEGUI::OgreRenderer*> (getHG3DClassPtr(*thisclass_c, "CEGUI::OgreRenderer"));
  const bool enabled_cpp = (bool)enabled_c;
  (thisclass_cpp->setFrameControlExecutionEnabled(enabled_cpp));
};

// Returns whether rendering done by CEGUI
extern "C" CEGUI_LIB_EXPORT void cegui_ogrrndr_isFrameControlExecutionEnabled(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  CEGUI::OgreRenderer * thisclass_cpp = static_cast<CEGUI::OgreRenderer*> (getHG3DClassPtr(*thisclass_c, "CEGUI::OgreRenderer"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isFrameControlExecutionEnabled());
  *result_c = (int)result_cpp;
};

// Sets all the required render states needed for CEGUI
extern "C" CEGUI_LIB_EXPORT void cegui_ogrrndr_initialiseRenderStateSettings(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::OgreRenderer * thisclass_cpp = static_cast<CEGUI::OgreRenderer*> (getHG3DClassPtr(*thisclass_c, "CEGUI::OgreRenderer"));
  (thisclass_cpp->initialiseRenderStateSettings());
};

// Destroy all GeometryBuffer objects created by this Renderer
extern "C" CEGUI_LIB_EXPORT void cegui_ogrrndr_destroyAllGeometryBuffers(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::OgreRenderer * thisclass_cpp = static_cast<CEGUI::OgreRenderer*> (getHG3DClassPtr(*thisclass_c, "CEGUI::OgreRenderer"));
  (thisclass_cpp->destroyAllGeometryBuffers());
};

// Destory all TextureTarget objects created by this Renderer
extern "C" CEGUI_LIB_EXPORT void cegui_ogrrndr_destroyAllTextureTargets(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::OgreRenderer * thisclass_cpp = static_cast<CEGUI::OgreRenderer*> (getHG3DClassPtr(*thisclass_c, "CEGUI::OgreRenderer"));
  (thisclass_cpp->destroyAllTextureTargets());
};

// Destroy all Texture objects created by this Renderer
extern "C" CEGUI_LIB_EXPORT void cegui_ogrrndr_destroyAllTextures(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::OgreRenderer * thisclass_cpp = static_cast<CEGUI::OgreRenderer*> (getHG3DClassPtr(*thisclass_c, "CEGUI::OgreRenderer"));
  (thisclass_cpp->destroyAllTextures());
};

// Perform any operations required to put the system into a state ready for rendering operations to begin. 
extern "C" CEGUI_LIB_EXPORT void cegui_ogrrndr_beginRendering(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::OgreRenderer * thisclass_cpp = static_cast<CEGUI::OgreRenderer*> (getHG3DClassPtr(*thisclass_c, "CEGUI::OgreRenderer"));
  (thisclass_cpp->beginRendering());
};

// Perform any operations required to finalise rendering. 
extern "C" CEGUI_LIB_EXPORT void cegui_ogrrndr_endRendering(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::OgreRenderer * thisclass_cpp = static_cast<CEGUI::OgreRenderer*> (getHG3DClassPtr(*thisclass_c, "CEGUI::OgreRenderer"));
  (thisclass_cpp->endRendering());
};

// Return the pixel size of the maximum supported texture. 
extern "C" CEGUI_LIB_EXPORT void cegui_ogrrndr_getMaxTextureSize(struct hg3dclass_struct * thisclass_c, unsigned int * result_c)
{
  CEGUI::OgreRenderer * thisclass_cpp = static_cast<CEGUI::OgreRenderer*> (getHG3DClassPtr(*thisclass_c, "CEGUI::OgreRenderer"));
  uint result_cpp;
  result_cpp = (thisclass_cpp->getMaxTextureSize());
  *result_c = (unsigned int)result_cpp;
};

// Return identification string for the renderer module. 
extern "C" CEGUI_LIB_EXPORT void cegui_ogrrndr_getIdentifierString(struct hg3dclass_struct * thisclass_c, char * result_c)
{
  CEGUI::OgreRenderer * thisclass_cpp = static_cast<CEGUI::OgreRenderer*> (getHG3DClassPtr(*thisclass_c, "CEGUI::OgreRenderer"));
  CEGUI::String result_cpp;
  result_cpp = (thisclass_cpp->getIdentifierString());
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

