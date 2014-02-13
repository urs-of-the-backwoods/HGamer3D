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

// ClassSystemHG3D.cpp

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
	#include "./CEGUI.h"
#include "./CEGUIString.h"
#include "RendererModules/Ogre/CEGUIOgreRenderer.h"
#include "./WindowManagerHG3D.h"
#include "./SystemHG3D.h"
#include "HG3DEventStaticFunctions.h"
#include "HG3DListboxStaticFunctions.h"
#include "HG3DEventController.h"
#include "HG3DCommandHandler.h"
#include "HG3DEventModule.h"
#include "HG3DWindowStaticFunctions.h"

using namespace CEGUI;



// 
extern "C" CEGUI_LIB_EXPORT void cegui_sstm_hg3d_createNoLogger(struct hg3dclass_struct * result_c)
{
  CEGUI::Logger * result_cpp;
  result_cpp = (SystemHG3D::createNoLogger());
  *result_c = getHG3DClass_Logger((void *) result_cpp);
;
};

// 
extern "C" CEGUI_LIB_EXPORT void cegui_sstm_hg3d_getDefaultResourceProvider(struct hg3dclass_struct * system_c, struct hg3dclass_struct * result_c)
{
  CEGUI::System * system_cpp = static_cast<CEGUI::System*> (getHG3DClassPtr(*system_c, "CEGUI::System"));
  CEGUI::DefaultResourceProvider * result_cpp;
  result_cpp = (SystemHG3D::getDefaultResourceProvider(system_cpp));
  *result_c = getHG3DClass_DefaultResourceProvider((void *) result_cpp);
;
};

// 
extern "C" CEGUI_LIB_EXPORT void cegui_sstm_hg3d_getSchemeManagerSingleton(struct hg3dclass_struct * result_c)
{
  CEGUI::SchemeManager * result_cpp;
  result_cpp = (SystemHG3D::getSchemeManagerSingleton());
  *result_c = getHG3DClass_SchemeManager((void *) result_cpp);
;
};

// 
extern "C" CEGUI_LIB_EXPORT void cegui_sstm_hg3d_schemeManagerCreate(struct hg3dclass_struct * smgr_c, char * scheme_c)
{
  CEGUI::SchemeManager * smgr_cpp = static_cast<CEGUI::SchemeManager*> (getHG3DClassPtr(*smgr_c, "CEGUI::SchemeManager"));
  const char * scheme_cpp = (char*) scheme_c;
  (SystemHG3D::schemeManagerCreate(smgr_cpp, scheme_cpp));
};

// 
extern "C" CEGUI_LIB_EXPORT void cegui_sstm_hg3d_fontManagerCreate(struct hg3dclass_struct * fmgr_c, char * font_c)
{
  CEGUI::FontManager * fmgr_cpp = static_cast<CEGUI::FontManager*> (getHG3DClassPtr(*fmgr_c, "CEGUI::FontManager"));
  const char * font_cpp = (char*) font_c;
  (SystemHG3D::fontManagerCreate(fmgr_cpp, font_cpp));
};

// 
extern "C" CEGUI_LIB_EXPORT void cegui_sstm_hg3d_getFontManagerSingleton(struct hg3dclass_struct * result_c)
{
  CEGUI::FontManager * result_cpp;
  result_cpp = (SystemHG3D::getFontManagerSingleton());
  *result_c = getHG3DClass_FontManager((void *) result_cpp);
;
};

// 
extern "C" CEGUI_LIB_EXPORT void cegui_sstm_hg3d_getLoggerSingleton(struct hg3dclass_struct * result_c)
{
  CEGUI::Logger * result_cpp;
  result_cpp = (SystemHG3D::getLoggerSingleton());
  *result_c = getHG3DClass_Logger((void *) result_cpp);
;
};

