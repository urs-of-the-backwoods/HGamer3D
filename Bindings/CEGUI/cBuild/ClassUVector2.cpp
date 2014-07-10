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

// ClassUVector2.cpp

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
#include "HG3DCommandHandler.h"
#include "HG3DEventStaticFunctions.h"
#include "HG3DListboxStaticFunctions.h"
#include "HG3DEventController.h"
#include "HG3DEventModule.h"
#include "HG3DWindowStaticFunctions.h"

using namespace CEGUI;



// 
extern "C" CEGUI_LIB_EXPORT void cegui_uv2_construct(struct hg3dclass_struct * x_c, struct hg3dclass_struct * y_c, struct hg3dclass_struct * result_c)
{
  const CEGUI::UDim * x_cpp = static_cast<CEGUI::UDim*> (getHG3DClassPtr(*x_c, "CEGUI::UDim"));
  const CEGUI::UDim * y_cpp = static_cast<CEGUI::UDim*> (getHG3DClassPtr(*y_c, "CEGUI::UDim"));
  CEGUI::UVector2 * result_cpp;
  result_cpp = (new CEGUI::UVector2(*x_cpp, *y_cpp));
  *result_c = getHG3DClass_UVector2((void *) result_cpp);
;
};

// 
extern "C" CEGUI_LIB_EXPORT void cegui_uv2_destruct(struct hg3dclass_struct * thisclass_c)
{
  CEGUI::UVector2 * thisclass_cpp = static_cast<CEGUI::UVector2*> (getHG3DClassPtr(*thisclass_c, "CEGUI::UVector2"));
  (delete thisclass_cpp);
};

