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

// ClassHG3DUtilities.cpp

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
extern "C" Ogre_LIB_EXPORT void ogre_hg3dutl_getWindowHandle(struct hg3dclass_struct * window_c, unsigned long * result_c)
{
  Ogre::RenderWindow * window_cpp = static_cast<Ogre::RenderWindow*> (getHG3DClassPtr(*window_c, "Ogre::RenderWindow"));
  unsigned long result_cpp;
  result_cpp = (HG3DUtilities::getWindowHandle(window_cpp));
  *result_c = (unsigned long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_hg3dutl_setupCloseEventHandler(struct hg3dclass_struct * window_c)
{
  Ogre::RenderWindow * window_cpp = static_cast<Ogre::RenderWindow*> (getHG3DClassPtr(*window_c, "Ogre::RenderWindow"));
  (HG3DUtilities::setupCloseEventHandler(window_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_hg3dutl_checkQuitReceived(long * result_c)
{
  int result_cpp;
  result_cpp = (HG3DUtilities::checkQuitReceived());
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_hg3dutl_buildTangentVectors(struct hg3dclass_struct * entity_c)
{
  Ogre::Entity * entity_cpp = static_cast<Ogre::Entity*> (getHG3DClassPtr(*entity_c, "Ogre::Entity"));
  (HG3DUtilities::buildTangentVectors(entity_cpp));
};

