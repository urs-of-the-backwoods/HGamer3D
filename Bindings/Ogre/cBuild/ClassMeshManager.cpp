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

// ClassMeshManager.cpp

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
extern "C" Ogre_LIB_EXPORT void ogre_mshmgr_construct(struct hg3dclass_struct * result_c)
{
  Ogre::MeshManager * result_cpp;
  result_cpp = (new Ogre::MeshManager());
  *result_c = getHG3DClass_MeshManager((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mshmgr_destruct(struct hg3dclass_struct * thisclass_c)
{
  Ogre::MeshManager * thisclass_cpp = static_cast<Ogre::MeshManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::MeshManager"));
  (delete thisclass_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mshmgr_setPrepareAllMeshesForShadowVolumes(struct hg3dclass_struct * thisclass_c, long enable_c)
{
  Ogre::MeshManager * thisclass_cpp = static_cast<Ogre::MeshManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::MeshManager"));
  bool enable_cpp = (bool)enable_c;
  (thisclass_cpp->setPrepareAllMeshesForShadowVolumes(enable_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mshmgr_getPrepareAllMeshesForShadowVolumes(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::MeshManager * thisclass_cpp = static_cast<Ogre::MeshManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::MeshManager"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->getPrepareAllMeshesForShadowVolumes());
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mshmgr_getBoundsPaddingFactor(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  Ogre::MeshManager * thisclass_cpp = static_cast<Ogre::MeshManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::MeshManager"));
  Real result_cpp;
  result_cpp = (thisclass_cpp->getBoundsPaddingFactor());
  *result_c = (float)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mshmgr_setBoundsPaddingFactor(struct hg3dclass_struct * thisclass_c, float paddingFactor_c)
{
  Ogre::MeshManager * thisclass_cpp = static_cast<Ogre::MeshManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::MeshManager"));
  Real paddingFactor_cpp = (Real)paddingFactor_c;
  (thisclass_cpp->setBoundsPaddingFactor(paddingFactor_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mshmgr_loadResource(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * res_c)
{
  Ogre::MeshManager * thisclass_cpp = static_cast<Ogre::MeshManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::MeshManager"));
  Ogre::Resource * res_cpp = static_cast<Ogre::Resource*> (getHG3DClassPtr(*res_c, "Ogre::Resource"));
  (thisclass_cpp->loadResource(res_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mshmgr_getSingleton(struct hg3dclass_struct * result_c)
{
  Ogre::MeshManager * result_cpp;
  result_cpp = &(Ogre::MeshManager::getSingleton());
  *result_c = getHG3DClass_MeshManager((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mshmgr_getSingletonPtr(struct hg3dclass_struct * result_c)
{
  Ogre::MeshManager * result_cpp;
  result_cpp = (Ogre::MeshManager::getSingletonPtr());
  *result_c = getHG3DClass_MeshManager((void *) result_cpp);
;
};

