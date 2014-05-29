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

// ClassRenderable.cpp

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
	#include "StructSharedPtr.h"
#include "./Ogre.h"
#include "./OgreString.h"
#include "./WindowUtilsHG3D.h"
#include "HG3DUtilities.h"

using namespace Ogre;
using Ogre::uint;



// 
extern "C" Ogre_LIB_EXPORT void ogre_rndl_destruct(struct hg3dclass_struct * thisclass_c)
{
  Ogre::Renderable * thisclass_cpp = static_cast<Ogre::Renderable*> (getHG3DClassPtr(*thisclass_c, "Ogre::Renderable"));
  (delete thisclass_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rndl_getMaterial(struct hg3dclass_struct * thisclass_c, struct sharedptr_struct * result_c)
{
  Ogre::Renderable * thisclass_cpp = static_cast<Ogre::Renderable*> (getHG3DClassPtr(*thisclass_c, "Ogre::Renderable"));
  MaterialPtr result_cpp;
  result_cpp = (thisclass_cpp->getMaterial());
  *result_c = *((struct sharedptr_struct*) &result_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rndl_preRender(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * sm_c, struct hg3dclass_struct * rsys_c, long * result_c)
{
  Ogre::Renderable * thisclass_cpp = static_cast<Ogre::Renderable*> (getHG3DClassPtr(*thisclass_c, "Ogre::Renderable"));
  Ogre::SceneManager * sm_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*sm_c, "Ogre::SceneManager"));
  Ogre::RenderSystem * rsys_cpp = static_cast<Ogre::RenderSystem*> (getHG3DClassPtr(*rsys_c, "Ogre::RenderSystem"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->preRender(sm_cpp, rsys_cpp));
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rndl_postRender(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * sm_c, struct hg3dclass_struct * rsys_c)
{
  Ogre::Renderable * thisclass_cpp = static_cast<Ogre::Renderable*> (getHG3DClassPtr(*thisclass_c, "Ogre::Renderable"));
  Ogre::SceneManager * sm_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*sm_c, "Ogre::SceneManager"));
  Ogre::RenderSystem * rsys_cpp = static_cast<Ogre::RenderSystem*> (getHG3DClassPtr(*rsys_c, "Ogre::RenderSystem"));
  (thisclass_cpp->postRender(sm_cpp, rsys_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rndl_getNumWorldTransforms(struct hg3dclass_struct * thisclass_c, unsigned short * result_c)
{
  Ogre::Renderable * thisclass_cpp = static_cast<Ogre::Renderable*> (getHG3DClassPtr(*thisclass_c, "Ogre::Renderable"));
  unsigned short result_cpp;
  result_cpp = (thisclass_cpp->getNumWorldTransforms());
  *result_c = (unsigned short)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rndl_setUseIdentityProjection(struct hg3dclass_struct * thisclass_c, long useIdentityProjection_c)
{
  Ogre::Renderable * thisclass_cpp = static_cast<Ogre::Renderable*> (getHG3DClassPtr(*thisclass_c, "Ogre::Renderable"));
  bool useIdentityProjection_cpp = (bool)useIdentityProjection_c;
  (thisclass_cpp->setUseIdentityProjection(useIdentityProjection_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rndl_getUseIdentityProjection(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::Renderable * thisclass_cpp = static_cast<Ogre::Renderable*> (getHG3DClassPtr(*thisclass_c, "Ogre::Renderable"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->getUseIdentityProjection());
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rndl_setUseIdentityView(struct hg3dclass_struct * thisclass_c, long useIdentityView_c)
{
  Ogre::Renderable * thisclass_cpp = static_cast<Ogre::Renderable*> (getHG3DClassPtr(*thisclass_c, "Ogre::Renderable"));
  bool useIdentityView_cpp = (bool)useIdentityView_c;
  (thisclass_cpp->setUseIdentityView(useIdentityView_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rndl_getUseIdentityView(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::Renderable * thisclass_cpp = static_cast<Ogre::Renderable*> (getHG3DClassPtr(*thisclass_c, "Ogre::Renderable"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->getUseIdentityView());
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rndl_getSquaredViewDepth(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * cam_c, float * result_c)
{
  Ogre::Renderable * thisclass_cpp = static_cast<Ogre::Renderable*> (getHG3DClassPtr(*thisclass_c, "Ogre::Renderable"));
  const Ogre::Camera * cam_cpp = static_cast<Ogre::Camera*> (getHG3DClassPtr(*cam_c, "Ogre::Camera"));
  Real result_cpp;
  result_cpp = (thisclass_cpp->getSquaredViewDepth(cam_cpp));
  *result_c = (float)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rndl_getCastsShadows(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::Renderable * thisclass_cpp = static_cast<Ogre::Renderable*> (getHG3DClassPtr(*thisclass_c, "Ogre::Renderable"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->getCastsShadows());
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rndl_removeCustomParameter(struct hg3dclass_struct * thisclass_c, long index_c)
{
  Ogre::Renderable * thisclass_cpp = static_cast<Ogre::Renderable*> (getHG3DClassPtr(*thisclass_c, "Ogre::Renderable"));
  size_t index_cpp = (size_t)index_c;
  (thisclass_cpp->removeCustomParameter(index_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rndl_hasCustomParameter(struct hg3dclass_struct * thisclass_c, long index_c, long * result_c)
{
  Ogre::Renderable * thisclass_cpp = static_cast<Ogre::Renderable*> (getHG3DClassPtr(*thisclass_c, "Ogre::Renderable"));
  size_t index_cpp = (size_t)index_c;
  bool result_cpp;
  result_cpp = (thisclass_cpp->hasCustomParameter(index_cpp));
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rndl_setPolygonModeOverrideable(struct hg3dclass_struct * thisclass_c, long override_c)
{
  Ogre::Renderable * thisclass_cpp = static_cast<Ogre::Renderable*> (getHG3DClassPtr(*thisclass_c, "Ogre::Renderable"));
  bool override_cpp = (bool)override_c;
  (thisclass_cpp->setPolygonModeOverrideable(override_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rndl_getPolygonModeOverrideable(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::Renderable * thisclass_cpp = static_cast<Ogre::Renderable*> (getHG3DClassPtr(*thisclass_c, "Ogre::Renderable"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->getPolygonModeOverrideable());
  *result_c = (long)result_cpp;
};

