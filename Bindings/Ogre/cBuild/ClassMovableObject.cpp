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

// ClassMovableObject.cpp

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
extern "C" Ogre_LIB_EXPORT void ogre_mvo_destruct(struct hg3dclass_struct * thisclass_c)
{
  Ogre::MovableObject * thisclass_cpp = static_cast<Ogre::MovableObject*> (getHG3DClassPtr(*thisclass_c, "Ogre::MovableObject"));
  (delete thisclass_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mvo_getName(struct hg3dclass_struct * thisclass_c, char * result_c)
{
  Ogre::MovableObject * thisclass_cpp = static_cast<Ogre::MovableObject*> (getHG3DClassPtr(*thisclass_c, "Ogre::MovableObject"));
  Ogre::String result_cpp;
  result_cpp = (thisclass_cpp->getName());
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mvo_getMovableType(struct hg3dclass_struct * thisclass_c, char * result_c)
{
  Ogre::MovableObject * thisclass_cpp = static_cast<Ogre::MovableObject*> (getHG3DClassPtr(*thisclass_c, "Ogre::MovableObject"));
  Ogre::String result_cpp;
  result_cpp = (thisclass_cpp->getMovableType());
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mvo_getParentNode(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  Ogre::MovableObject * thisclass_cpp = static_cast<Ogre::MovableObject*> (getHG3DClassPtr(*thisclass_c, "Ogre::MovableObject"));
  Ogre::Node * result_cpp;
  result_cpp = (thisclass_cpp->getParentNode());
  *result_c = getHG3DClass_Node((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mvo_getParentSceneNode(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  Ogre::MovableObject * thisclass_cpp = static_cast<Ogre::MovableObject*> (getHG3DClassPtr(*thisclass_c, "Ogre::MovableObject"));
  Ogre::SceneNode * result_cpp;
  result_cpp = (thisclass_cpp->getParentSceneNode());
  *result_c = getHG3DClass_SceneNode((void *) result_cpp);
;
};

// Gets whether the parent node is a TagPoint (or a SceneNode
extern "C" Ogre_LIB_EXPORT void ogre_mvo_isParentTagPoint(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::MovableObject * thisclass_cpp = static_cast<Ogre::MovableObject*> (getHG3DClassPtr(*thisclass_c, "Ogre::MovableObject"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isParentTagPoint());
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mvo_isAttached(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::MovableObject * thisclass_cpp = static_cast<Ogre::MovableObject*> (getHG3DClassPtr(*thisclass_c, "Ogre::MovableObject"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isAttached());
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mvo_detachFromParent(struct hg3dclass_struct * thisclass_c)
{
  Ogre::MovableObject * thisclass_cpp = static_cast<Ogre::MovableObject*> (getHG3DClassPtr(*thisclass_c, "Ogre::MovableObject"));
  (thisclass_cpp->detachFromParent());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mvo_isInScene(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::MovableObject * thisclass_cpp = static_cast<Ogre::MovableObject*> (getHG3DClassPtr(*thisclass_c, "Ogre::MovableObject"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isInScene());
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mvo_getBoundingRadius(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  Ogre::MovableObject * thisclass_cpp = static_cast<Ogre::MovableObject*> (getHG3DClassPtr(*thisclass_c, "Ogre::MovableObject"));
  Real result_cpp;
  result_cpp = (thisclass_cpp->getBoundingRadius());
  *result_c = (float)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mvo_setVisible(struct hg3dclass_struct * thisclass_c, long visible_c)
{
  Ogre::MovableObject * thisclass_cpp = static_cast<Ogre::MovableObject*> (getHG3DClassPtr(*thisclass_c, "Ogre::MovableObject"));
  bool visible_cpp = (bool)visible_c;
  (thisclass_cpp->setVisible(visible_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mvo_getVisible(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::MovableObject * thisclass_cpp = static_cast<Ogre::MovableObject*> (getHG3DClassPtr(*thisclass_c, "Ogre::MovableObject"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->getVisible());
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mvo_isVisible(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::MovableObject * thisclass_cpp = static_cast<Ogre::MovableObject*> (getHG3DClassPtr(*thisclass_c, "Ogre::MovableObject"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isVisible());
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mvo_setRenderingDistance(struct hg3dclass_struct * thisclass_c, float dist_c)
{
  Ogre::MovableObject * thisclass_cpp = static_cast<Ogre::MovableObject*> (getHG3DClassPtr(*thisclass_c, "Ogre::MovableObject"));
  Real dist_cpp = (Real)dist_c;
  (thisclass_cpp->setRenderingDistance(dist_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mvo_getRenderingDistance(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  Ogre::MovableObject * thisclass_cpp = static_cast<Ogre::MovableObject*> (getHG3DClassPtr(*thisclass_c, "Ogre::MovableObject"));
  Real result_cpp;
  result_cpp = (thisclass_cpp->getRenderingDistance());
  *result_c = (float)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mvo_setRenderingMinPixelSize(struct hg3dclass_struct * thisclass_c, float pixelSize_c)
{
  Ogre::MovableObject * thisclass_cpp = static_cast<Ogre::MovableObject*> (getHG3DClassPtr(*thisclass_c, "Ogre::MovableObject"));
  Real pixelSize_cpp = (Real)pixelSize_c;
  (thisclass_cpp->setRenderingMinPixelSize(pixelSize_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mvo_getRenderingMinPixelSize(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  Ogre::MovableObject * thisclass_cpp = static_cast<Ogre::MovableObject*> (getHG3DClassPtr(*thisclass_c, "Ogre::MovableObject"));
  Real result_cpp;
  result_cpp = (thisclass_cpp->getRenderingMinPixelSize());
  *result_c = (float)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mvo_setQueryFlags(struct hg3dclass_struct * thisclass_c, unsigned long flags_c)
{
  Ogre::MovableObject * thisclass_cpp = static_cast<Ogre::MovableObject*> (getHG3DClassPtr(*thisclass_c, "Ogre::MovableObject"));
  uint32 flags_cpp = (uint32)flags_c;
  (thisclass_cpp->setQueryFlags(flags_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mvo_addQueryFlags(struct hg3dclass_struct * thisclass_c, unsigned long flags_c)
{
  Ogre::MovableObject * thisclass_cpp = static_cast<Ogre::MovableObject*> (getHG3DClassPtr(*thisclass_c, "Ogre::MovableObject"));
  uint32 flags_cpp = (uint32)flags_c;
  (thisclass_cpp->addQueryFlags(flags_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mvo_removeQueryFlags(struct hg3dclass_struct * thisclass_c, unsigned long flags_c)
{
  Ogre::MovableObject * thisclass_cpp = static_cast<Ogre::MovableObject*> (getHG3DClassPtr(*thisclass_c, "Ogre::MovableObject"));
  uint32 flags_cpp = (uint32)flags_c;
  (thisclass_cpp->removeQueryFlags(flags_cpp));
};

// Returns the query flags relevant for this object. 
extern "C" Ogre_LIB_EXPORT void ogre_mvo_getQueryFlags(struct hg3dclass_struct * thisclass_c, unsigned long * result_c)
{
  Ogre::MovableObject * thisclass_cpp = static_cast<Ogre::MovableObject*> (getHG3DClassPtr(*thisclass_c, "Ogre::MovableObject"));
  uint32 result_cpp;
  result_cpp = (thisclass_cpp->getQueryFlags());
  *result_c = (unsigned long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mvo_setVisibilityFlags(struct hg3dclass_struct * thisclass_c, unsigned long flags_c)
{
  Ogre::MovableObject * thisclass_cpp = static_cast<Ogre::MovableObject*> (getHG3DClassPtr(*thisclass_c, "Ogre::MovableObject"));
  uint32 flags_cpp = (uint32)flags_c;
  (thisclass_cpp->setVisibilityFlags(flags_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mvo_addVisibilityFlags(struct hg3dclass_struct * thisclass_c, unsigned long flags_c)
{
  Ogre::MovableObject * thisclass_cpp = static_cast<Ogre::MovableObject*> (getHG3DClassPtr(*thisclass_c, "Ogre::MovableObject"));
  uint32 flags_cpp = (uint32)flags_c;
  (thisclass_cpp->addVisibilityFlags(flags_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mvo_removeVisibilityFlags(struct hg3dclass_struct * thisclass_c, unsigned long flags_c)
{
  Ogre::MovableObject * thisclass_cpp = static_cast<Ogre::MovableObject*> (getHG3DClassPtr(*thisclass_c, "Ogre::MovableObject"));
  uint32 flags_cpp = (uint32)flags_c;
  (thisclass_cpp->removeVisibilityFlags(flags_cpp));
};

// Returns the visibility flags relevant for this object. 
extern "C" Ogre_LIB_EXPORT void ogre_mvo_getVisibilityFlags(struct hg3dclass_struct * thisclass_c, unsigned long * result_c)
{
  Ogre::MovableObject * thisclass_cpp = static_cast<Ogre::MovableObject*> (getHG3DClassPtr(*thisclass_c, "Ogre::MovableObject"));
  uint32 result_cpp;
  result_cpp = (thisclass_cpp->getVisibilityFlags());
  *result_c = (unsigned long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mvo_getLightMask(struct hg3dclass_struct * thisclass_c, unsigned long * result_c)
{
  Ogre::MovableObject * thisclass_cpp = static_cast<Ogre::MovableObject*> (getHG3DClassPtr(*thisclass_c, "Ogre::MovableObject"));
  uint32 result_cpp;
  result_cpp = (thisclass_cpp->getLightMask());
  *result_c = (unsigned long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mvo_setLightMask(struct hg3dclass_struct * thisclass_c, unsigned long lightMask_c)
{
  Ogre::MovableObject * thisclass_cpp = static_cast<Ogre::MovableObject*> (getHG3DClassPtr(*thisclass_c, "Ogre::MovableObject"));
  uint32 lightMask_cpp = (uint32)lightMask_c;
  (thisclass_cpp->setLightMask(lightMask_cpp));
};

// Define a default implementation of method from ShadowCaster which implements no shadows. 
extern "C" Ogre_LIB_EXPORT void ogre_mvo_hasEdgeList(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::MovableObject * thisclass_cpp = static_cast<Ogre::MovableObject*> (getHG3DClassPtr(*thisclass_c, "Ogre::MovableObject"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->hasEdgeList());
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mvo_setCastShadows(struct hg3dclass_struct * thisclass_c, long enabled_c)
{
  Ogre::MovableObject * thisclass_cpp = static_cast<Ogre::MovableObject*> (getHG3DClassPtr(*thisclass_c, "Ogre::MovableObject"));
  bool enabled_cpp = (bool)enabled_c;
  (thisclass_cpp->setCastShadows(enabled_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mvo_getCastShadows(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::MovableObject * thisclass_cpp = static_cast<Ogre::MovableObject*> (getHG3DClassPtr(*thisclass_c, "Ogre::MovableObject"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->getCastShadows());
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mvo_getReceivesShadows(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::MovableObject * thisclass_cpp = static_cast<Ogre::MovableObject*> (getHG3DClassPtr(*thisclass_c, "Ogre::MovableObject"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->getReceivesShadows());
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mvo_getPointExtrusionDistance(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * l_c, float * result_c)
{
  Ogre::MovableObject * thisclass_cpp = static_cast<Ogre::MovableObject*> (getHG3DClassPtr(*thisclass_c, "Ogre::MovableObject"));
  const Ogre::Light * l_cpp = static_cast<Ogre::Light*> (getHG3DClassPtr(*l_c, "Ogre::Light"));
  Real result_cpp;
  result_cpp = (thisclass_cpp->getPointExtrusionDistance(l_cpp));
  *result_c = (float)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mvo_getTypeFlags(struct hg3dclass_struct * thisclass_c, unsigned long * result_c)
{
  Ogre::MovableObject * thisclass_cpp = static_cast<Ogre::MovableObject*> (getHG3DClassPtr(*thisclass_c, "Ogre::MovableObject"));
  uint32 result_cpp;
  result_cpp = (thisclass_cpp->getTypeFlags());
  *result_c = (unsigned long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mvo_setDebugDisplayEnabled(struct hg3dclass_struct * thisclass_c, long enabled_c)
{
  Ogre::MovableObject * thisclass_cpp = static_cast<Ogre::MovableObject*> (getHG3DClassPtr(*thisclass_c, "Ogre::MovableObject"));
  bool enabled_cpp = (bool)enabled_c;
  (thisclass_cpp->setDebugDisplayEnabled(enabled_cpp));
};

// Gets whether debug display of this object is enabled. 
extern "C" Ogre_LIB_EXPORT void ogre_mvo_isDebugDisplayEnabled(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::MovableObject * thisclass_cpp = static_cast<Ogre::MovableObject*> (getHG3DClassPtr(*thisclass_c, "Ogre::MovableObject"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isDebugDisplayEnabled());
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mvo_setDefaultQueryFlags(unsigned long flags_c)
{
  uint32 flags_cpp = (uint32)flags_c;
  (Ogre::MovableObject::setDefaultQueryFlags(flags_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mvo_getDefaultQueryFlags(unsigned long * result_c)
{
  uint32 result_cpp;
  result_cpp = (Ogre::MovableObject::getDefaultQueryFlags());
  *result_c = (unsigned long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mvo_setDefaultVisibilityFlags(unsigned long flags_c)
{
  uint32 flags_cpp = (uint32)flags_c;
  (Ogre::MovableObject::setDefaultVisibilityFlags(flags_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mvo_getDefaultVisibilityFlags(unsigned long * result_c)
{
  uint32 result_cpp;
  result_cpp = (Ogre::MovableObject::getDefaultVisibilityFlags());
  *result_c = (unsigned long)result_cpp;
};

