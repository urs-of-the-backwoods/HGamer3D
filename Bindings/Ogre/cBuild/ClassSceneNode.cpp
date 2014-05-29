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

// ClassSceneNode.cpp

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
	#include "StructVec3.h"
#include "StructQuaternion.h"
#include "./Ogre.h"
#include "./OgreString.h"
#include "./WindowUtilsHG3D.h"
#include "HG3DUtilities.h"

using namespace Ogre;
using Ogre::uint;



// 
extern "C" Ogre_LIB_EXPORT void ogre_sn_destruct(struct hg3dclass_struct * thisclass_c)
{
  Ogre::SceneNode * thisclass_cpp = static_cast<Ogre::SceneNode*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneNode"));
  (delete thisclass_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_sn_attachObject(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * obj_c)
{
  Ogre::SceneNode * thisclass_cpp = static_cast<Ogre::SceneNode*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneNode"));
  Ogre::MovableObject * obj_cpp = static_cast<Ogre::MovableObject*> (getHG3DClassPtr(*obj_c, "Ogre::MovableObject"));
  (thisclass_cpp->attachObject(obj_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_sn_numAttachedObjects(struct hg3dclass_struct * thisclass_c, unsigned short * result_c)
{
  Ogre::SceneNode * thisclass_cpp = static_cast<Ogre::SceneNode*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneNode"));
  unsigned short result_cpp;
  result_cpp = (thisclass_cpp->numAttachedObjects());
  *result_c = (unsigned short)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_sn_getAttachedObject(struct hg3dclass_struct * thisclass_c, unsigned short index_c, struct hg3dclass_struct * result_c)
{
  Ogre::SceneNode * thisclass_cpp = static_cast<Ogre::SceneNode*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneNode"));
  unsigned short index_cpp = (unsigned short)index_c;
  Ogre::MovableObject * result_cpp;
  result_cpp = (thisclass_cpp->getAttachedObject(index_cpp));
  *result_c = getHG3DClass_MovableObject((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_sn_getAttachedObject2(struct hg3dclass_struct * thisclass_c, char * name_c, struct hg3dclass_struct * result_c)
{
  Ogre::SceneNode * thisclass_cpp = static_cast<Ogre::SceneNode*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneNode"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  Ogre::MovableObject * result_cpp;
  result_cpp = (thisclass_cpp->getAttachedObject(name_cpp));
  *result_c = getHG3DClass_MovableObject((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_sn_detachObject(struct hg3dclass_struct * thisclass_c, unsigned short index_c, struct hg3dclass_struct * result_c)
{
  Ogre::SceneNode * thisclass_cpp = static_cast<Ogre::SceneNode*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneNode"));
  unsigned short index_cpp = (unsigned short)index_c;
  Ogre::MovableObject * result_cpp;
  result_cpp = (thisclass_cpp->detachObject(index_cpp));
  *result_c = getHG3DClass_MovableObject((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_sn_detachObject2(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * obj_c)
{
  Ogre::SceneNode * thisclass_cpp = static_cast<Ogre::SceneNode*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneNode"));
  Ogre::MovableObject * obj_cpp = static_cast<Ogre::MovableObject*> (getHG3DClassPtr(*obj_c, "Ogre::MovableObject"));
  (thisclass_cpp->detachObject(obj_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_sn_detachObject3(struct hg3dclass_struct * thisclass_c, char * name_c, struct hg3dclass_struct * result_c)
{
  Ogre::SceneNode * thisclass_cpp = static_cast<Ogre::SceneNode*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneNode"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  Ogre::MovableObject * result_cpp;
  result_cpp = (thisclass_cpp->detachObject(name_cpp));
  *result_c = getHG3DClass_MovableObject((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_sn_detachAllObjects(struct hg3dclass_struct * thisclass_c)
{
  Ogre::SceneNode * thisclass_cpp = static_cast<Ogre::SceneNode*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneNode"));
  (thisclass_cpp->detachAllObjects());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_sn_isInSceneGraph(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::SceneNode * thisclass_cpp = static_cast<Ogre::SceneNode*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneNode"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isInSceneGraph());
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_sn_getCreator(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  Ogre::SceneNode * thisclass_cpp = static_cast<Ogre::SceneNode*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneNode"));
  Ogre::SceneManager * result_cpp;
  result_cpp = (thisclass_cpp->getCreator());
  *result_c = getHG3DClass_SceneManager((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_sn_removeAndDestroyChild(struct hg3dclass_struct * thisclass_c, char * name_c)
{
  Ogre::SceneNode * thisclass_cpp = static_cast<Ogre::SceneNode*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneNode"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  (thisclass_cpp->removeAndDestroyChild(name_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_sn_removeAndDestroyChild2(struct hg3dclass_struct * thisclass_c, unsigned short index_c)
{
  Ogre::SceneNode * thisclass_cpp = static_cast<Ogre::SceneNode*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneNode"));
  unsigned short index_cpp = (unsigned short)index_c;
  (thisclass_cpp->removeAndDestroyChild(index_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_sn_removeAndDestroyAllChildren(struct hg3dclass_struct * thisclass_c)
{
  Ogre::SceneNode * thisclass_cpp = static_cast<Ogre::SceneNode*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneNode"));
  (thisclass_cpp->removeAndDestroyAllChildren());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_sn_showBoundingBox(struct hg3dclass_struct * thisclass_c, long bShow_c)
{
  Ogre::SceneNode * thisclass_cpp = static_cast<Ogre::SceneNode*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneNode"));
  bool bShow_cpp = (bool)bShow_c;
  (thisclass_cpp->showBoundingBox(bShow_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_sn_hideBoundingBox(struct hg3dclass_struct * thisclass_c, long bHide_c)
{
  Ogre::SceneNode * thisclass_cpp = static_cast<Ogre::SceneNode*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneNode"));
  bool bHide_cpp = (bool)bHide_c;
  (thisclass_cpp->hideBoundingBox(bHide_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_sn_getShowBoundingBox(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::SceneNode * thisclass_cpp = static_cast<Ogre::SceneNode*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneNode"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->getShowBoundingBox());
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_sn_createChildSceneNode(struct hg3dclass_struct * thisclass_c, struct vector3_struct * translate_c, struct quaternion_struct * rotate_c, struct hg3dclass_struct * result_c)
{
  Ogre::SceneNode * thisclass_cpp = static_cast<Ogre::SceneNode*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneNode"));
  Vector3 translate_cpp = *((Vector3*) translate_c);
  Quaternion rotate_cpp = *((Quaternion*) rotate_c);
  Ogre::SceneNode * result_cpp;
  result_cpp = (thisclass_cpp->createChildSceneNode(translate_cpp, rotate_cpp));
  *result_c = getHG3DClass_SceneNode((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_sn_createChildSceneNode2(struct hg3dclass_struct * thisclass_c, char * name_c, struct vector3_struct * translate_c, struct quaternion_struct * rotate_c, struct hg3dclass_struct * result_c)
{
  Ogre::SceneNode * thisclass_cpp = static_cast<Ogre::SceneNode*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneNode"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  Vector3 translate_cpp = *((Vector3*) translate_c);
  Quaternion rotate_cpp = *((Quaternion*) rotate_c);
  Ogre::SceneNode * result_cpp;
  result_cpp = (thisclass_cpp->createChildSceneNode(name_cpp, translate_cpp, rotate_cpp));
  *result_c = getHG3DClass_SceneNode((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_sn_setFixedYawAxis(struct hg3dclass_struct * thisclass_c, long useFixed_c, struct vector3_struct * fixedAxis_c)
{
  Ogre::SceneNode * thisclass_cpp = static_cast<Ogre::SceneNode*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneNode"));
  bool useFixed_cpp = (bool)useFixed_c;
  Vector3 fixedAxis_cpp = *((Vector3*) fixedAxis_c);
  (thisclass_cpp->setFixedYawAxis(useFixed_cpp, fixedAxis_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_sn_getAutoTrackTarget(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  Ogre::SceneNode * thisclass_cpp = static_cast<Ogre::SceneNode*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneNode"));
  Ogre::SceneNode * result_cpp;
  result_cpp = (thisclass_cpp->getAutoTrackTarget());
  *result_c = getHG3DClass_SceneNode((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_sn_getAutoTrackOffset(struct hg3dclass_struct * thisclass_c, struct vector3_struct * result_c)
{
  Ogre::SceneNode * thisclass_cpp = static_cast<Ogre::SceneNode*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneNode"));
  Vector3 result_cpp;
  result_cpp = (thisclass_cpp->getAutoTrackOffset());
  *result_c = *((struct vector3_struct*) &result_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_sn_getAutoTrackLocalDirection(struct hg3dclass_struct * thisclass_c, struct vector3_struct * result_c)
{
  Ogre::SceneNode * thisclass_cpp = static_cast<Ogre::SceneNode*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneNode"));
  Vector3 result_cpp;
  result_cpp = (thisclass_cpp->getAutoTrackLocalDirection());
  *result_c = *((struct vector3_struct*) &result_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_sn_getParentSceneNode(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  Ogre::SceneNode * thisclass_cpp = static_cast<Ogre::SceneNode*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneNode"));
  Ogre::SceneNode * result_cpp;
  result_cpp = (thisclass_cpp->getParentSceneNode());
  *result_c = getHG3DClass_SceneNode((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_sn_setVisible(struct hg3dclass_struct * thisclass_c, long visible_c, long cascade_c)
{
  Ogre::SceneNode * thisclass_cpp = static_cast<Ogre::SceneNode*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneNode"));
  bool visible_cpp = (bool)visible_c;
  bool cascade_cpp = (bool)cascade_c;
  (thisclass_cpp->setVisible(visible_cpp, cascade_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_sn_flipVisibility(struct hg3dclass_struct * thisclass_c, long cascade_c)
{
  Ogre::SceneNode * thisclass_cpp = static_cast<Ogre::SceneNode*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneNode"));
  bool cascade_cpp = (bool)cascade_c;
  (thisclass_cpp->flipVisibility(cascade_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_sn_setDebugDisplayEnabled(struct hg3dclass_struct * thisclass_c, long enabled_c, long cascade_c)
{
  Ogre::SceneNode * thisclass_cpp = static_cast<Ogre::SceneNode*> (getHG3DClassPtr(*thisclass_c, "Ogre::SceneNode"));
  bool enabled_cpp = (bool)enabled_c;
  bool cascade_cpp = (bool)cascade_c;
  (thisclass_cpp->setDebugDisplayEnabled(enabled_cpp, cascade_cpp));
};

