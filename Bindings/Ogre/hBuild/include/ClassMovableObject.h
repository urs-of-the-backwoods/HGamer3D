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

// ClassMovableObject.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassMovableObject
#define _DEFINED_HG3D_ClassMovableObject

#include "ClassPtr.h"
#include "ClassNode.h"
#include "ClassSceneNode.h"
#include "ClassLight.h"


// 
void ogre_mvo_destruct(struct hg3dclass_struct * thisclass_c);

// 
void ogre_mvo_getName(struct hg3dclass_struct * thisclass_c, char * result_c);

// 
void ogre_mvo_getMovableType(struct hg3dclass_struct * thisclass_c, char * result_c);

// 
void ogre_mvo_getParentNode(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// 
void ogre_mvo_getParentSceneNode(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// Gets whether the parent node is a TagPoint (or a SceneNode
void ogre_mvo_isParentTagPoint(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_mvo_isAttached(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_mvo_detachFromParent(struct hg3dclass_struct * thisclass_c);

// 
void ogre_mvo_isInScene(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_mvo_getBoundingRadius(struct hg3dclass_struct * thisclass_c, float * result_c);

// 
void ogre_mvo_setVisible(struct hg3dclass_struct * thisclass_c, long visible_c);

// 
void ogre_mvo_getVisible(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_mvo_isVisible(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_mvo_setRenderingDistance(struct hg3dclass_struct * thisclass_c, float dist_c);

// 
void ogre_mvo_getRenderingDistance(struct hg3dclass_struct * thisclass_c, float * result_c);

// 
void ogre_mvo_setRenderingMinPixelSize(struct hg3dclass_struct * thisclass_c, float pixelSize_c);

// 
void ogre_mvo_getRenderingMinPixelSize(struct hg3dclass_struct * thisclass_c, float * result_c);

// 
void ogre_mvo_setQueryFlags(struct hg3dclass_struct * thisclass_c, unsigned long flags_c);

// 
void ogre_mvo_addQueryFlags(struct hg3dclass_struct * thisclass_c, unsigned long flags_c);

// 
void ogre_mvo_removeQueryFlags(struct hg3dclass_struct * thisclass_c, unsigned long flags_c);

// Returns the query flags relevant for this object. 
void ogre_mvo_getQueryFlags(struct hg3dclass_struct * thisclass_c, unsigned long * result_c);

// 
void ogre_mvo_setVisibilityFlags(struct hg3dclass_struct * thisclass_c, unsigned long flags_c);

// 
void ogre_mvo_addVisibilityFlags(struct hg3dclass_struct * thisclass_c, unsigned long flags_c);

// 
void ogre_mvo_removeVisibilityFlags(struct hg3dclass_struct * thisclass_c, unsigned long flags_c);

// Returns the visibility flags relevant for this object. 
void ogre_mvo_getVisibilityFlags(struct hg3dclass_struct * thisclass_c, unsigned long * result_c);

// 
void ogre_mvo_getLightMask(struct hg3dclass_struct * thisclass_c, unsigned long * result_c);

// 
void ogre_mvo_setLightMask(struct hg3dclass_struct * thisclass_c, unsigned long lightMask_c);

// Define a default implementation of method from ShadowCaster which implements no shadows. 
void ogre_mvo_hasEdgeList(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_mvo_setCastShadows(struct hg3dclass_struct * thisclass_c, long enabled_c);

// 
void ogre_mvo_getCastShadows(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_mvo_getReceivesShadows(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_mvo_getPointExtrusionDistance(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * l_c, float * result_c);

// 
void ogre_mvo_getTypeFlags(struct hg3dclass_struct * thisclass_c, unsigned long * result_c);

// 
void ogre_mvo_setDebugDisplayEnabled(struct hg3dclass_struct * thisclass_c, long enabled_c);

// Gets whether debug display of this object is enabled. 
void ogre_mvo_isDebugDisplayEnabled(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_mvo_setDefaultQueryFlags(unsigned long flags_c);

// 
void ogre_mvo_getDefaultQueryFlags(unsigned long * result_c);

// 
void ogre_mvo_setDefaultVisibilityFlags(unsigned long flags_c);

// 
void ogre_mvo_getDefaultVisibilityFlags(unsigned long * result_c);

#endif 
