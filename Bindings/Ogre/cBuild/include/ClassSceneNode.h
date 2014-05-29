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

// ClassSceneNode.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassSceneNode
#define _DEFINED_HG3D_ClassSceneNode

#include "ClassPtr.h"
#include "ClassMovableObject.h"
#include "ClassSceneManager.h"
#include "StructVec3.h"
#include "StructQuaternion.h"


// 
void ogre_sn_destruct(struct hg3dclass_struct * thisclass_c);

// 
void ogre_sn_attachObject(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * obj_c);

// 
void ogre_sn_numAttachedObjects(struct hg3dclass_struct * thisclass_c, unsigned short * result_c);

// 
void ogre_sn_getAttachedObject(struct hg3dclass_struct * thisclass_c, unsigned short index_c, struct hg3dclass_struct * result_c);

// 
void ogre_sn_getAttachedObject2(struct hg3dclass_struct * thisclass_c, char * name_c, struct hg3dclass_struct * result_c);

// 
void ogre_sn_detachObject(struct hg3dclass_struct * thisclass_c, unsigned short index_c, struct hg3dclass_struct * result_c);

// 
void ogre_sn_detachObject2(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * obj_c);

// 
void ogre_sn_detachObject3(struct hg3dclass_struct * thisclass_c, char * name_c, struct hg3dclass_struct * result_c);

// 
void ogre_sn_detachAllObjects(struct hg3dclass_struct * thisclass_c);

// 
void ogre_sn_isInSceneGraph(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_sn_getCreator(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// 
void ogre_sn_removeAndDestroyChild(struct hg3dclass_struct * thisclass_c, char * name_c);

// 
void ogre_sn_removeAndDestroyChild2(struct hg3dclass_struct * thisclass_c, unsigned short index_c);

// 
void ogre_sn_removeAndDestroyAllChildren(struct hg3dclass_struct * thisclass_c);

// 
void ogre_sn_showBoundingBox(struct hg3dclass_struct * thisclass_c, long bShow_c);

// 
void ogre_sn_hideBoundingBox(struct hg3dclass_struct * thisclass_c, long bHide_c);

// 
void ogre_sn_getShowBoundingBox(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_sn_createChildSceneNode(struct hg3dclass_struct * thisclass_c, struct vector3_struct * translate_c, struct quaternion_struct * rotate_c, struct hg3dclass_struct * result_c);

// 
void ogre_sn_createChildSceneNode2(struct hg3dclass_struct * thisclass_c, char * name_c, struct vector3_struct * translate_c, struct quaternion_struct * rotate_c, struct hg3dclass_struct * result_c);

// 
void ogre_sn_setFixedYawAxis(struct hg3dclass_struct * thisclass_c, long useFixed_c, struct vector3_struct * fixedAxis_c);

// 
void ogre_sn_getAutoTrackTarget(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// 
void ogre_sn_getAutoTrackOffset(struct hg3dclass_struct * thisclass_c, struct vector3_struct * result_c);

// 
void ogre_sn_getAutoTrackLocalDirection(struct hg3dclass_struct * thisclass_c, struct vector3_struct * result_c);

// 
void ogre_sn_getParentSceneNode(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c);

// 
void ogre_sn_setVisible(struct hg3dclass_struct * thisclass_c, long visible_c, long cascade_c);

// 
void ogre_sn_flipVisibility(struct hg3dclass_struct * thisclass_c, long cascade_c);

// 
void ogre_sn_setDebugDisplayEnabled(struct hg3dclass_struct * thisclass_c, long enabled_c, long cascade_c);

#endif 
