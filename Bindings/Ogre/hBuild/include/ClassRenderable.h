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

// ClassRenderable.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassRenderable
#define _DEFINED_HG3D_ClassRenderable

#include "ClassPtr.h"
#include "StructSharedPtr.h"
#include "ClassSceneManager.h"
#include "ClassRenderSystem.h"
#include "ClassCamera.h"


// 
void ogre_rndl_destruct(struct hg3dclass_struct * thisclass_c);

// 
void ogre_rndl_getMaterial(struct hg3dclass_struct * thisclass_c, struct sharedptr_struct * result_c);

// 
void ogre_rndl_preRender(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * sm_c, struct hg3dclass_struct * rsys_c, long * result_c);

// 
void ogre_rndl_postRender(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * sm_c, struct hg3dclass_struct * rsys_c);

// 
void ogre_rndl_getNumWorldTransforms(struct hg3dclass_struct * thisclass_c, unsigned short * result_c);

// 
void ogre_rndl_setUseIdentityProjection(struct hg3dclass_struct * thisclass_c, long useIdentityProjection_c);

// 
void ogre_rndl_getUseIdentityProjection(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_rndl_setUseIdentityView(struct hg3dclass_struct * thisclass_c, long useIdentityView_c);

// 
void ogre_rndl_getUseIdentityView(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_rndl_getSquaredViewDepth(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * cam_c, float * result_c);

// 
void ogre_rndl_getCastsShadows(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_rndl_removeCustomParameter(struct hg3dclass_struct * thisclass_c, long index_c);

// 
void ogre_rndl_hasCustomParameter(struct hg3dclass_struct * thisclass_c, long index_c, long * result_c);

// 
void ogre_rndl_setPolygonModeOverrideable(struct hg3dclass_struct * thisclass_c, long override_c);

// 
void ogre_rndl_getPolygonModeOverrideable(struct hg3dclass_struct * thisclass_c, long * result_c);

#endif 
