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

// ClassBillboardChain.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassBillboardChain
#define _DEFINED_HG3D_ClassBillboardChain

#include "ClassPtr.h"
#include "EnumBillboardChainTexCoordDirection.h"
#include "StructVec3.h"
#include "ClassCamera.h"
#include "StructSharedPtr.h"
#include "ClassSceneManager.h"
#include "ClassRenderSystem.h"


// destructor 
void ogre_bbdc_destruct(struct hg3dclass_struct * thisclass_c);

// 
void ogre_bbdc_setMaxChainElements(struct hg3dclass_struct * thisclass_c, int maxElements_c);

// 
void ogre_bbdc_getMaxChainElements(struct hg3dclass_struct * thisclass_c, int * result_c);

// 
void ogre_bbdc_setNumberOfChains(struct hg3dclass_struct * thisclass_c, int numChains_c);

// 
void ogre_bbdc_getNumberOfChains(struct hg3dclass_struct * thisclass_c, int * result_c);

// 
void ogre_bbdc_setUseTextureCoords(struct hg3dclass_struct * thisclass_c, int use_c);

// 
void ogre_bbdc_getUseTextureCoords(struct hg3dclass_struct * thisclass_c, int * result_c);

// 
void ogre_bbdc_setTextureCoordDirection(struct hg3dclass_struct * thisclass_c, enum EnumBillboardChainTexCoordDirection dir_c);

// 
void ogre_bbdc_getTextureCoordDirection(struct hg3dclass_struct * thisclass_c, enum EnumBillboardChainTexCoordDirection * result_c);

// 
void ogre_bbdc_setOtherTextureCoordRange(struct hg3dclass_struct * thisclass_c, float start_c, float end_c);

// 
void ogre_bbdc_setUseVertexColours(struct hg3dclass_struct * thisclass_c, int use_c);

// 
void ogre_bbdc_getUseVertexColours(struct hg3dclass_struct * thisclass_c, int * result_c);

// 
void ogre_bbdc_setDynamic(struct hg3dclass_struct * thisclass_c, int dyn_c);

// 
void ogre_bbdc_getDynamic(struct hg3dclass_struct * thisclass_c, int * result_c);

// 
void ogre_bbdc_removeChainElement(struct hg3dclass_struct * thisclass_c, int chainIndex_c);

// 
void ogre_bbdc_getNumChainElements(struct hg3dclass_struct * thisclass_c, int chainIndex_c, int * result_c);

// 
void ogre_bbdc_clearChain(struct hg3dclass_struct * thisclass_c, int chainIndex_c);

// 
void ogre_bbdc_clearAllChains(struct hg3dclass_struct * thisclass_c);

// 
void ogre_bbdc_setFaceCamera(struct hg3dclass_struct * thisclass_c, int faceCamera_c, struct vector3_struct * normalVector_c);

// Get the material name in use. 
void ogre_bbdc_getMaterialName(struct hg3dclass_struct * thisclass_c, char * result_c);

// Set the material name to use for rendering. 
void ogre_bbdc_setMaterialName(struct hg3dclass_struct * thisclass_c, char * name_c, char * groupName_c);

// 
void ogre_bbdc_getSquaredViewDepth(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * cam_c, float * result_c);

// 
void ogre_bbdc_getBoundingRadius(struct hg3dclass_struct * thisclass_c, float * result_c);

// 
void ogre_bbdc_getMaterial(struct hg3dclass_struct * thisclass_c, struct sharedptr_struct * result_c);

// 
void ogre_bbdc_getMovableType(struct hg3dclass_struct * thisclass_c, char * result_c);

// 
void ogre_bbdc_preRender(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * sm_c, struct hg3dclass_struct * rsys_c, int * result_c);

#endif 
