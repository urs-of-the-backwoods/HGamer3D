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

// ClassMaterial.h

// 

#include "wchar.h"
#ifndef _DEFINED_HG3D_ClassMaterial
#define _DEFINED_HG3D_ClassMaterial

#include "ClassPtr.h"
#include "StructSharedPtr.h"
#include "StructColour.h"


// 
void ogre_mtrl_destruct(struct hg3dclass_struct * thisclass_c);

// 
void ogre_mtrl_isTransparent(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_mtrl_setReceiveShadows(struct hg3dclass_struct * thisclass_c, long enabled_c);

// 
void ogre_mtrl_getReceiveShadows(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_mtrl_setTransparencyCastsShadows(struct hg3dclass_struct * thisclass_c, long enabled_c);

// 
void ogre_mtrl_getTransparencyCastsShadows(struct hg3dclass_struct * thisclass_c, long * result_c);

// 
void ogre_mtrl_getNumTechniques(struct hg3dclass_struct * thisclass_c, unsigned short * result_c);

// 
void ogre_mtrl_removeTechnique(struct hg3dclass_struct * thisclass_c, unsigned short index_c);

// 
void ogre_mtrl_removeAllTechniques(struct hg3dclass_struct * thisclass_c);

// 
void ogre_mtrl_getNumSupportedTechniques(struct hg3dclass_struct * thisclass_c, unsigned short * result_c);

// 
void ogre_mtrl_getUnsupportedTechniquesExplanation(struct hg3dclass_struct * thisclass_c, char * result_c);

// 
void ogre_mtrl_getNumLodLevels(struct hg3dclass_struct * thisclass_c, unsigned short schemeIndex_c, unsigned short * result_c);

// 
void ogre_mtrl_getNumLodLevels2(struct hg3dclass_struct * thisclass_c, char * schemeName_c, unsigned short * result_c);

// 
void ogre_mtrl_clone(struct hg3dclass_struct * thisclass_c, char * newName_c, long changeGroup_c, char * newGroup_c, struct sharedptr_struct * result_c);

// 
void ogre_mtrl_copyDetailsTo(struct hg3dclass_struct * thisclass_c, struct sharedptr_struct * mat_c);

// 
void ogre_mtrl_compile(struct hg3dclass_struct * thisclass_c, long autoManageTextureUnits_c);

// 
void ogre_mtrl_setPointSize(struct hg3dclass_struct * thisclass_c, float ps_c);

// 
void ogre_mtrl_setAmbient(struct hg3dclass_struct * thisclass_c, float red_c, float green_c, float blue_c);

// 
void ogre_mtrl_setAmbient2(struct hg3dclass_struct * thisclass_c, struct colourvalue_struct * ambient_c);

// 
void ogre_mtrl_setDiffuse(struct hg3dclass_struct * thisclass_c, float red_c, float green_c, float blue_c, float alpha_c);

// 
void ogre_mtrl_setDiffuse2(struct hg3dclass_struct * thisclass_c, struct colourvalue_struct * diffuse_c);

// 
void ogre_mtrl_setSpecular(struct hg3dclass_struct * thisclass_c, float red_c, float green_c, float blue_c, float alpha_c);

// 
void ogre_mtrl_setSpecular2(struct hg3dclass_struct * thisclass_c, struct colourvalue_struct * specular_c);

// 
void ogre_mtrl_setShininess(struct hg3dclass_struct * thisclass_c, float val_c);

// 
void ogre_mtrl_setSelfIllumination(struct hg3dclass_struct * thisclass_c, float red_c, float green_c, float blue_c);

// 
void ogre_mtrl_setSelfIllumination2(struct hg3dclass_struct * thisclass_c, struct colourvalue_struct * selfIllum_c);

// 
void ogre_mtrl_setDepthCheckEnabled(struct hg3dclass_struct * thisclass_c, long enabled_c);

// 
void ogre_mtrl_setDepthWriteEnabled(struct hg3dclass_struct * thisclass_c, long enabled_c);

// 
void ogre_mtrl_setColourWriteEnabled(struct hg3dclass_struct * thisclass_c, long enabled_c);

// 
void ogre_mtrl_setLightingEnabled(struct hg3dclass_struct * thisclass_c, long enabled_c);

// 
void ogre_mtrl_setDepthBias(struct hg3dclass_struct * thisclass_c, float constantBias_c, float slopeScaleBias_c);

// 
void ogre_mtrl_setTextureAnisotropy(struct hg3dclass_struct * thisclass_c, long maxAniso_c);

// 
void ogre_mtrl_touch(struct hg3dclass_struct * thisclass_c);

// 
void ogre_mtrl_getCompilationRequired(struct hg3dclass_struct * thisclass_c, long * result_c);

#endif 
