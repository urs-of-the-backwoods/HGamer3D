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

// ClassMaterial.cpp

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
#include "StructColour.h"
#include "./Ogre.h"
#include "./OgreString.h"
#include "./WindowUtilsHG3D.h"
#include "HG3DUtilities.h"

using namespace Ogre;
using Ogre::uint;



// 
extern "C" Ogre_LIB_EXPORT void ogre_mtrl_destruct(struct hg3dclass_struct * thisclass_c)
{
  Ogre::Material * thisclass_cpp = static_cast<Ogre::Material*> (getHG3DClassPtr(*thisclass_c, "Ogre::Material"));
  (delete thisclass_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mtrl_isTransparent(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::Material * thisclass_cpp = static_cast<Ogre::Material*> (getHG3DClassPtr(*thisclass_c, "Ogre::Material"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isTransparent());
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mtrl_setReceiveShadows(struct hg3dclass_struct * thisclass_c, long enabled_c)
{
  Ogre::Material * thisclass_cpp = static_cast<Ogre::Material*> (getHG3DClassPtr(*thisclass_c, "Ogre::Material"));
  bool enabled_cpp = (bool)enabled_c;
  (thisclass_cpp->setReceiveShadows(enabled_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mtrl_getReceiveShadows(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::Material * thisclass_cpp = static_cast<Ogre::Material*> (getHG3DClassPtr(*thisclass_c, "Ogre::Material"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->getReceiveShadows());
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mtrl_setTransparencyCastsShadows(struct hg3dclass_struct * thisclass_c, long enabled_c)
{
  Ogre::Material * thisclass_cpp = static_cast<Ogre::Material*> (getHG3DClassPtr(*thisclass_c, "Ogre::Material"));
  bool enabled_cpp = (bool)enabled_c;
  (thisclass_cpp->setTransparencyCastsShadows(enabled_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mtrl_getTransparencyCastsShadows(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::Material * thisclass_cpp = static_cast<Ogre::Material*> (getHG3DClassPtr(*thisclass_c, "Ogre::Material"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->getTransparencyCastsShadows());
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mtrl_getNumTechniques(struct hg3dclass_struct * thisclass_c, unsigned short * result_c)
{
  Ogre::Material * thisclass_cpp = static_cast<Ogre::Material*> (getHG3DClassPtr(*thisclass_c, "Ogre::Material"));
  unsigned short result_cpp;
  result_cpp = (thisclass_cpp->getNumTechniques());
  *result_c = (unsigned short)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mtrl_removeTechnique(struct hg3dclass_struct * thisclass_c, unsigned short index_c)
{
  Ogre::Material * thisclass_cpp = static_cast<Ogre::Material*> (getHG3DClassPtr(*thisclass_c, "Ogre::Material"));
  unsigned short index_cpp = (unsigned short)index_c;
  (thisclass_cpp->removeTechnique(index_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mtrl_removeAllTechniques(struct hg3dclass_struct * thisclass_c)
{
  Ogre::Material * thisclass_cpp = static_cast<Ogre::Material*> (getHG3DClassPtr(*thisclass_c, "Ogre::Material"));
  (thisclass_cpp->removeAllTechniques());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mtrl_getNumSupportedTechniques(struct hg3dclass_struct * thisclass_c, unsigned short * result_c)
{
  Ogre::Material * thisclass_cpp = static_cast<Ogre::Material*> (getHG3DClassPtr(*thisclass_c, "Ogre::Material"));
  unsigned short result_cpp;
  result_cpp = (thisclass_cpp->getNumSupportedTechniques());
  *result_c = (unsigned short)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mtrl_getUnsupportedTechniquesExplanation(struct hg3dclass_struct * thisclass_c, char * result_c)
{
  Ogre::Material * thisclass_cpp = static_cast<Ogre::Material*> (getHG3DClassPtr(*thisclass_c, "Ogre::Material"));
  Ogre::String result_cpp;
  result_cpp = (thisclass_cpp->getUnsupportedTechniquesExplanation());
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mtrl_getNumLodLevels(struct hg3dclass_struct * thisclass_c, unsigned short schemeIndex_c, unsigned short * result_c)
{
  Ogre::Material * thisclass_cpp = static_cast<Ogre::Material*> (getHG3DClassPtr(*thisclass_c, "Ogre::Material"));
  unsigned short schemeIndex_cpp = (unsigned short)schemeIndex_c;
  unsigned short result_cpp;
  result_cpp = (thisclass_cpp->getNumLodLevels(schemeIndex_cpp));
  *result_c = (unsigned short)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mtrl_getNumLodLevels2(struct hg3dclass_struct * thisclass_c, char * schemeName_c, unsigned short * result_c)
{
  Ogre::Material * thisclass_cpp = static_cast<Ogre::Material*> (getHG3DClassPtr(*thisclass_c, "Ogre::Material"));
  Ogre::String schemeName_cpp = Ogre::String((const char*) schemeName_c);
  unsigned short result_cpp;
  result_cpp = (thisclass_cpp->getNumLodLevels(schemeName_cpp));
  *result_c = (unsigned short)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mtrl_clone(struct hg3dclass_struct * thisclass_c, char * newName_c, long changeGroup_c, char * newGroup_c, struct sharedptr_struct * result_c)
{
  Ogre::Material * thisclass_cpp = static_cast<Ogre::Material*> (getHG3DClassPtr(*thisclass_c, "Ogre::Material"));
  Ogre::String newName_cpp = Ogre::String((const char*) newName_c);
  bool changeGroup_cpp = (bool)changeGroup_c;
  Ogre::String newGroup_cpp = Ogre::String((const char*) newGroup_c);
  MaterialPtr result_cpp;
  result_cpp = (thisclass_cpp->clone(newName_cpp, changeGroup_cpp, newGroup_cpp));
  *result_c = *((struct sharedptr_struct*) &result_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mtrl_copyDetailsTo(struct hg3dclass_struct * thisclass_c, struct sharedptr_struct * mat_c)
{
  Ogre::Material * thisclass_cpp = static_cast<Ogre::Material*> (getHG3DClassPtr(*thisclass_c, "Ogre::Material"));
  MaterialPtr mat_cpp;
  (thisclass_cpp->copyDetailsTo(mat_cpp));
  *mat_c = *((struct sharedptr_struct*) &mat_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mtrl_compile(struct hg3dclass_struct * thisclass_c, long autoManageTextureUnits_c)
{
  Ogre::Material * thisclass_cpp = static_cast<Ogre::Material*> (getHG3DClassPtr(*thisclass_c, "Ogre::Material"));
  bool autoManageTextureUnits_cpp = (bool)autoManageTextureUnits_c;
  (thisclass_cpp->compile(autoManageTextureUnits_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mtrl_setPointSize(struct hg3dclass_struct * thisclass_c, float ps_c)
{
  Ogre::Material * thisclass_cpp = static_cast<Ogre::Material*> (getHG3DClassPtr(*thisclass_c, "Ogre::Material"));
  Real ps_cpp = (Real)ps_c;
  (thisclass_cpp->setPointSize(ps_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mtrl_setAmbient(struct hg3dclass_struct * thisclass_c, float red_c, float green_c, float blue_c)
{
  Ogre::Material * thisclass_cpp = static_cast<Ogre::Material*> (getHG3DClassPtr(*thisclass_c, "Ogre::Material"));
  Real red_cpp = (Real)red_c;
  Real green_cpp = (Real)green_c;
  Real blue_cpp = (Real)blue_c;
  (thisclass_cpp->setAmbient(red_cpp, green_cpp, blue_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mtrl_setAmbient2(struct hg3dclass_struct * thisclass_c, struct colourvalue_struct * ambient_c)
{
  Ogre::Material * thisclass_cpp = static_cast<Ogre::Material*> (getHG3DClassPtr(*thisclass_c, "Ogre::Material"));
  ColourValue ambient_cpp = *((ColourValue*) ambient_c);
  (thisclass_cpp->setAmbient(ambient_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mtrl_setDiffuse(struct hg3dclass_struct * thisclass_c, float red_c, float green_c, float blue_c, float alpha_c)
{
  Ogre::Material * thisclass_cpp = static_cast<Ogre::Material*> (getHG3DClassPtr(*thisclass_c, "Ogre::Material"));
  Real red_cpp = (Real)red_c;
  Real green_cpp = (Real)green_c;
  Real blue_cpp = (Real)blue_c;
  Real alpha_cpp = (Real)alpha_c;
  (thisclass_cpp->setDiffuse(red_cpp, green_cpp, blue_cpp, alpha_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mtrl_setDiffuse2(struct hg3dclass_struct * thisclass_c, struct colourvalue_struct * diffuse_c)
{
  Ogre::Material * thisclass_cpp = static_cast<Ogre::Material*> (getHG3DClassPtr(*thisclass_c, "Ogre::Material"));
  ColourValue diffuse_cpp = *((ColourValue*) diffuse_c);
  (thisclass_cpp->setDiffuse(diffuse_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mtrl_setSpecular(struct hg3dclass_struct * thisclass_c, float red_c, float green_c, float blue_c, float alpha_c)
{
  Ogre::Material * thisclass_cpp = static_cast<Ogre::Material*> (getHG3DClassPtr(*thisclass_c, "Ogre::Material"));
  Real red_cpp = (Real)red_c;
  Real green_cpp = (Real)green_c;
  Real blue_cpp = (Real)blue_c;
  Real alpha_cpp = (Real)alpha_c;
  (thisclass_cpp->setSpecular(red_cpp, green_cpp, blue_cpp, alpha_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mtrl_setSpecular2(struct hg3dclass_struct * thisclass_c, struct colourvalue_struct * specular_c)
{
  Ogre::Material * thisclass_cpp = static_cast<Ogre::Material*> (getHG3DClassPtr(*thisclass_c, "Ogre::Material"));
  ColourValue specular_cpp = *((ColourValue*) specular_c);
  (thisclass_cpp->setSpecular(specular_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mtrl_setShininess(struct hg3dclass_struct * thisclass_c, float val_c)
{
  Ogre::Material * thisclass_cpp = static_cast<Ogre::Material*> (getHG3DClassPtr(*thisclass_c, "Ogre::Material"));
  Real val_cpp = (Real)val_c;
  (thisclass_cpp->setShininess(val_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mtrl_setSelfIllumination(struct hg3dclass_struct * thisclass_c, float red_c, float green_c, float blue_c)
{
  Ogre::Material * thisclass_cpp = static_cast<Ogre::Material*> (getHG3DClassPtr(*thisclass_c, "Ogre::Material"));
  Real red_cpp = (Real)red_c;
  Real green_cpp = (Real)green_c;
  Real blue_cpp = (Real)blue_c;
  (thisclass_cpp->setSelfIllumination(red_cpp, green_cpp, blue_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mtrl_setSelfIllumination2(struct hg3dclass_struct * thisclass_c, struct colourvalue_struct * selfIllum_c)
{
  Ogre::Material * thisclass_cpp = static_cast<Ogre::Material*> (getHG3DClassPtr(*thisclass_c, "Ogre::Material"));
  ColourValue selfIllum_cpp = *((ColourValue*) selfIllum_c);
  (thisclass_cpp->setSelfIllumination(selfIllum_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mtrl_setDepthCheckEnabled(struct hg3dclass_struct * thisclass_c, long enabled_c)
{
  Ogre::Material * thisclass_cpp = static_cast<Ogre::Material*> (getHG3DClassPtr(*thisclass_c, "Ogre::Material"));
  bool enabled_cpp = (bool)enabled_c;
  (thisclass_cpp->setDepthCheckEnabled(enabled_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mtrl_setDepthWriteEnabled(struct hg3dclass_struct * thisclass_c, long enabled_c)
{
  Ogre::Material * thisclass_cpp = static_cast<Ogre::Material*> (getHG3DClassPtr(*thisclass_c, "Ogre::Material"));
  bool enabled_cpp = (bool)enabled_c;
  (thisclass_cpp->setDepthWriteEnabled(enabled_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mtrl_setColourWriteEnabled(struct hg3dclass_struct * thisclass_c, long enabled_c)
{
  Ogre::Material * thisclass_cpp = static_cast<Ogre::Material*> (getHG3DClassPtr(*thisclass_c, "Ogre::Material"));
  bool enabled_cpp = (bool)enabled_c;
  (thisclass_cpp->setColourWriteEnabled(enabled_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mtrl_setLightingEnabled(struct hg3dclass_struct * thisclass_c, long enabled_c)
{
  Ogre::Material * thisclass_cpp = static_cast<Ogre::Material*> (getHG3DClassPtr(*thisclass_c, "Ogre::Material"));
  bool enabled_cpp = (bool)enabled_c;
  (thisclass_cpp->setLightingEnabled(enabled_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mtrl_setDepthBias(struct hg3dclass_struct * thisclass_c, float constantBias_c, float slopeScaleBias_c)
{
  Ogre::Material * thisclass_cpp = static_cast<Ogre::Material*> (getHG3DClassPtr(*thisclass_c, "Ogre::Material"));
  float constantBias_cpp = (float)constantBias_c;
  float slopeScaleBias_cpp = (float)slopeScaleBias_c;
  (thisclass_cpp->setDepthBias(constantBias_cpp, slopeScaleBias_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mtrl_setTextureAnisotropy(struct hg3dclass_struct * thisclass_c, long maxAniso_c)
{
  Ogre::Material * thisclass_cpp = static_cast<Ogre::Material*> (getHG3DClassPtr(*thisclass_c, "Ogre::Material"));
  int maxAniso_cpp = (int)maxAniso_c;
  (thisclass_cpp->setTextureAnisotropy(maxAniso_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mtrl_touch(struct hg3dclass_struct * thisclass_c)
{
  Ogre::Material * thisclass_cpp = static_cast<Ogre::Material*> (getHG3DClassPtr(*thisclass_c, "Ogre::Material"));
  (thisclass_cpp->touch());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mtrl_getCompilationRequired(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::Material * thisclass_cpp = static_cast<Ogre::Material*> (getHG3DClassPtr(*thisclass_c, "Ogre::Material"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->getCompilationRequired());
  *result_c = (long)result_cpp;
};

