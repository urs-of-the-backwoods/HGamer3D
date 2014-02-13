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

// ClassBillboardChain.cpp

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
	#include "EnumBillboardChainTexCoordDirection.h"
#include "StructVec3.h"
#include "StructSharedPtr.h"
#include "./Ogre.h"
#include "./OgreString.h"
#include "./WindowUtilsHG3D.h"
#include "HG3DUtilities.h"

using namespace Ogre;



// destructor 
extern "C" Ogre_LIB_EXPORT void ogre_bbdc_destruct(struct hg3dclass_struct * thisclass_c)
{
  Ogre::BillboardChain * thisclass_cpp = static_cast<Ogre::BillboardChain*> (getHG3DClassPtr(*thisclass_c, "Ogre::BillboardChain"));
  (delete thisclass_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbdc_setMaxChainElements(struct hg3dclass_struct * thisclass_c, int maxElements_c)
{
  Ogre::BillboardChain * thisclass_cpp = static_cast<Ogre::BillboardChain*> (getHG3DClassPtr(*thisclass_c, "Ogre::BillboardChain"));
  size_t maxElements_cpp = (size_t)maxElements_c;
  (thisclass_cpp->setMaxChainElements(maxElements_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbdc_getMaxChainElements(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::BillboardChain * thisclass_cpp = static_cast<Ogre::BillboardChain*> (getHG3DClassPtr(*thisclass_c, "Ogre::BillboardChain"));
  size_t result_cpp;
  result_cpp = (thisclass_cpp->getMaxChainElements());
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbdc_setNumberOfChains(struct hg3dclass_struct * thisclass_c, int numChains_c)
{
  Ogre::BillboardChain * thisclass_cpp = static_cast<Ogre::BillboardChain*> (getHG3DClassPtr(*thisclass_c, "Ogre::BillboardChain"));
  size_t numChains_cpp = (size_t)numChains_c;
  (thisclass_cpp->setNumberOfChains(numChains_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbdc_getNumberOfChains(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::BillboardChain * thisclass_cpp = static_cast<Ogre::BillboardChain*> (getHG3DClassPtr(*thisclass_c, "Ogre::BillboardChain"));
  size_t result_cpp;
  result_cpp = (thisclass_cpp->getNumberOfChains());
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbdc_setUseTextureCoords(struct hg3dclass_struct * thisclass_c, int use_c)
{
  Ogre::BillboardChain * thisclass_cpp = static_cast<Ogre::BillboardChain*> (getHG3DClassPtr(*thisclass_c, "Ogre::BillboardChain"));
  bool use_cpp = (bool)use_c;
  (thisclass_cpp->setUseTextureCoords(use_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbdc_getUseTextureCoords(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::BillboardChain * thisclass_cpp = static_cast<Ogre::BillboardChain*> (getHG3DClassPtr(*thisclass_c, "Ogre::BillboardChain"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->getUseTextureCoords());
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbdc_setTextureCoordDirection(struct hg3dclass_struct * thisclass_c, enum EnumBillboardChainTexCoordDirection dir_c)
{
  Ogre::BillboardChain * thisclass_cpp = static_cast<Ogre::BillboardChain*> (getHG3DClassPtr(*thisclass_c, "Ogre::BillboardChain"));
  enum Ogre::BillboardChain::TexCoordDirection dir_cpp = (enum Ogre::BillboardChain::TexCoordDirection)dir_c;
  (thisclass_cpp->setTextureCoordDirection(dir_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbdc_getTextureCoordDirection(struct hg3dclass_struct * thisclass_c, enum EnumBillboardChainTexCoordDirection * result_c)
{
  Ogre::BillboardChain * thisclass_cpp = static_cast<Ogre::BillboardChain*> (getHG3DClassPtr(*thisclass_c, "Ogre::BillboardChain"));
  enum Ogre::BillboardChain::TexCoordDirection result_cpp;
  result_cpp = (thisclass_cpp->getTextureCoordDirection());
  *result_c = (enum EnumBillboardChainTexCoordDirection) result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbdc_setOtherTextureCoordRange(struct hg3dclass_struct * thisclass_c, float start_c, float end_c)
{
  Ogre::BillboardChain * thisclass_cpp = static_cast<Ogre::BillboardChain*> (getHG3DClassPtr(*thisclass_c, "Ogre::BillboardChain"));
  Real start_cpp = (Real)start_c;
  Real end_cpp = (Real)end_c;
  (thisclass_cpp->setOtherTextureCoordRange(start_cpp, end_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbdc_setUseVertexColours(struct hg3dclass_struct * thisclass_c, int use_c)
{
  Ogre::BillboardChain * thisclass_cpp = static_cast<Ogre::BillboardChain*> (getHG3DClassPtr(*thisclass_c, "Ogre::BillboardChain"));
  bool use_cpp = (bool)use_c;
  (thisclass_cpp->setUseVertexColours(use_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbdc_getUseVertexColours(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::BillboardChain * thisclass_cpp = static_cast<Ogre::BillboardChain*> (getHG3DClassPtr(*thisclass_c, "Ogre::BillboardChain"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->getUseVertexColours());
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbdc_setDynamic(struct hg3dclass_struct * thisclass_c, int dyn_c)
{
  Ogre::BillboardChain * thisclass_cpp = static_cast<Ogre::BillboardChain*> (getHG3DClassPtr(*thisclass_c, "Ogre::BillboardChain"));
  bool dyn_cpp = (bool)dyn_c;
  (thisclass_cpp->setDynamic(dyn_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbdc_getDynamic(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::BillboardChain * thisclass_cpp = static_cast<Ogre::BillboardChain*> (getHG3DClassPtr(*thisclass_c, "Ogre::BillboardChain"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->getDynamic());
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbdc_removeChainElement(struct hg3dclass_struct * thisclass_c, int chainIndex_c)
{
  Ogre::BillboardChain * thisclass_cpp = static_cast<Ogre::BillboardChain*> (getHG3DClassPtr(*thisclass_c, "Ogre::BillboardChain"));
  size_t chainIndex_cpp = (size_t)chainIndex_c;
  (thisclass_cpp->removeChainElement(chainIndex_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbdc_getNumChainElements(struct hg3dclass_struct * thisclass_c, int chainIndex_c, int * result_c)
{
  Ogre::BillboardChain * thisclass_cpp = static_cast<Ogre::BillboardChain*> (getHG3DClassPtr(*thisclass_c, "Ogre::BillboardChain"));
  size_t chainIndex_cpp = (size_t)chainIndex_c;
  size_t result_cpp;
  result_cpp = (thisclass_cpp->getNumChainElements(chainIndex_cpp));
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbdc_clearChain(struct hg3dclass_struct * thisclass_c, int chainIndex_c)
{
  Ogre::BillboardChain * thisclass_cpp = static_cast<Ogre::BillboardChain*> (getHG3DClassPtr(*thisclass_c, "Ogre::BillboardChain"));
  size_t chainIndex_cpp = (size_t)chainIndex_c;
  (thisclass_cpp->clearChain(chainIndex_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbdc_clearAllChains(struct hg3dclass_struct * thisclass_c)
{
  Ogre::BillboardChain * thisclass_cpp = static_cast<Ogre::BillboardChain*> (getHG3DClassPtr(*thisclass_c, "Ogre::BillboardChain"));
  (thisclass_cpp->clearAllChains());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbdc_setFaceCamera(struct hg3dclass_struct * thisclass_c, int faceCamera_c, struct vector3_struct * normalVector_c)
{
  Ogre::BillboardChain * thisclass_cpp = static_cast<Ogre::BillboardChain*> (getHG3DClassPtr(*thisclass_c, "Ogre::BillboardChain"));
  bool faceCamera_cpp = (bool)faceCamera_c;
  Vector3 normalVector_cpp = *((Vector3*) normalVector_c);
  (thisclass_cpp->setFaceCamera(faceCamera_cpp, normalVector_cpp));
};

// Get the material name in use. 
extern "C" Ogre_LIB_EXPORT void ogre_bbdc_getMaterialName(struct hg3dclass_struct * thisclass_c, char * result_c)
{
  Ogre::BillboardChain * thisclass_cpp = static_cast<Ogre::BillboardChain*> (getHG3DClassPtr(*thisclass_c, "Ogre::BillboardChain"));
  Ogre::String result_cpp;
  result_cpp = (thisclass_cpp->getMaterialName());
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

// Set the material name to use for rendering. 
extern "C" Ogre_LIB_EXPORT void ogre_bbdc_setMaterialName(struct hg3dclass_struct * thisclass_c, char * name_c, char * groupName_c)
{
  Ogre::BillboardChain * thisclass_cpp = static_cast<Ogre::BillboardChain*> (getHG3DClassPtr(*thisclass_c, "Ogre::BillboardChain"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  Ogre::String groupName_cpp = Ogre::String((const char*) groupName_c);
  (thisclass_cpp->setMaterialName(name_cpp, groupName_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbdc_getSquaredViewDepth(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * cam_c, float * result_c)
{
  Ogre::BillboardChain * thisclass_cpp = static_cast<Ogre::BillboardChain*> (getHG3DClassPtr(*thisclass_c, "Ogre::BillboardChain"));
  const Ogre::Camera * cam_cpp = static_cast<Ogre::Camera*> (getHG3DClassPtr(*cam_c, "Ogre::Camera"));
  Real result_cpp;
  result_cpp = (thisclass_cpp->getSquaredViewDepth(cam_cpp));
  *result_c = (float)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbdc_getBoundingRadius(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  Ogre::BillboardChain * thisclass_cpp = static_cast<Ogre::BillboardChain*> (getHG3DClassPtr(*thisclass_c, "Ogre::BillboardChain"));
  Real result_cpp;
  result_cpp = (thisclass_cpp->getBoundingRadius());
  *result_c = (float)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbdc_getMaterial(struct hg3dclass_struct * thisclass_c, struct sharedptr_struct * result_c)
{
  Ogre::BillboardChain * thisclass_cpp = static_cast<Ogre::BillboardChain*> (getHG3DClassPtr(*thisclass_c, "Ogre::BillboardChain"));
  MaterialPtr result_cpp;
  result_cpp = (thisclass_cpp->getMaterial());
  *result_c = *((struct sharedptr_struct*) &result_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbdc_getMovableType(struct hg3dclass_struct * thisclass_c, char * result_c)
{
  Ogre::BillboardChain * thisclass_cpp = static_cast<Ogre::BillboardChain*> (getHG3DClassPtr(*thisclass_c, "Ogre::BillboardChain"));
  Ogre::String result_cpp;
  result_cpp = (thisclass_cpp->getMovableType());
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_bbdc_preRender(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * sm_c, struct hg3dclass_struct * rsys_c, int * result_c)
{
  Ogre::BillboardChain * thisclass_cpp = static_cast<Ogre::BillboardChain*> (getHG3DClassPtr(*thisclass_c, "Ogre::BillboardChain"));
  Ogre::SceneManager * sm_cpp = static_cast<Ogre::SceneManager*> (getHG3DClassPtr(*sm_c, "Ogre::SceneManager"));
  Ogre::RenderSystem * rsys_cpp = static_cast<Ogre::RenderSystem*> (getHG3DClassPtr(*rsys_c, "Ogre::RenderSystem"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->preRender(sm_cpp, rsys_cpp));
  *result_c = (int)result_cpp;
};

