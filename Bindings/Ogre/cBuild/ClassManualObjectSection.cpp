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

// ClassManualObjectSection.cpp

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
	#include "EnumRenderOperationOperationType.h"
#include "StructSharedPtr.h"
#include "./Ogre.h"
#include "./OgreString.h"
#include "./WindowUtilsHG3D.h"
#include "HG3DUtilities.h"

using namespace Ogre;
using Ogre::uint;



// 
extern "C" Ogre_LIB_EXPORT void ogre_mnos_construct(struct hg3dclass_struct * parent_c, char * materialName_c, enum EnumRenderOperationOperationType opType_c, char * groupName_c, struct hg3dclass_struct * result_c)
{
  Ogre::ManualObject * parent_cpp = static_cast<Ogre::ManualObject*> (getHG3DClassPtr(*parent_c, "Ogre::ManualObject"));
  Ogre::String materialName_cpp = Ogre::String((const char*) materialName_c);
  enum RenderOperation::OperationType opType_cpp = (enum RenderOperation::OperationType)opType_c;
  Ogre::String groupName_cpp = Ogre::String((const char*) groupName_c);
  Ogre::ManualObject::ManualObjectSection * result_cpp;
  result_cpp = (new Ogre::ManualObject::ManualObjectSection(parent_cpp, materialName_cpp, opType_cpp, groupName_cpp));
  *result_c = getHG3DClass_ManualObjectSection((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mnos_destruct(struct hg3dclass_struct * thisclass_c)
{
  Ogre::ManualObject::ManualObjectSection * thisclass_cpp = static_cast<Ogre::ManualObject::ManualObjectSection*> (getHG3DClassPtr(*thisclass_c, "Ogre::ManualObject::ManualObjectSection"));
  (delete thisclass_cpp);
};

// Retrieve the material name in use. 
extern "C" Ogre_LIB_EXPORT void ogre_mnos_getMaterialName(struct hg3dclass_struct * thisclass_c, char * result_c)
{
  Ogre::ManualObject::ManualObjectSection * thisclass_cpp = static_cast<Ogre::ManualObject::ManualObjectSection*> (getHG3DClassPtr(*thisclass_c, "Ogre::ManualObject::ManualObjectSection"));
  Ogre::String result_cpp;
  result_cpp = (thisclass_cpp->getMaterialName());
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

// Retrieve the material group in use. 
extern "C" Ogre_LIB_EXPORT void ogre_mnos_getMaterialGroup(struct hg3dclass_struct * thisclass_c, char * result_c)
{
  Ogre::ManualObject::ManualObjectSection * thisclass_cpp = static_cast<Ogre::ManualObject::ManualObjectSection*> (getHG3DClassPtr(*thisclass_c, "Ogre::ManualObject::ManualObjectSection"));
  Ogre::String result_cpp;
  result_cpp = (thisclass_cpp->getMaterialGroup());
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

// update the material name in use 
extern "C" Ogre_LIB_EXPORT void ogre_mnos_setMaterialName(struct hg3dclass_struct * thisclass_c, char * name_c, char * groupName_c)
{
  Ogre::ManualObject::ManualObjectSection * thisclass_cpp = static_cast<Ogre::ManualObject::ManualObjectSection*> (getHG3DClassPtr(*thisclass_c, "Ogre::ManualObject::ManualObjectSection"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  Ogre::String groupName_cpp = Ogre::String((const char*) groupName_c);
  (thisclass_cpp->setMaterialName(name_cpp, groupName_cpp));
};

// Set whether we need 32-bit indices. 
extern "C" Ogre_LIB_EXPORT void ogre_mnos_set32BitIndices(struct hg3dclass_struct * thisclass_c, long n32_c)
{
  Ogre::ManualObject::ManualObjectSection * thisclass_cpp = static_cast<Ogre::ManualObject::ManualObjectSection*> (getHG3DClassPtr(*thisclass_c, "Ogre::ManualObject::ManualObjectSection"));
  bool n32_cpp = (bool)n32_c;
  (thisclass_cpp->set32BitIndices(n32_cpp));
};

// Get whether we need 32-bit indices. 
extern "C" Ogre_LIB_EXPORT void ogre_mnos_get32BitIndices(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::ManualObject::ManualObjectSection * thisclass_cpp = static_cast<Ogre::ManualObject::ManualObjectSection*> (getHG3DClassPtr(*thisclass_c, "Ogre::ManualObject::ManualObjectSection"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->get32BitIndices());
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mnos_getMaterial(struct hg3dclass_struct * thisclass_c, struct sharedptr_struct * result_c)
{
  Ogre::ManualObject::ManualObjectSection * thisclass_cpp = static_cast<Ogre::ManualObject::ManualObjectSection*> (getHG3DClassPtr(*thisclass_c, "Ogre::ManualObject::ManualObjectSection"));
  MaterialPtr result_cpp;
  result_cpp = (thisclass_cpp->getMaterial());
  *result_c = *((struct sharedptr_struct*) &result_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_mnos_getSquaredViewDepth(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * _c, float * result_c)
{
  Ogre::ManualObject::ManualObjectSection * thisclass_cpp = static_cast<Ogre::ManualObject::ManualObjectSection*> (getHG3DClassPtr(*thisclass_c, "Ogre::ManualObject::ManualObjectSection"));
  const Ogre::Camera * _cpp = static_cast<Ogre::Camera*> (getHG3DClassPtr(*_c, "Ogre::Camera"));
  Real result_cpp;
  result_cpp = (thisclass_cpp->getSquaredViewDepth(_cpp));
  *result_c = (float)result_cpp;
};

