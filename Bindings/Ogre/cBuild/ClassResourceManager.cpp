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

// ClassResourceManager.cpp

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
extern "C" Ogre_LIB_EXPORT void ogre_rsrcmgr_destruct(struct hg3dclass_struct * thisclass_c)
{
  Ogre::ResourceManager * thisclass_cpp = static_cast<Ogre::ResourceManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::ResourceManager"));
  (delete thisclass_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rsrcmgr_setMemoryBudget(struct hg3dclass_struct * thisclass_c, long bytes_c)
{
  Ogre::ResourceManager * thisclass_cpp = static_cast<Ogre::ResourceManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::ResourceManager"));
  size_t bytes_cpp = (size_t)bytes_c;
  (thisclass_cpp->setMemoryBudget(bytes_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rsrcmgr_getMemoryBudget(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::ResourceManager * thisclass_cpp = static_cast<Ogre::ResourceManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::ResourceManager"));
  size_t result_cpp;
  result_cpp = (thisclass_cpp->getMemoryBudget());
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rsrcmgr_getMemoryUsage(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::ResourceManager * thisclass_cpp = static_cast<Ogre::ResourceManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::ResourceManager"));
  size_t result_cpp;
  result_cpp = (thisclass_cpp->getMemoryUsage());
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rsrcmgr_unload(struct hg3dclass_struct * thisclass_c, char * name_c)
{
  Ogre::ResourceManager * thisclass_cpp = static_cast<Ogre::ResourceManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::ResourceManager"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  (thisclass_cpp->unload(name_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rsrcmgr_unloadAll(struct hg3dclass_struct * thisclass_c, long reloadableOnly_c)
{
  Ogre::ResourceManager * thisclass_cpp = static_cast<Ogre::ResourceManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::ResourceManager"));
  bool reloadableOnly_cpp = (bool)reloadableOnly_c;
  (thisclass_cpp->unloadAll(reloadableOnly_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rsrcmgr_reloadAll(struct hg3dclass_struct * thisclass_c, long reloadableOnly_c)
{
  Ogre::ResourceManager * thisclass_cpp = static_cast<Ogre::ResourceManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::ResourceManager"));
  bool reloadableOnly_cpp = (bool)reloadableOnly_c;
  (thisclass_cpp->reloadAll(reloadableOnly_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rsrcmgr_unloadUnreferencedResources(struct hg3dclass_struct * thisclass_c, long reloadableOnly_c)
{
  Ogre::ResourceManager * thisclass_cpp = static_cast<Ogre::ResourceManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::ResourceManager"));
  bool reloadableOnly_cpp = (bool)reloadableOnly_c;
  (thisclass_cpp->unloadUnreferencedResources(reloadableOnly_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rsrcmgr_reloadUnreferencedResources(struct hg3dclass_struct * thisclass_c, long reloadableOnly_c)
{
  Ogre::ResourceManager * thisclass_cpp = static_cast<Ogre::ResourceManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::ResourceManager"));
  bool reloadableOnly_cpp = (bool)reloadableOnly_c;
  (thisclass_cpp->reloadUnreferencedResources(reloadableOnly_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rsrcmgr_remove2(struct hg3dclass_struct * thisclass_c, char * name_c)
{
  Ogre::ResourceManager * thisclass_cpp = static_cast<Ogre::ResourceManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::ResourceManager"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  (thisclass_cpp->remove(name_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rsrcmgr_removeAll(struct hg3dclass_struct * thisclass_c)
{
  Ogre::ResourceManager * thisclass_cpp = static_cast<Ogre::ResourceManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::ResourceManager"));
  (thisclass_cpp->removeAll());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rsrcmgr_removeUnreferencedResources(struct hg3dclass_struct * thisclass_c, long reloadableOnly_c)
{
  Ogre::ResourceManager * thisclass_cpp = static_cast<Ogre::ResourceManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::ResourceManager"));
  bool reloadableOnly_cpp = (bool)reloadableOnly_c;
  (thisclass_cpp->removeUnreferencedResources(reloadableOnly_cpp));
};

// Returns whether the named resource exists in this manager. 
extern "C" Ogre_LIB_EXPORT void ogre_rsrcmgr_resourceExists(struct hg3dclass_struct * thisclass_c, char * name_c, long * result_c)
{
  Ogre::ResourceManager * thisclass_cpp = static_cast<Ogre::ResourceManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::ResourceManager"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  bool result_cpp;
  result_cpp = (thisclass_cpp->resourceExists(name_cpp));
  *result_c = (long)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rsrcmgr_getLoadingOrder(struct hg3dclass_struct * thisclass_c, float * result_c)
{
  Ogre::ResourceManager * thisclass_cpp = static_cast<Ogre::ResourceManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::ResourceManager"));
  Real result_cpp;
  result_cpp = (thisclass_cpp->getLoadingOrder());
  *result_c = (float)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rsrcmgr_getResourceType(struct hg3dclass_struct * thisclass_c, char * result_c)
{
  Ogre::ResourceManager * thisclass_cpp = static_cast<Ogre::ResourceManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::ResourceManager"));
  Ogre::String result_cpp;
  result_cpp = (thisclass_cpp->getResourceType());
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rsrcmgr_setVerbose(struct hg3dclass_struct * thisclass_c, long v_c)
{
  Ogre::ResourceManager * thisclass_cpp = static_cast<Ogre::ResourceManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::ResourceManager"));
  bool v_cpp = (bool)v_c;
  (thisclass_cpp->setVerbose(v_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rsrcmgr_getVerbose(struct hg3dclass_struct * thisclass_c, long * result_c)
{
  Ogre::ResourceManager * thisclass_cpp = static_cast<Ogre::ResourceManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::ResourceManager"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->getVerbose());
  *result_c = (long)result_cpp;
};

// Destroy a resource pool. 
extern "C" Ogre_LIB_EXPORT void ogre_rsrcmgr_destroyResourcePool2(struct hg3dclass_struct * thisclass_c, char * name_c)
{
  Ogre::ResourceManager * thisclass_cpp = static_cast<Ogre::ResourceManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::ResourceManager"));
  Ogre::String name_cpp = Ogre::String((const char*) name_c);
  (thisclass_cpp->destroyResourcePool(name_cpp));
};

// destroy all pools 
extern "C" Ogre_LIB_EXPORT void ogre_rsrcmgr_destroyAllResourcePools(struct hg3dclass_struct * thisclass_c)
{
  Ogre::ResourceManager * thisclass_cpp = static_cast<Ogre::ResourceManager*> (getHG3DClassPtr(*thisclass_c, "Ogre::ResourceManager"));
  (thisclass_cpp->destroyAllResourcePools());
};

