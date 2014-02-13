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

// ClassResource.cpp

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



// 
extern "C" Ogre_LIB_EXPORT void ogre_rsrc_destruct(struct hg3dclass_struct * thisclass_c)
{
  Ogre::Resource * thisclass_cpp = static_cast<Ogre::Resource*> (getHG3DClassPtr(*thisclass_c, "Ogre::Resource"));
  (delete thisclass_cpp);
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rsrc_prepare(struct hg3dclass_struct * thisclass_c, int backgroundThread_c)
{
  Ogre::Resource * thisclass_cpp = static_cast<Ogre::Resource*> (getHG3DClassPtr(*thisclass_c, "Ogre::Resource"));
  bool backgroundThread_cpp = (bool)backgroundThread_c;
  (thisclass_cpp->prepare(backgroundThread_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rsrc_load(struct hg3dclass_struct * thisclass_c, int backgroundThread_c)
{
  Ogre::Resource * thisclass_cpp = static_cast<Ogre::Resource*> (getHG3DClassPtr(*thisclass_c, "Ogre::Resource"));
  bool backgroundThread_cpp = (bool)backgroundThread_c;
  (thisclass_cpp->load(backgroundThread_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rsrc_reload(struct hg3dclass_struct * thisclass_c)
{
  Ogre::Resource * thisclass_cpp = static_cast<Ogre::Resource*> (getHG3DClassPtr(*thisclass_c, "Ogre::Resource"));
  (thisclass_cpp->reload());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rsrc_isReloadable(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::Resource * thisclass_cpp = static_cast<Ogre::Resource*> (getHG3DClassPtr(*thisclass_c, "Ogre::Resource"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isReloadable());
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rsrc_isManuallyLoaded(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::Resource * thisclass_cpp = static_cast<Ogre::Resource*> (getHG3DClassPtr(*thisclass_c, "Ogre::Resource"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isManuallyLoaded());
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rsrc_unload(struct hg3dclass_struct * thisclass_c)
{
  Ogre::Resource * thisclass_cpp = static_cast<Ogre::Resource*> (getHG3DClassPtr(*thisclass_c, "Ogre::Resource"));
  (thisclass_cpp->unload());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rsrc_getSize(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::Resource * thisclass_cpp = static_cast<Ogre::Resource*> (getHG3DClassPtr(*thisclass_c, "Ogre::Resource"));
  size_t result_cpp;
  result_cpp = (thisclass_cpp->getSize());
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rsrc_touch(struct hg3dclass_struct * thisclass_c)
{
  Ogre::Resource * thisclass_cpp = static_cast<Ogre::Resource*> (getHG3DClassPtr(*thisclass_c, "Ogre::Resource"));
  (thisclass_cpp->touch());
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rsrc_getName(struct hg3dclass_struct * thisclass_c, char * result_c)
{
  Ogre::Resource * thisclass_cpp = static_cast<Ogre::Resource*> (getHG3DClassPtr(*thisclass_c, "Ogre::Resource"));
  Ogre::String result_cpp;
  result_cpp = (thisclass_cpp->getName());
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rsrc_isPrepared(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::Resource * thisclass_cpp = static_cast<Ogre::Resource*> (getHG3DClassPtr(*thisclass_c, "Ogre::Resource"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isPrepared());
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rsrc_isLoaded(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::Resource * thisclass_cpp = static_cast<Ogre::Resource*> (getHG3DClassPtr(*thisclass_c, "Ogre::Resource"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isLoaded());
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rsrc_isLoading(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::Resource * thisclass_cpp = static_cast<Ogre::Resource*> (getHG3DClassPtr(*thisclass_c, "Ogre::Resource"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isLoading());
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rsrc_isBackgroundLoaded(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::Resource * thisclass_cpp = static_cast<Ogre::Resource*> (getHG3DClassPtr(*thisclass_c, "Ogre::Resource"));
  bool result_cpp;
  result_cpp = (thisclass_cpp->isBackgroundLoaded());
  *result_c = (int)result_cpp;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rsrc_setBackgroundLoaded(struct hg3dclass_struct * thisclass_c, int bl_c)
{
  Ogre::Resource * thisclass_cpp = static_cast<Ogre::Resource*> (getHG3DClassPtr(*thisclass_c, "Ogre::Resource"));
  bool bl_cpp = (bool)bl_c;
  (thisclass_cpp->setBackgroundLoaded(bl_cpp));
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rsrc_escalateLoading(struct hg3dclass_struct * thisclass_c)
{
  Ogre::Resource * thisclass_cpp = static_cast<Ogre::Resource*> (getHG3DClassPtr(*thisclass_c, "Ogre::Resource"));
  (thisclass_cpp->escalateLoading());
};

// Gets the group which this resource is a member of. 
extern "C" Ogre_LIB_EXPORT void ogre_rsrc_getGroup(struct hg3dclass_struct * thisclass_c, char * result_c)
{
  Ogre::Resource * thisclass_cpp = static_cast<Ogre::Resource*> (getHG3DClassPtr(*thisclass_c, "Ogre::Resource"));
  Ogre::String result_cpp;
  result_cpp = (thisclass_cpp->getGroup());
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rsrc_changeGroupOwnership(struct hg3dclass_struct * thisclass_c, char * newGroup_c)
{
  Ogre::Resource * thisclass_cpp = static_cast<Ogre::Resource*> (getHG3DClassPtr(*thisclass_c, "Ogre::Resource"));
  Ogre::String newGroup_cpp = Ogre::String((const char*) newGroup_c);
  (thisclass_cpp->changeGroupOwnership(newGroup_cpp));
};

// Gets the manager which created this resource. 
extern "C" Ogre_LIB_EXPORT void ogre_rsrc_getCreator(struct hg3dclass_struct * thisclass_c, struct hg3dclass_struct * result_c)
{
  Ogre::Resource * thisclass_cpp = static_cast<Ogre::Resource*> (getHG3DClassPtr(*thisclass_c, "Ogre::Resource"));
  Ogre::ResourceManager * result_cpp;
  result_cpp = (thisclass_cpp->getCreator());
  *result_c = getHG3DClass_ResourceManager((void *) result_cpp);
;
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rsrc_getOrigin(struct hg3dclass_struct * thisclass_c, char * result_c)
{
  Ogre::Resource * thisclass_cpp = static_cast<Ogre::Resource*> (getHG3DClassPtr(*thisclass_c, "Ogre::Resource"));
  Ogre::String result_cpp;
  result_cpp = (thisclass_cpp->getOrigin());
  if (strlen( (char *) result_cpp.c_str()) < (1024 * 64 - 1))  { 
    strcpy(result_c, (char *) result_cpp.c_str()); 
  } else {
    strcpy(result_c, "error: outstring larger then 64k");
  };
};

// 
extern "C" Ogre_LIB_EXPORT void ogre_rsrc_getStateCount(struct hg3dclass_struct * thisclass_c, int * result_c)
{
  Ogre::Resource * thisclass_cpp = static_cast<Ogre::Resource*> (getHG3DClassPtr(*thisclass_c, "Ogre::Resource"));
  size_t result_cpp;
  result_cpp = (thisclass_cpp->getStateCount());
  *result_c = (int)result_cpp;
};

